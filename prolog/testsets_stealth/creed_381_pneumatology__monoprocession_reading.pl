% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: Creed of 381 Inviolability Wall (Mono-Procession Reading)
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The mono-procession reading of the Creed of 381 operates as a wall-type
 *   commitment-system arrangement: it fixes the creedal text as received,
 *   routes every proposed alteration through consent of the whole Church, and
 *   brands unilateral amendment as breach of communion. The arrangement's
 *   principal beneficiary is the Eastern communion of autocephalous churches,
 *   whose decentralized polity is protected — no single see can legislate
 *   doctrine over them, and the consent requirement structurally favors their
 *   numbers in any future council. The bearer of its costs is the Western see
 *   that amended the procession clause and claims authority to clarify
 *   implicit doctrine, holding permanent breach status under the
 *   non-recognition regime. Epsilon's referent is the standing arrangement
 *   under contest — the inviolability and non-recognition regime as it has
 *   actually operated — assessed by this reading's own lights; it is not the
 *   reunion arrangement this reading opposes. The claim/metric gap is
 *   deliberate: the reading CLAIMS the arrangement as faithful guardianship
 *   of a common inheritance, while the authored metrics describe actively
 *   enforced, asymmetrically costly operation — the engine measures that
 *   divergence; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - eastern_autocephalous_churches: Primary beneficiary and enforcement administrator (institutional/identity_locked) — collects polity preservation and negotiating leverage, administers the non-recognition machinery through synodal encyclicals and communion restrictions
 *   - western_unilateral_innovators: Primary target (institutional/identity_locked) — carries breach status, bears non-recognition costs, locked into the magisterial identity the wall condemns
 *   - creed_reciting_laity: Diffuse beneficiary with indirect costs (moderate/constrained) — receives a fixed confession, pays for division in family and sacramental life
 *   - eastern_doctrinal_development_advocates: Excluded voice (moderate/constrained) — inside the churches but outside any procedure their proposals could ever win
 *   - ecumenical_dialogue_commissions: Analytical observer (analytical/analytical) — documents textual history and consent mechanics, holds no jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.68).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.72).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "Creed of 381 Inviolability Wall (Mono-Procession Reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, '4c7c7743-2a62-4842-826e-01236267c8ce').
narrative_ontology:cs_kernel_codification('4c7c7743-2a62-4842-826e-01236267c8ce', fixed_text).
narrative_ontology:cs_authority_grounding('4c7c7743-2a62-4842-826e-01236267c8ce', lineage).
narrative_ontology:cs_interpretation_layer_present('4c7c7743-2a62-4842-826e-01236267c8ce').
narrative_ontology:cs_reading_relation('4c7c7743-2a62-4842-826e-01236267c8ce', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('4c7c7743-2a62-4842-826e-01236267c8ce', creed_381_pneumatology__ecumenical_reunion_reading, forecloses).
narrative_ontology:cs_axiom('4c7c7743-2a62-4842-826e-01236267c8ce', foundational, spirit_proceeds_from_father_alone).
narrative_ontology:cs_axiom_status(spirit_proceeds_from_father_alone, holdable).
narrative_ontology:cs_axiom_grounding('4c7c7743-2a62-4842-826e-01236267c8ce', spirit_proceeds_from_father_alone, theological).
narrative_ontology:cs_axiom('4c7c7743-2a62-4842-826e-01236267c8ce', foundational, creed_amendment_requires_ecumenical_consent).
narrative_ontology:cs_axiom_status(creed_amendment_requires_ecumenical_consent, holdable).
narrative_ontology:cs_axiom_grounding('4c7c7743-2a62-4842-826e-01236267c8ce', creed_amendment_requires_ecumenical_consent, conventional).
narrative_ontology:cs_axiom('4c7c7743-2a62-4842-826e-01236267c8ce', secondary, unilateral_amendment_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_amendment_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('4c7c7743-2a62-4842-826e-01236267c8ce', unilateral_amendment_constitutes_breach, conventional).
narrative_ontology:cs_reference_frame('4c7c7743-2a62-4842-826e-01236267c8ce', conciliar_reception_of_381_text).
narrative_ontology:cs_drift_state('4c7c7743-2a62-4842-826e-01236267c8ce', contemporary_ecumenical_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4c7c7743-2a62-4842-826e-01236267c8ce', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, creed_reciting_laity).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, creed_reciting_laity).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, conciliar_consent_amendment_doctrine).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, monoprocession_trinitarianism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A communion of self-governing churches — the ancient patriarchates and the national churches — that confess the 381 text unamended and refuse recognition to any altered recension. They administer the non-recognition machinery through synodal encyclicals, commemoration breaks, and communion restrictions, and they collect the arrangement's principal returns: no single see can legislate doctrine over them, and the consent requirement weights any future council toward their numbers. Leaving the arrangement would mean conceding that the creed can be revised without them, which their self-understanding as the unchanged Church does not permit.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, agenda_setter).

% The Roman see and the Latin West, which added wording to the creed's procession clause and claim authority to clarify implicit doctrine. Under this arrangement they carry permanent breach status: their recension goes unrecognized, their sacramental standing is contested at the margins, and every dialogue round opens with the demand that they revert. Their way out — revoking the insertion or renouncing clarifying authority — would rupture a magisterial identity built on possessing exactly that authority.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators, payer,
    institutional, civilizational, identity_locked, global).

% Receive a fixed confession identical to what their grandparents recited, and the assurance that their church's faith cannot be altered overhead. Pay indirectly: the breach designation divides families, intermarriage, and sacramental sharing across the East-West line, and the frozen text leaves every question the creed never anticipated to improvised jurisdictional answers.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, creed_reciting_laity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, creed_reciting_laity, payer).

% Theologians and reformers inside the Eastern churches who argue the inviolability norm freezes legitimate development and hands each new question to ad hoc synodal fiat. The arrangement gives them no procedure by which their proposals could ever amend anything; they appear in its records only as objects of ruling, never as participants in framing it.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_doctrinal_development_advocates, excluded,
    moderate, biographical, constrained, regional).

% Joint theological commissions and academic historians who document the creed's textual history, the reception of the 381 council, and the mechanics of the consent requirement. They take testimony from every seat, publish findings neither communion controls, and hold no jurisdiction over the arrangement they study.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, ecumenical_dialogue_commissions, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__monoprocession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds all autocephalous churches to one creedal text and routes every proposed alteration through consent of the whole, so that no regional church wakes up under a confession someone else rewrote.
% TRANSFER_FUNCTION: Moves doctrinal authority from any single see to the ecumenical collective; moves the costs of breach — non-recognition, contested sacramental standing, perpetual negotiation — onto sees that amend unilaterally; preserves a structural veto weighted toward the numerically dominant Eastern communion.
% ABSENT_VOICES: Reunion-minded theologians on both sides, Western defenders of doctrinal development, and Eastern advocates of revision procedures would contest the inviolability premise itself; they appear in the arrangement's record only as subjects of its rulings, not as participants in framing it.
% DISAPPEARANCE_RATIONALE: If the inviolability norm vanished overnight, regional amendments would multiply, the East's veto over creedal change would evaporate, the West's breach status would lose its juridical basis, and communion boundaries would reorganize around whichever texts each church adopted.
% FOUNDING_PROBLEM: The Council of 381 issued the creed to settle the Pneumatomachian denial of the Spirit's divinity and to complete Nicaea's confession; the inviolability norm grew up to protect that conciliar settlement from later unilateral revision by any single see.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of doctrine corroborate the original problem and its resolution: the Spirit's divinity is uncontested on every side, and the West's own liturgy retains the creed's anti-Pneumatomachian core. Attestation that the problem remains live — that unilateral doctrinal legislation still requires this exact wall — comes almost entirely from the Eastern churches themselves; no source outside the beneficiary set independently certifies the live status.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__monoprocession_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__monoprocession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__monoprocession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__monoprocession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__monoprocession_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the arrangement converts the West's standing insertion into permanent breach liability, demands doctrinal conformity from every church, and concentrates amendment power in a consent structure favoring the East's numbers — tempered by the genuine coordination good of one shared confession. Suppression (0.72) reflects that persistence depends on active non-recognition, communion restriction, and refusal of the amended text, not on participant preference. Theater (0.28) is moderate-low: the text genuinely governs communion relations, though a growing share of activity — joint statements, anniversary condemnations, dialogue documents with no jurisdictional effect — is performative. Accessibility collapse (0.50) is middling: rival amendment theories collapse inside the reading's own frame, yet the sibling readings remain live positions held by other parties. Resistance (0.60) records a millennium of Western non-compliance and continuing dialogue friction. All three series run on one shared time grid (points 0-30, roughly 1870-2025 at ~5 years/unit). The trajectories are cyclical, not monotonic: hardening after Vatican I (T0-T6), thaw through the dialogue era culminating in the lifting of anathemas near T18, accumulation of unresolved grievances, and re-hardening after T24 (Uniatism disputes, autocephaly crises, stalled dialogues). The oscillation is partly an extraction mechanism in itself — intermittent reinforcement: each thaw raises convergence expectations that the subsequent re-hardening converts into renewed conformity commitments. Base properties are measured at T30, the re-hardened phase.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary-administrator seat the wall is fidelity: the creed as received, defended against revision overhead. From the payer seat the same structure is an exclusion regime premised on an ecclesiology — consent-only amendment — that its holder rejects as itself illegitimate. The two principal seats are nominally matched (both institutional, global, civilizational-horizon, identity_locked), yet they diverge completely on what exit even means: for the East, exit means conceding that the creed is revisable without them; for the West, exit means conceding a millennium of error. Identity lock is the load-bearing asymmetry — both seats are locked, but into opposite premises, so the same wall computes as guardianship in one seat and as breach-enforcement in the other. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: eastern_autocephalous_churches sit near the subsidized end (they collect polity preservation, veto position, and dialogue leverage at near-zero cost to themselves), and creed_reciting_laity sit low-to-intermediate (genuine benefit from a stable confession, diffuse indirect costs from division). western_unilateral_innovators sit near the full-target end: they bear the breach designation's costs, face identity-locked exit, and the arrangement's enforcement exists precisely to hold them in that position. The excluded development advocates feed no derivation — their absence is commentary-grade signal, not correction-grade. No directionality overrides are needed: the beneficiary/victim declarations plus exit options produce the correct directionalities without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — consolidating the Spirit's divinity against the Pneumatomachians and completing Nicaea's confession — is dead, and the corroboration record shows it: no party on any side disputes the Spirit's divinity, and the West's own liturgy uses the creed's anti-Pneumatomachian core. Yet the arrangement persists with a transformed function: polity defense against unilateral doctrinal legislation. The tangled_rope classification prevents mislabeling in both directions. Pure-extraction labeling would erase the real coordination function — a common confession across autonomous churches genuinely requires consent-routed amendment, or it fragments into locally rewritten creeds. Pure-coordination labeling would erase the asymmetric breach costs and the positional veto the consent structure confers. The R5 fields record the genealogy honestly: founding_problem_status is contested (the East attests the unilateralism threat is live; no source outside the beneficiary set independently certifies it), and the contested-status x world_rearranges combination flags the live dispute over obsolescence without asserting zombie capture — the arrangement is actively maintained with real stakes, not theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates only the monoprocession_reading of kernel creed_381_pneumatology; what structural elements would the sibling readings change?',
    'Compare against the compiled sibling stories: the filioque_reading relocates amendment authority to a magisterium (inverting the victim set); the ecumenical_reunion_reading replaces breach designation with bilateral recognition (dissolving the wall entirely).',
    'Under a sibling reading the beneficiary/victim sets invert or dissolve, epsilon moves off the wall-defense referent, and the classification recomputes — likely rope under the reunion reading, a differently shaped tangled_rope under the filioque reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    consent_norm_symmetry,
    'Does the ecumenical-consent requirement bind all sees symmetrically, or does it entrench a structural veto weighted toward the numerically dominant Eastern communion?',
    'Model amendment scenarios under the norm: enumerate the sees whose consent is required, their distribution, and whether any single see or bloc can block or force passage.',
    'Symmetric operation supports the coordination-side reading of the arrangement; asymmetric operation confirms the extraction component and the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_norm_symmetry, empirical, 'Whether the consent mechanism is neutral procedure or positional advantage.').

omega_variable(
    breach_cost_incidence,
    'Who materially bears the costs of the breach designation — the West alone, or both communions through lost unity, restricted intercommunion, and divided diaspora jurisdictions?',
    'Trace concrete cost flows: sacramental-access disputes, jurisdictional conflicts over diaspora parishes, and the bargaining leverage the breach status confers in every dialogue round.',
    'If costs fall substantially on both sides the victim declaration narrows and the arrangement looks less extractive; if the West bears them disproportionately while the East collects negotiating leverage, the asymmetry is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breach_cost_incidence, empirical, 'Incidence of the breach designation''s costs across the two communions.').

omega_variable(
    inviolability_grounding,
    'Is the creed''s inviolability a structural feature of conciliar reception itself — the price any shared text pays for coherence — or a constructed polity defense erected by one communion to protect its position?',
    'Comparative ecclesiology: examine whether other consent-governed confessional texts show the same enforcement profile, and whether the inviolability norm predates the East-West rupture or was articulated in response to it.',
    'If inviolability is intrinsic to shared-text governance, part of the measured extraction is the irreducible cost of coordination; if it is positional, the arrangement sits closer to pure wall-building.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inviolability_grounding, conceptual, 'Natural-feature versus constructed-defense ambiguity of the inviolability norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t0, creed_381_pneumatology__monoprocession_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cree_tr_t6, creed_381_pneumatology__monoprocession_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(cree_tr_t12, creed_381_pneumatology__monoprocession_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(cree_tr_t18, creed_381_pneumatology__monoprocession_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement(cree_tr_t24, creed_381_pneumatology__monoprocession_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(cree_tr_t30, creed_381_pneumatology__monoprocession_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(cree_be_t0, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cree_be_t6, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(cree_be_t12, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(cree_be_t18, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(cree_be_t24, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(cree_be_t30, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t0, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(cree_su_t6, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(cree_su_t12, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(cree_su_t18, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(cree_su_t24, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(cree_su_t30, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Filioque dispute' decomposes into three structurally distinct constraints — one per reading of kernel creed_381_pneumatology — because each reading fixes a different epsilon referent, a different beneficiary/victim structure, and a different enforcement surface. This story authors the wall-type reading: high epsilon against the standing non-recognition regime, Eastern beneficiaries, Western breach-bearers. Structural kinship: the filioque_reading's magisterial premise is the practice this wall declares breach (downstream of this reading's condemnation); the ecumenical_reunion_reading is the dissolution proposal both wall-type readings resist. All three files cross-link via affects_constraints so contamination and purity analysis can trace the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
