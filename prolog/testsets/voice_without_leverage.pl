% ============================================================================
% CONSTRAINT STORY: voice_without_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voice_without_leverage, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: voice_without_leverage
 *   human_readable: Consultative Voice Without Structural Leverage
 *   domain: political/institutional
 *
 * SUMMARY:
 *   For fifteen years, a local authority holder has participated in a
 *   deliberative process with a central authority, receiving frequent small
 *   rhetorical concessions — softened language, delayed deadlines, minor
 *   carve-outs. These concessions have been read by the local authority
 *   holder as evidence of genuine, growing bargaining power within the
 *   relationship. The structural reality is different: the central authority
 *   retains sole, unappealable power to revoke the local authority holder's
 *   entire operating standing, and exercises this power — when it chooses to
 *   — through an ordinary administrative order delivered in the same flat,
 *   even register used for routine notices. The affective consistency of the
 *   authority's communication (no register shift between granting a
 *   concession and revoking standing) is itself a key observable: it signals
 *   that the concessions were never bargaining outcomes but were merely
 *   costless courtesies, and the revocation was never an escalation but
 *   simply the same undifferentiated administrative capacity being exercised
 *   on a different subject.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voice_without_leverage, 0.71).
domain_priors:suppression_score(voice_without_leverage, 0.79).
domain_priors:theater_ratio(voice_without_leverage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voice_without_leverage, extractiveness, 0.71).
narrative_ontology:constraint_metric(voice_without_leverage, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(voice_without_leverage, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(voice_without_leverage, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(voice_without_leverage, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voice_without_leverage, tangled_rope).
narrative_ontology:human_readable(voice_without_leverage, "Consultative Voice Without Structural Leverage").
narrative_ontology:topic_domain(voice_without_leverage, "political/institutional").

domain_priors:requires_active_enforcement(voice_without_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voice_without_leverage, central_authority).
narrative_ontology:constraint_victim(voice_without_leverage, local_authority_holder).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(voice_without_leverage, junior_regional_offices).
narrative_ontology:constraint_vindicates(voice_without_leverage, procedural_inclusion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chairs the deliberative body, sets the agenda for every session, and grants rhetorical concessions to local authority holders on a regular basis — softening language, acknowledging objections, delaying minor implementation details. Retains sole, unappealable power to revoke a local authority holder's entire operating standing (charter, funding, jurisdiction) through an ordinary administrative order, issued in the same flat register as a routine notice. The concessions cost it nothing structural; the revocation power is never spent on trivial matters, which is precisely what keeps it credible and rarely visible.
narrative_ontology:constraint_stakeholder(voice_without_leverage, central_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(voice_without_leverage, central_authority, beneficiary).

% Has sat in deliberation with the central authority for fifteen years, winning small, real-sounding concessions — wording changes, timeline extensions, minor carve-outs — and has come to read this pattern as evidence of genuine bargaining power. Has no independent charter, funding source, or appeal mechanism outside the central authority's own process. When a substantive conflict arises, discovers the fifteen years of concessions bought no leverage over that decision: the authority simply issues a revocation order administratively, in the identical bureaucratic tone used for scheduling notices. Cannot exit the relationship without losing operating standing entirely.
narrative_ontology:constraint_stakeholder(voice_without_leverage, local_authority_holder, payer,
    moderate, biographical, trapped, regional).

% Watch the local authority holder's fifteen-year negotiation record as a template for their own dealings with the central authority. Draw false confidence from the visible concessions, unaware that the revocation power sits one level up and has never been tested against a serious conflict. Bear indirect cost when the local authority holder's eventual revocation disrupts services they depend on.
narrative_ontology:constraint_stakeholder(voice_without_leverage, junior_regional_offices, observer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(voice_without_leverage, junior_regional_offices, payer).

% An external audit or legislative oversight body that could, in principle, compare concession-to-revocation ratios and register-consistency data to detect the asymmetry, but is not a standing party to the deliberative process and is not consulted before revocation orders are issued. Their absence from the room is what allows the concession pattern to be read as the whole relationship.
narrative_ontology:constraint_stakeholder(voice_without_leverage, oversight_reviewers, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(voice_without_leverage, central_authority).
narrative_ontology:fixing_cost_class(voice_without_leverage, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The deliberative body genuinely resolves day-to-day administrative friction — wording disputes, scheduling, minor carve-outs — without escalating every disagreement to a formal legal challenge, which is a real coordination saving for both sides.
% TRANSFER_FUNCTION: Moves the appearance of shared authority from the central authority to the local authority holder (in the form of rhetorical concessions) while moving actual operating standing — charter continuity, funding, jurisdiction — unilaterally in the opposite direction whenever the central authority chooses to exercise it.
% ABSENT_VOICES: External oversight reviewers who could audit the concession/revocation ratio and the register-consistency signal are not part of the deliberative process and are never consulted before a revocation order issues; their absence lets the pattern of routine concessions stand unchallenged as evidence of balanced power.
% DISAPPEARANCE_RATIONALE: The central authority would say nothing changes structurally if the concession theater ended — its unilateral revocation power is unaffected either way. The local authority holder would say the entire basis of the fifteen-year relationship — the belief that dialogue produces real bargaining outcomes — would collapse, forcing an open confrontation over the previously obscured power asymmetry.
% FOUNDING_PROBLEM: The deliberative body was established to give local authority holders a formal channel to raise objections and negotiate implementation details, avoiding the cost and delay of adversarial legal proceedings for routine administrative matters.
% FOUNDING_PROBLEM_CORROBORATION: The central authority attests the consultative function remains live and cites the volume of concessions granted as proof. The local authority holder, after experiencing an uncontested revocation, attests the consultative channel has become theater masking an unappealable unilateral power; no external oversight body has yet corroborated either account, since none sits inside the process.
narrative_ontology:disappearance_verdict(voice_without_leverage, contested).
narrative_ontology:founding_problem_status(voice_without_leverage, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(voice_without_leverage, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(voice_without_leverage, 'none', 1).
narrative_ontology:epsilon_provenance(voice_without_leverage, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voice_without_leverage_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(voice_without_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(voice_without_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.71) because each successive concession further embeds the local authority holder's operational dependence on the relationship, while the underlying unilateral revocation capacity is never diminished by the concessions granted against it. Theater ratio climbs in parallel (0.30 to 0.58) as more of the deliberative activity comes to consist of concessions that cost the central authority nothing structurally while accumulating psychological investment on the other side. Suppression is high and rising (0.55 to 0.79) — not because deliberation itself is coercive, but because the local authority holder's only channel for grievance is the same body that holds unappealable revocation power, closing off any external recourse. Accessibility collapse (0.62) reflects that once the local authority holder understands the structure, there is no alternative venue — its charter itself depends on the central authority's continued forbearance.
 *
 * PERSPECTIVAL GAP:
 *   From the central authority's seat, the arrangement is straightforward administration: it grants concessions because they are low-cost and maintains oversight because that is its mandate — no contradiction is felt because the concession and the revocation are treated as the same category of act. From the local authority holder's seat, the same structure reads first as partnership (during the concession years) and then, retroactively, as betrayal (at the moment of revocation) — but structurally nothing changed between those two moments; only the object of the authority's undifferentiated administrative capacity changed. This is the core of the tangled rope: real coordination (routine matters are resolved without escalation) coexists with, and is used to obscure, an asymmetric extraction (operating standing itself is never actually on the table).
 *
 * DIRECTIONALITY LOGIC:
 *   The central authority is the clear structural beneficiary: it bears essentially no cost from granting concessions (they are cheap, cosmetic, reversible) and retains full, uncontested capacity to extract the thing that actually matters — operating standing — at will. The local authority holder is the target: trapped exit options (loss of charter means loss of function entirely), moderate power that has never been tested against the central authority's actual unilateral capacity, and a biographical time horizon that makes the fifteen-year pattern feel like an earned, durable relationship rather than a standing asymmetry that was never up for negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this purely as coordination (a rope) by insisting on the coexistence of the genuine coordination function — deliberation does resolve real, low-stakes friction — with the asymmetric extraction that only becomes visible when a substantive conflict arises. It equally prevents mislabeling this as pure extraction (a snare) by acknowledging that fifteen years of concessions were not fabricated: they were real, if trivial, exercises of the same administrative capacity that later revoked standing. The tangled rope classification captures that the coordination function is not fake, but it is not what is actually at stake in the relationship's structural asymmetry — the revocation power was never coordinated away, merely never exercised until it was.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concession_revocation_ratio_threshold,
    'Is there a measurable ratio of concessions-granted to unilateral-enforcement-actions that reliably predicts whether an authority''s consultative process masks an unappealable revocation power, versus genuinely constrains it?',
    'Comparative institutional analysis across multiple central-local authority pairs, tracking concession frequency, concession magnitude, revocation frequency, and revocation magnitude over multi-decade intervals; test whether high concession-to-revocation ratios correlate with subsequent uncontested revocations.',
    'If a reliable threshold exists, oversight bodies could flag consultative relationships approaching it before a revocation occurs, converting this from an ex-post to an ex-ante detection mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_revocation_ratio_threshold, empirical, 'Whether concession/revocation ratios can predictively signal masked unilateral power.').

omega_variable(
    register_consistency_as_signal,
    'Does the authority''s use of an identical administrative register for both concessions and revocations reliably indicate that concessions carry no real structural cost — or could register consistency simply reflect institutional communication style unrelated to the underlying power balance?',
    'Comparative discourse analysis of authority communications across cases with known differing power asymmetries; check whether register shifts (or their absence) correlate with independently verified degrees of local authority holder leverage.',
    'If register consistency is a reliable tell, it becomes a low-cost diagnostic for external observers; if not, the observable is a red herring and detection must rely on the concession/revocation ratio alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(register_consistency_as_signal, empirical, 'Whether affective/register flatness across concession and revocation acts is diagnostic of masked power asymmetry.').

omega_variable(
    genuine_vs_cosmetic_concession_boundary,
    'At what point does a concession stop being purely cosmetic and start constituting a real transfer of bargaining leverage to the local authority holder?',
    'Track whether any granted concession has ever been invoked successfully by the local authority holder to block or delay a subsequent revocation-adjacent action; a concession that has never once constrained the central authority''s later unilateral conduct is evidence it was purely cosmetic.',
    'If no concession in the historical record has ever constrained a later unilateral action, the coordination function is weaker than it appears and the classification should weight more heavily toward extraction; if some concessions have demonstrably bound the central authority, the coordination function is more substantial than the narrative assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_cosmetic_concession_boundary, conceptual, 'Whether any granted concession has ever functioned as genuine leverage rather than pure theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voice_without_leverage, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(voic_tr_t0, voice_without_leverage, theater_ratio, 0, 0.3).
narrative_ontology:measurement(voic_tr_t3, voice_without_leverage, theater_ratio, 3, 0.36).
narrative_ontology:measurement(voic_tr_t6, voice_without_leverage, theater_ratio, 6, 0.43).
narrative_ontology:measurement(voic_tr_t9, voice_without_leverage, theater_ratio, 9, 0.49).
narrative_ontology:measurement(voic_tr_t12, voice_without_leverage, theater_ratio, 12, 0.54).
narrative_ontology:measurement(voic_tr_t15, voice_without_leverage, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(voic_be_t0, voice_without_leverage, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(voic_be_t3, voice_without_leverage, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(voic_be_t6, voice_without_leverage, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(voic_be_t9, voice_without_leverage, base_extractiveness, 9, 0.61).
narrative_ontology:measurement(voic_be_t12, voice_without_leverage, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(voic_be_t15, voice_without_leverage, base_extractiveness, 15, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(voic_su_t0, voice_without_leverage, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(voic_su_t3, voice_without_leverage, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(voic_su_t6, voice_without_leverage, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(voic_su_t9, voice_without_leverage, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(voic_su_t12, voice_without_leverage, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(voic_su_t15, voice_without_leverage, suppression_requirement, 15, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voice_without_leverage, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This story isolates the deliberative-concession dynamic as its own constraint, distinct from any sibling story that might address the formal legal/appeals structure governing revocation itself (a separate constraint with its own ε, since the appeals structure's extractiveness would be measured by different observables — availability and success rate of appeal, not concession/revocation ratio).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
