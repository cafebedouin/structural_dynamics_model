% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Settlement Legitimacy — Composite Overdetermination Reading
 *   domain: historical/legal/cultural
 *
 * SUMMARY:
 *   By the late nineteenth century in Western Europe and much of North
 *   America, dueling had gone from an accepted (if regulated) mechanism of
 *   elite dispute resolution to a criminal, socially disreputable, and
 *   increasingly rare practice. The composite reading holds that no single
 *   cause explains this: courts criminalized it, insurers voided policies for
 *   duel deaths, professional bodies delicensed participants, military
 *   discipline systems punished it, and the broader culture came to find the
 *   practice unintelligible as a rational response to insult. Each mechanism
 *   alone might have been survivable by the honor culture; together they were
 *   not.
 *
 * KEY AGENTS:
 *   - residual_honor_claimants: bear the compounding weight of legal, material, and cultural pressures simultaneously
 *   - state_legal_monopolists: administer the legal lever among several independent levers
 *   - insurance_and_professional_bodies: independently impose material penalties without coordinating with the state
 *   - bourgeois_respectability_class: cultural beneficiaries of the shift, without having engineered it
 *   - military_officer_corps_traditionalists: last holdouts, squeezed from within their own institution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.28).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.42).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Settlement Legitimacy — Composite Overdetermination Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical/legal/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '4cee07cc-8c5f-43d0-ad21-631ba370b35c').
narrative_ontology:cs_kernel_codification('4cee07cc-8c5f-43d0-ad21-631ba370b35c', distributed).
narrative_ontology:cs_authority_grounding('4cee07cc-8c5f-43d0-ad21-631ba370b35c', distributed).
narrative_ontology:cs_reading_relation('4cee07cc-8c5f-43d0-ad21-631ba370b35c', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('4cee07cc-8c5f-43d0-ad21-631ba370b35c', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('4cee07cc-8c5f-43d0-ad21-631ba370b35c', foundational, decline_is_causally_overdetermined).
narrative_ontology:cs_axiom_status(decline_is_causally_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('4cee07cc-8c5f-43d0-ad21-631ba370b35c', decline_is_causally_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('4cee07cc-8c5f-43d0-ad21-631ba370b35c', secondary, cultural_contraction_is_dominant_but_not_exclusive_cause).
narrative_ontology:cs_axiom_status(cultural_contraction_is_dominant_but_not_exclusive_cause, holdable).
narrative_ontology:cs_axiom_grounding('4cee07cc-8c5f-43d0-ad21-631ba370b35c', cultural_contraction_is_dominant_but_not_exclusive_cause, empirically_contingent).
narrative_ontology:cs_reference_frame('4cee07cc-8c5f-43d0-ad21-631ba370b35c', elite_honor_code_as_legitimate_settlement_mechanism).
narrative_ontology:cs_drift_state('4cee07cc-8c5f-43d0-ad21-631ba370b35c', post_professionalization_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4cee07cc-8c5f-43d0-ad21-631ba370b35c', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_monopolists).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, insurance_and_professional_bodies).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_respectability_class).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, residual_honor_claimants).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, military_officer_corps_traditionalists).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, rational_dispute_resolution_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic and professional men who still felt insults required a personal-violence answer, now find the duel simultaneously criminalized, socially ridiculed, materially costly (loss of career, insurance, legal exposure), and increasingly unintelligible as a way of settling anything. They bear the cost of a fading practice they cannot cleanly abandon or cleanly perform.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, residual_honor_claimants, payer,
    moderate, biographical, constrained, national).

% Officer honor codes formally retained dueling obligations longer than civil society, but promotion boards, courts-martial exposure, and the officer corps's own professionalization reforms squeezed the practice from multiple directions at once. Their voice in the broader legitimacy debate is marginal by the period this constraint tracks.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_officer_corps_traditionalists, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, military_officer_corps_traditionalists, excluded).

% Courts, legislatures, and prosecutors that criminalize dueling, expand civil libel remedies, and build the alternative dispute infrastructure (courts of honor, then ordinary courts) that renders private violence redundant. They administer several independent levers — criminal law, tort law, licensing — any one of which would have pressured dueling downward.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_monopolists, agenda_setter,
    institutional, generational, arbitrage, national).

% Life insurers refusing payout on duel deaths, medical and legal professional associations imposing disbarment/delicensing for participation, and employers dismissing duelists add independent material penalties. None of these actors coordinated with each other or with the state; each pursued its own interest and the penalties stacked.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, insurance_and_professional_bodies, beneficiary,
    organized, generational, arbitrage, national).

% The rising commercial and professional middle class treats dueling as aristocratic barbarism and markets its own self-control and litigiousness as the modern, respectable alternative. Their cultural ascendancy both reflects and accelerates the unthinkability shift, and they benefit reputationally from dueling's disappearance without having designed the outcome.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_respectability_class, beneficiary,
    organized, generational, mobile, national).

% Historical sociologists reconstructing the decline from court records, insurance archives, newspaper commentary, and military discipline records, arguing over whether any single mechanism was sufficient or whether the decline required the convergence of several.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, historians_of_honor_culture, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No single coordinating body existed; rather, several independently-operating institutions (courts, insurers, professional licensing bodies, military discipline systems, and shifting elite manners) each separately reduced dueling's viability, and their pressures overlapped in time and reinforced one another.
% TRANSFER_FUNCTION: Legitimacy and status previously settled through personal combat migrate to courts, professional bodies, and reputational markets; material costs shift from risk-of-death/legal jeopardy for duelists onto insurers and licensing bodies who absorb the administrative burden of adjudicating honor disputes through non-violent channels.
% ABSENT_VOICES: The duelists' own class fragmented and eventually could not articulate a unified defense — no organized lobby for dueling persisted into the terminal period, so the practice had no institutional voice defending it against the converging pressures; military traditionalists retained partial voice but were marginalized within their own institution.
% DISAPPEARANCE_RATIONALE: If any single mechanism (say, criminal prosecution alone) had disappeared, historians disagree whether dueling would have persisted via the remaining channels (insurance penalties, professional exclusion, cultural ridicule) or revived — this is precisely the overdetermination question the composite reading exists to assert: multiple independently sufficient-or-nearly-sufficient pressures converged, so removing any one still leaves the others suppressing the practice.
% FOUNDING_PROBLEM: Honor disputes among gentlemen required a legitimate, socially recognized mechanism for settling insults to reputation that avoided both endless private vendetta and the indignity of relying on courts perceived as beneath a gentleman's status.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside any dueling-adjacent institution (court record archivists, insurance-industry historians) corroborate that by the late nineteenth century the reputational-settlement function had been fully absorbed by libel law, professional codes of conduct, and press mechanisms, with no surviving institutional constituency arguing the original problem still required combat as its answer.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 by 1910) because this reading does not center on a single extractive institution collecting rents from the practice's suppression — the beneficiaries (insurers, professional bodies, the respectability class) gain diffusely and incidentally rather than through a coordinated extraction scheme. Suppression is moderate (0.42) and reflects the additive but not centrally coercive character of the several independent pressures — no single enforcer needed to apply maximal force because the mechanisms overlapped. Theater ratio rises substantially across the interval (0.10 to 0.55) because as the underlying practice hollows out, the residual formal codes of honor (courts of honor, ritualized apologies, published cartels) persist as performance long after the functional need for combat-based settlement has receded — this is a genuinely piton-like signature: an atrophied function maintained by increasingly hollow ceremony while several structural currents work against it simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   From the state's agenda-setting seat, this looks like successful policy convergence: law, market discipline, and culture all pointed the same direction, vindicating the state's monopoly claim on legitimate violence. From the residual claimant's seat, this looks like an inescapable pincer: whichever pressure they might have weathered alone, the combination left no viable path to continued practice. The engine should register this asymmetry as a structural fact about overdetermination, not as an error in one seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   State legal monopolists and the professional/insurance bodies sit near the beneficiary end: they either collect the vindication of their preferred dispute-resolution monopoly or avoid payout/liability risk, with strong exit options (arbitrage — they can redirect their institutional energies elsewhere without loss). The respectability class benefits reputationally with mobile exit (their status is not staked on the outcome). The residual claimants and military traditionalists sit toward the target end: constrained exit, bearing the compounding cost of several simultaneous pressures they did not choose and could not fully resist, though their power is not negligible (moderate/organized) — this is not a powerless-victim story, which differentiates it structurally from a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a legitimate mechanism for settling reputational insult outside vendetta or court) is genuinely dead by the end of the interval — libel law, press mechanisms, and professional codes fully absorbed the function. This is not a case of an extractive arrangement persisting past its function for someone's benefit (which would push toward snare or piton-with-capturer); rather, the composite reading documents an arrangement's LEGITIMACY collapsing under multiple simultaneously-sufficient pressures with no single capturer benefiting from its persistence. The theatrical residue (rising theater_ratio) is the piton-like signature of ceremonial honor codes outliving their function, but no concentrated beneficiary profits from maintaining that theater — it fades because it fades, not because someone extracts from its continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_vs_necessity_of_contraction,
    'Is cultural contraction (the unthinkability shift) independently sufficient to explain dueling''s decline, or does it require the material/institutional pressures as necessary co-causes, as this composite reading asserts?',
    'Comparative case analysis: identify sub-populations or regions where cultural contraction proceeded without matching legal/material pressure (or vice versa) and observe whether dueling persisted or declined at similar rates. Divergent decline rates would support overdetermination; convergent decline rates regardless of which pressures were present would support the contraction_reading''s stronger claim.',
    'If contraction alone is sufficient, this composite reading collapses into the contraction_reading and the material/institutional mechanisms become epiphenomenal rather than causally load-bearing — the classification would shift toward treating this as a redundant restatement rather than a distinct structural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_vs_necessity_of_contraction, empirical, 'Whether cultural contraction alone explains the decline or requires the co-occurring material and institutional pressures this reading credits as independently reinforcing.').

omega_variable(
    residual_practice_scope,
    'Does the documented decline this reading describes actually reach zero practice, or does a residual fringe persist among a subpopulation that the composite reading''s national/generational scope obscures, as the drop_reading holds?',
    'Fine-grained archival search for dueling incidents past the period this story treats as terminal, disaggregated by region, class, and institutional affiliation (particularly military officer corps records, which retain the practice longest).',
    'If a persistent residual practice is documented, the composite reading''s claim of overdetermined decline needs qualification — the mechanisms may have been sufficient to marginalize but not eliminate the practice, which would mean this story''s disappearance_verdict of ''contested'' understates the persistence the drop_reading identifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_practice_scope, empirical, 'Whether the decline this composite reading documents is total or leaves an unaccounted residual practice.').

omega_variable(
    beneficiary_diffuseness_ambiguity,
    'Are the declared beneficiaries (state monopolists, insurers, respectability class) genuinely uncoordinated independent actors, or did they in fact coordinate (formally or informally) to jointly suppress an elite practice that threatened their respective interests?',
    'Archival correlation of timing and correspondence between state legislative action, insurance industry lobbying, and professional body rule-changes — evidence of cross-institutional coordination (shared personnel, joint lobbying, cited precedent) would indicate a more coordinated extraction structure than this reading currently credits.',
    'If coordination is found, the classification would shift from piton (diffuse, no concentrated capturer) toward tangled_rope (coordinated beneficiaries plus identifiable victims plus active enforcement) — this matters for whether the gain_flow should be treated as diffuse or attributed to a coordinating bloc.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_diffuseness_ambiguity, conceptual, 'Whether the multiple beneficiary groups this reading treats as independent were in fact coordinated, which would change the classification from diffuse piton toward coordinated tangled_rope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1770, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1770, honor_settlement_legitimacy__composite_reading, theater_ratio, 1770, 0.1).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(hono_tr_t1830, honor_settlement_legitimacy__composite_reading, theater_ratio, 1830, 0.28).
narrative_ontology:measurement(hono_tr_t1860, honor_settlement_legitimacy__composite_reading, theater_ratio, 1860, 0.4).
narrative_ontology:measurement(hono_tr_t1885, honor_settlement_legitimacy__composite_reading, theater_ratio, 1885, 0.5).
narrative_ontology:measurement(hono_tr_t1910, honor_settlement_legitimacy__composite_reading, theater_ratio, 1910, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t1770, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1770, 0.18).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(hono_be_t1830, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1830, 0.24).
narrative_ontology:measurement(hono_be_t1860, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1860, 0.26).
narrative_ontology:measurement(hono_be_t1885, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1885, 0.27).
narrative_ontology:measurement(hono_be_t1910, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1910, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1770, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1770, 0.2).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(hono_su_t1830, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1830, 0.38).
narrative_ontology:measurement(hono_su_t1860, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1860, 0.42).
narrative_ontology:measurement(hono_su_t1885, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1885, 0.42).
narrative_ontology:measurement(hono_su_t1910, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1910, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% This constraint is the composite_reading member of the honor_settlement_legitimacy kernel family (3 readings: composite, contraction, drop). Each reading shares the same historical kernel — the legitimacy status of dueling as a dispute-resolution mechanism — but instantiates a structurally distinct causal claim about its decline. contraction_reading treats cultural-framework transformation as the sole/dominant sufficient cause; drop_reading treats the practice as persisting at the margins rather than genuinely disappearing; this composite_reading treats the decline as caused by multiple independently-reinforcing mechanisms (legal, material, institutional, cultural) with contraction as the dominant but not exclusive thread. All three should be read together as competing structural accounts of the same underlying kernel, not as three measurements of one constraint — hence separate files, separate ε, linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
