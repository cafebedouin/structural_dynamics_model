% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession (Negotiated-Amendment-Only Exit)
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   The standing arrangement under contest: a continental federation's
 *   constitutional rule that unilateral secession is impermissible and that
 *   legitimate exit runs only through a negotiated constitutional amendment.
 *   This file instantiates ONE reading of kernel
 *   secession_legitimacy_boundary — the constitutional_impossibility_reading
 *   — so ε's referent is the standing arrangement itself, assessed by this
 *   reading's own lights: the reading holds the federal-extraction claim
 *   invalid, and therefore authors low ε over a fixed referent. The sibling
 *   readings (popular_sovereignty_reading, grievance_threshold_reading,
 *   treaty_primacy_reading) are separate constraint files over the SAME
 *   arrangement and will author high ε and declared victim sets; the
 *   cross-reading divergence is the family's designed data, not an
 *   inconsistency. The claim/metrics are authored independently: claimed_type
 *   rope is this reading's structural belief about the arrangement (genuine
 *   perimeter coordination with a real exit channel, all declared groups net
 *   beneficiaries); the metrics describe the operation as it actually runs —
 *   actively enforced, recurrently resisted, with enforcement intensity that
 *   decayed from military consolidation to judicial routine and then climbed
 *   again as referendum-era mobilization resumed.
 *
 * KEY AGENTS:
 *   - federal_government: agenda-setter seat (institutional power, arbitrage exit) — holds and enforces the prohibition, controls amendment initiation, receives the concentrated exit-decision authority
 *   - constitutional_courts: enforcement organ (institutional power, identity_locked exit) — adjudicates the boundary; its warrant is fused with the frame it enforces
 *   - secessionist_province_majority: cost-bearing seat (organized power, constrained exit) — barred from unilateral exit; its lawful channel runs through the counterparty's consent
 *   - resource_rich_province_governments: dual seat (powerful, constrained exit) — net union beneficiary on market and security flows, net contributor on fiscal flows; cannot unilaterally capture its resource base
 *   - smaller_constituent_provinces: protected beneficiary seat (moderate power, generational horizon) — perimeter stability is their primary good
 *   - provincial_internal_minorities: protected beneficiary seat (moderate power, mobile exit) — shielded from a provincial-majority exit they oppose
 *   - indigenous_treaty_holders: excluded seat (organized power, trapped exit) — consent claim predates the frame and holds no structural vote in the exit channel
 *   - international_state_system: observer seat (institutional power, analytical exit) — recognition practice buttresses the rule from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.22).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.58).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession (Negotiated-Amendment-Only Exit)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '2a52dc20-06d6-4c56-b810-4ed174db9909').
narrative_ontology:cs_kernel_codification('2a52dc20-06d6-4c56-b810-4ed174db9909', fixed_text).
narrative_ontology:cs_authority_grounding('2a52dc20-06d6-4c56-b810-4ed174db9909', lineage).
narrative_ontology:cs_interpretation_layer_present('2a52dc20-06d6-4c56-b810-4ed174db9909').
narrative_ontology:cs_reading_relation('2a52dc20-06d6-4c56-b810-4ed174db9909', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('2a52dc20-06d6-4c56-b810-4ed174db9909', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('2a52dc20-06d6-4c56-b810-4ed174db9909', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('2a52dc20-06d6-4c56-b810-4ed174db9909', foundational, amendment_sole_legitimate_exit_channel).
narrative_ontology:cs_axiom_status(amendment_sole_legitimate_exit_channel, holdable).
narrative_ontology:cs_axiom_grounding('2a52dc20-06d6-4c56-b810-4ed174db9909', amendment_sole_legitimate_exit_channel, conventional).
narrative_ontology:cs_axiom('2a52dc20-06d6-4c56-b810-4ed174db9909', foundational, constituent_authority_vests_in_constitutional_whole).
narrative_ontology:cs_axiom_status(constituent_authority_vests_in_constitutional_whole, holdable).
narrative_ontology:cs_axiom_grounding('2a52dc20-06d6-4c56-b810-4ed174db9909', constituent_authority_vests_in_constitutional_whole, conventional).
narrative_ontology:cs_reference_frame('2a52dc20-06d6-4c56-b810-4ed174db9909', indestructible_constitutional_union).
narrative_ontology:cs_drift_state('2a52dc20-06d6-4c56-b810-4ed174db9909', contemporary_referendum_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('2a52dc20-06d6-4c56-b810-4ed174db9909', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, smaller_constituent_provinces).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_internal_minorities).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_province_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_province_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_province_governments).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, indestructible_union_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the rule that no province or state may leave the union on its own motion: it litigates to defend the prohibition, controls when the constitutional amending process may be initiated, and holds the fiscal perimeter — transfers, shared debt, defense commitments — that depends on the perimeter staying fixed. It receives the exit-decision authority the rule concentrates: no departure happens without a process it participates in and can slow. Its own exit from the arrangement is not a question; it is the arrangement's operator.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, continental).

% Adjudicate the boundary: they have voided unilateral secession ordinances and issued the reference opinions that define what lawful exit would require. Their interpretive authority is fused with the constitutional order they enforce — a court that held the order optional would dissolve its own warrant. They cannot step outside the frame to evaluate it; they argue within it or not at all.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, continental).

% A provincial majority that periodically mobilizes to leave the union: it wins referendum mandates, litigates for recognition, and negotiates. The rule bars its preferred unilateral route outright; its only lawful channel is the amending process, whose consent requirements are held by the very order it seeks to leave. It bears the recurring costs of mobilization and of fiscal ties it cannot sever on its own motion. Its practical exit is the channel's gatekeepers saying yes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_province_majority, payer,
    organized, biographical, constrained, regional).

% Govern resource-rich provinces: they draw union-wide market access, federal infrastructure, and defense of their resource jurisdiction against external claims, and they fund net outflows into the shared fiscal pool. The rule means they cannot unilaterally capture their resource base by leaving; their periodic separation talk is disciplined by the knowledge that exit requires continent-spanning consent. Their position is dual — they draw union benefits and pay union costs, and the fixed perimeter holds both in place.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_province_governments, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_province_governments, payer).

% Small provinces protected on both flanks: the union shields them from absorption by larger neighbors, pools defense and currency costs they could not carry alone, and keeps the secessionist province — whose transport and market corridors they depend on — inside the perimeter. They have no independent exit worth taking and no desire to; their stake is the perimeter's stability.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, smaller_constituent_provinces, beneficiary,
    moderate, generational, constrained, continental).

% Linguistic and immigrant communities inside the secessionist province who oppose departure: a provincial-majority exit would take them out of the country without their consent. The rule that bars unilateral exit protects them from being moved by a vote they lose. Their personal exit option is internal migration, which they hold but would rather not exercise; their protection is collective and legal.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_internal_minorities, beneficiary,
    moderate, biographical, mobile, regional).

% Nations holding treaties that predate the constitutional order. Their position is that no rearrangement of sovereignty over their territories is legitimate without their consent — including the amendment process itself, which does not seat them as gatekeepers. They litigate inside the frame and assert their claims in public argument, but the rule's channel gives their consent claim no structural vote. They are held inside the perimeter whose final shape they were never seated to contest.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, indigenous_treaty_holders, excluded,
    organized, generational, trapped, continental).

% Other states and international bodies. Their recognition practice — withholding recognition from unilateral secessions outside decolonization and occupation — reinforces the rule from outside the federation: a province that left on its own motion would find doors closed. They take no position on the federation's internal fiscal politics; their stake is the precedent any successful unilateral exit would set for their own restless regions.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, international_state_system, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single federal membership perimeter: no constituent unit can be expelled or walk out unilaterally, which keeps one currency, common market, defense perimeter, and treaty identity intact through regional conflict; all exit ambition is routed into a single supermajoritarian amendment process that forces any exit-seeker to build a coalition spanning the whole federation.
% TRANSFER_FUNCTION: Moves exit-decision authority from provincial majorities to the federal constitutional order: the power to say yes to a region's departure is concentrated in an amending formula the departing region does not control. Secondarily, the barred-exit rule holds resource-rich provinces' resource bases inside the union's fiscal pool — unilateral capture-by-exit is off the table, so transfer flows continue under a take-it-or-amend frame.
% ABSENT_VOICES: Three constituencies are inside the polity but outside this reading's frame of legitimate argument: provincial sovereigntists (their referendum majorities are classified as constitutionally void rather than as claims to answer), structural-injustice challengers (their threshold arguments are classified as extra-constitutional and therefore inadmissible), and Indigenous treaty holders (their consent claim predates the frame and holds no seat in the amending formula). Within this reading's adjudication they appear only as rule-breakers or litigants, never as co-authors of the exit rule; their absence from the channel's design is precisely what the sibling readings exist to press.
% DISAPPEARANCE_RATIONALE: If the impermissibility rule vanished overnight, the world rearranges: resource-rich provinces would face immediate exit referenda with credible unilateral effect, the fiscal-transfer pool would unravel under exit threat, currency and debt arrangements would need renegotiation, and every province's bargaining position would reset around secession leverage. The federation's members are organized around the fixed perimeter; removing it reorganizes them.
% FOUNDING_PROBLEM: A continental union assembled from formerly autonomous colonies needed a membership rule that no member could unilaterally break without collapsing the shared currency, common market, and defense perimeter — and that no central authority could use to expel a region either. The settlement was symmetrical entrenchment: unilateral exit impermissible for everyone, legitimate exit only through a negotiated amendment requiring continent-spanning consent.
% FOUNDING_PROBLEM_CORROBORATION: The perimeter problem is current: secessionist mobilization is recurrent and fiscal and resource integration keeps deepening the stakes of exit. Corroboration from outside the benefiting parties: secessionist movements themselves pursue negotiated, consent-based referendum strategies rather than pure unilateralism — attesting that the negotiated channel is the operative legitimacy frame — and international recognition practice (non-recognition of unilateral secessions outside decolonization and occupation) attests the rule from a seat with no stake in this federation's fiscal politics. The parties with the strongest incentive to declare the founding problem dead instead treat the channel as live enough to litigate in.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).
:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type rope from this reading's seat: the arrangement solves a real collective-action problem — no member can be expelled or walk out unilaterally, so one currency, market, defense perimeter, and treaty identity survive regional conflict — and every declared beneficiary group draws net value from it. The metrics describe the operation independently of that claim. Suppression 0.58: the prohibition does not hold by consent alone — it is enforced by courts, and for the interval's first decades by arms; enforcement intensity decayed from 0.85 at t0 (post-settlement military consolidation) to 0.40 at t80 (settled judicial routine), then climbed back to 0.58 as referendum-era mobilization resumed, because the rule must be actively re-asserted each time a province tests it. Resistance 0.62: recurrent secessionist mobilization across the interval — never a successful unilateral breach, never extinguished. Accessibility_collapse 0.60: unilateral routes are categorically closed, but the amendment channel remains open as a real alternative, so alternatives collapse only partially. Theater 0.15: the rule genuinely binds; ceremonial constitutionalism is marginal. Extractiveness 0.22, rising slowly from 0.14: this reading holds the extraction claim invalid, and the residual ε tracks the deepening fiscal hold on resource-rich provinces rather than any rent this reading would recognize. All three series run on one shared nine-point grid (t=0..160, step 20) so no metric's end-state is backfilled into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the federal seat the arrangement is the constitution working as designed: a perimeter that makes regional politics possible by taking exit off the table. From the secessionist province's seat the same structure is a gate whose key is held by the counterparty — a lawful channel that has never once been walked through. From the excluded treaty-holder seat it is a frame that never seated them and then described their consent claim as out of order. From the internal-minority seat it is protection: the rule that bars the province's majority also bars that majority's power to move them. The engine computes per-seat classifications from the structural data; this file authors the federal-frame reading's claim and metrics, and the divergence across seats is the kernel's live content, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for federal_government, smaller_constituent_provinces, provincial_internal_minorities, and resource_rich_province_governments (net union beneficiaries under this reading's lights). The secessionist_province_majority bears the arrangement's costs but is deliberately NOT declared a victim: this reading holds the federal-extraction claim invalid, so no victim set is authored — its directionality derives from its constrained exit and lands mid-scale (a bound member, not a robbed sovereign). No directionality_overrides are used: the derivation from declarations plus exit options already produces this reading's own picture. The sibling stories over the same referent will declare victim sets; the cross-reading divergence is the family's signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — perimeter stability for a continental union — is live, so no mandatrophy declaration is authored: the arrangement has not outlived its function, and the (status=live, verdict=world_rearranges) pairing carries no zombie flag. The rope claim guards against the sibling trap of reading legitimate coordination as pure extraction; the authored suppression (0.58) and resistance (0.62) guard against the federal trap of reading an enforced perimeter as costless consent. The drift risk to watch is channel reachability (omega negotiated_channel_reality): if the amendment channel is formally open but practically unreachable, the arrangement drifts from coordination-with-exit-channel toward enforced enclosure, and the rope claim fails from every seat. The slow ε rise in the series tracks fiscal deepening, not mandate decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel secession_legitimacy_boundary (reading: constitutional_impossibility_reading). What does the classification become under the sibling readings over the same standing arrangement?',
    'Generate the three sibling stories over the identical referent (same union-beneficiary facts, same enforcement history) and compare per-reading ε, victim sets, and computed types.',
    'Siblings author high ε and declare secessionist provinces as victims; computed types shift toward tangled_rope/snare from those seats. Cross-reading divergence over one referent is the kernel''s primary data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel referent.').

omega_variable(
    foreclosure_symmetry,
    'The authored reading_relations declare forecloses to all three siblings from this seat; from each sibling seat, this reading is the one foreclosed. Is the mutual exclusivity real, or an artifact of authoring from the supremacy seat?',
    'Engine computation of axiom contradiction from grounding types and drift states across all four readings; compare the computed foreclosure graph to the authored edges.',
    'If computed foreclosure is asymmetric (this reading forecloses siblings but not conversely), the supremacy claim is structurally dominant, not merely first-authored; if symmetric foreclosure fails to compute, the kernel is a coexistence dispute and this reading''s categorically-illegitimate framing overclaims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_symmetry, conceptual, 'Seat-relativity of the foreclosure structure across the kernel''s four readings.').

omega_variable(
    negotiated_channel_reality,
    'Is the amendment channel a practically reachable exit, or a formally open gate whose key is held by the counterparty being exited from? No federation member has ever exited through it.',
    'Comparative analysis of amendment-formula reachability: identify any case where a region met every formal precondition and assess whether exit would actually have been granted; treat the total absence of precedent as evidence.',
    'If the channel is practically closed, accessibility_collapse rises toward 0.85, effective suppression exceeds the authored 0.58, and the rope claim fails even from this reading''s seat — the arrangement computes as enforced enclosure. If reachable, rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiated_channel_reality, empirical, 'Practical reachability of the sole lawful exit channel.').

omega_variable(
    naturalization_of_indestructibility,
    'Is the indestructible union a discovered constitutional fact (the tradition''s own naturalizing language: the Constitution looks to an indestructible Union) or an authored, amendable political choice?',
    'Doctrinal analysis of whether the reading''s tradition treats indestructibility as derivable from enacted text alone or as an interpretive overlay; test against whether the tradition would accept an amendment formally permitting unilateral secession.',
    'If naturalized, the constraint presents as mountain-like and the beneficiary declarations make it a false-summit candidate for FSM evaluation; if conventional, it is an ordinary entrenched rule whose legitimacy tracks the amending process itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_of_indestructibility, conceptual, 'Natural-law versus constructed status of the union''s indestructibility.').

omega_variable(
    fiscal_binding_drift,
    'Does deepening fiscal and resource integration raise the arrangement''s hold on resource-rich provinces — and with it the barred-exit cost those provinces bear — even under this reading''s lights, where the extraction claim is invalid?',
    'Net fiscal-flow series for secessionist-prone resource provinces across the interval; correlate the ε rise (0.14 to 0.22) with transfer dependence.',
    'If the ε rise tracks transfer deepening, the no-victim framing strains at the margins: the same structure that is coordination for recipients is progressively binding for net contributors, and the sibling grievance reading gains empirical footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_binding_drift, empirical, 'Whether fiscal deepening drives the measured ε drift and strains the no-victim claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_impossibility_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(secession_impossibility_tr_t0, observed).
narrative_ontology:measurement(secession_impossibility_tr_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(secession_impossibility_tr_t20, observed).
narrative_ontology:measurement(secession_impossibility_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(secession_impossibility_tr_t40, observed).
narrative_ontology:measurement(secession_impossibility_tr_t60, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement_basis(secession_impossibility_tr_t60, observed).
narrative_ontology:measurement(secession_impossibility_tr_t80, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement_basis(secession_impossibility_tr_t80, observed).
narrative_ontology:measurement(secession_impossibility_tr_t100, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(secession_impossibility_tr_t100, observed).
narrative_ontology:measurement(secession_impossibility_tr_t120, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 120, 0.13).
narrative_ontology:measurement_basis(secession_impossibility_tr_t120, observed).
narrative_ontology:measurement(secession_impossibility_tr_t140, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 140, 0.14).
narrative_ontology:measurement_basis(secession_impossibility_tr_t140, observed).
narrative_ontology:measurement(secession_impossibility_tr_t160, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 160, 0.15).
narrative_ontology:measurement_basis(secession_impossibility_tr_t160, observed).

% Extraction over time
narrative_ontology:measurement(secession_impossibility_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(secession_impossibility_be_t0, observed).
narrative_ontology:measurement(secession_impossibility_be_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(secession_impossibility_be_t20, observed).
narrative_ontology:measurement(secession_impossibility_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(secession_impossibility_be_t40, observed).
narrative_ontology:measurement(secession_impossibility_be_t60, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement_basis(secession_impossibility_be_t60, observed).
narrative_ontology:measurement(secession_impossibility_be_t80, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(secession_impossibility_be_t80, observed).
narrative_ontology:measurement(secession_impossibility_be_t100, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 100, 0.19).
narrative_ontology:measurement_basis(secession_impossibility_be_t100, observed).
narrative_ontology:measurement(secession_impossibility_be_t120, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 120, 0.2).
narrative_ontology:measurement_basis(secession_impossibility_be_t120, observed).
narrative_ontology:measurement(secession_impossibility_be_t140, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 140, 0.21).
narrative_ontology:measurement_basis(secession_impossibility_be_t140, observed).
narrative_ontology:measurement(secession_impossibility_be_t160, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 160, 0.22).
narrative_ontology:measurement_basis(secession_impossibility_be_t160, observed).

% Suppression requirement over time
narrative_ontology:measurement(secession_impossibility_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(secession_impossibility_su_t0, observed).
narrative_ontology:measurement(secession_impossibility_su_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(secession_impossibility_su_t20, observed).
narrative_ontology:measurement(secession_impossibility_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(secession_impossibility_su_t40, observed).
narrative_ontology:measurement(secession_impossibility_su_t60, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(secession_impossibility_su_t60, observed).
narrative_ontology:measurement(secession_impossibility_su_t80, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement_basis(secession_impossibility_su_t80, observed).
narrative_ontology:measurement(secession_impossibility_su_t100, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement_basis(secession_impossibility_su_t100, observed).
narrative_ontology:measurement(secession_impossibility_su_t120, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement_basis(secession_impossibility_su_t120, observed).
narrative_ontology:measurement(secession_impossibility_su_t140, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 140, 0.55).
narrative_ontology:measurement_basis(secession_impossibility_su_t140, observed).
narrative_ontology:measurement(secession_impossibility_su_t160, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 160, 0.58).
narrative_ontology:measurement_basis(secession_impossibility_su_t160, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'secession legitimacy' decomposes into four reading-stories over one standing arrangement (the constitutional prohibition plus amendment-only exit). Each story carries its own reading-indexed ε, its own beneficiary/victim structure, and its own claimed type; this file is the federal-frame reading (low ε, no victim set, claimed rope). Members link via affects_constraints; the upstream story is this one — the legally operative reading that courts enforce — which structurally conditions the environment in which the sibling readings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
