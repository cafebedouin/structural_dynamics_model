% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty — Sovereignty Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint is the sovereignty reading of the federation-membership
 *   kernel: federation membership is a conditional interstate treaty,
 *   national authority retains the legitimacy to control borders, and free
 *   movement is a negotiable policy rather than a constitutional entitlement.
 *   Under this reading, host-state labor markets and national governments
 *   benefit each time border legitimacy is reasserted (quotas, emergency
 *   suspensions, renegotiated derogations), while mobile citizens,
 *   cross-border workers, and binational families bear the cost of a right
 *   they treated as durable being revealed as conditional. The sibling
 *   integration_reading constraint (not authored here) holds the opposite
 *   premise — free movement as an irreversible constitutional right,
 *   supranational authority as legitimate adjudicator — and would show a very
 *   different beneficiary/victim structure and a much lower ε for mobility
 *   restriction, because under that reading restriction itself is
 *   illegitimate rather than a lawful exercise of retained sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.71).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty — Sovereignty Reading").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '5bf584c2-e6b1-4f5f-9d56-5be92936d0b2').
narrative_ontology:cs_kernel_codification('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', fixed_text).
narrative_ontology:cs_authority_grounding('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', distributed).
narrative_ontology:cs_reading_relation('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', foundational, treaty_revocability_persists_absent_express_surrender).
narrative_ontology:cs_axiom_status(treaty_revocability_persists_absent_express_surrender, holdable).
narrative_ontology:cs_axiom_grounding('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', treaty_revocability_persists_absent_express_surrender, conventional).
narrative_ontology:cs_axiom('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', foundational, border_control_is_retained_sovereign_competence).
narrative_ontology:cs_axiom_status(border_control_is_retained_sovereign_competence, holdable).
narrative_ontology:cs_axiom_grounding('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', border_control_is_retained_sovereign_competence, deontological).
narrative_ontology:cs_reference_frame('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', founding_treaty_interstate_bargain).
narrative_ontology:cs_drift_state('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', contemporary_free_movement_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5bf584c2-e6b1-4f5f-9d56-5be92936d0b2', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, host_state_local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, host_state_border_control_apparatus).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments_asserting_reentry_rights).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_federation_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, binational_families).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_sovereignty_persistence_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, treaty_revocability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Domestic workers and unions in receiving member states who benefit when free movement is treated as a negotiable, suspendable policy rather than a constitutional guarantee. Wage competition from incoming labor is throttled whenever a national government reasserts border authority; this seat collects the protective benefit each time the treaty's mobility clause is tightened or suspended.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, host_state_local_labor_markets, beneficiary,
    organized, generational, constrained, national).

% The national ministries, border agencies, and courts that administer entry and exit under this reading. They set and enforce the terms under which the federation's free-movement provision applies, invoke emergency or security clauses to suspend it, and treat every renewal of membership as an occasion to renegotiate the mobility terms. Their authority is the mechanism the constraint runs on.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, host_state_border_control_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Elected national governments that derive domestic political legitimacy from demonstrating they can still control who crosses their border, notwithstanding federation membership. They benefit from the treaty-not-constitution framing because it preserves a policy lever they can deploy for electoral or fiscal advantage, and they administer the derogation and quota mechanisms that make the free-movement clause conditional rather than absolute.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments_asserting_reentry_rights, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, national_governments_asserting_reentry_rights, agenda_setter).

% Citizens of one member state who have built lives, jobs, or family ties in another, relying on the expectation that free movement is a durable feature of federation membership. Under this reading their residence, work authorization, and family reunification status become renegotiable whenever a host government invokes border legitimacy — they cannot easily relocate again mid-crisis and bear the cost of a right they believed was settled being treated as a revocable policy.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_federation_citizens, payer,
    moderate, biographical, constrained, continental).

% Workers who commute or migrate seasonally across an internal federation border for employment that depends on continuous, predictable access. Quota reimpositions, emergency border checks, or renegotiated bilateral labor quotas can eliminate their livelihood on short notice; they have the least capacity to absorb a policy reversal and the least standing to contest it.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    powerless, immediate, trapped, regional).

% Households spanning two member states whose ability to live together depends on free movement remaining unconditional. When a national government reasserts border legitimacy — via residency caps, family-reunification quotas, or emergency suspension — these households face separation, relocation, or loss of legal status for one partner, with no direct standing in the intergovernmental renegotiation that produced the change.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, binational_families, payer,
    powerless, biographical, trapped, regional).

% The federation-level courts and commissions that would, under the rival integration reading, treat free movement as a constitutional right enforceable against member states. Under the sovereignty reading their rulings are treated as advisory or subject to national override; they are structurally sidelined from the actual adjudication of mobility disputes even though they would object to the conditional-treaty framing.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_supranational_institutions, excluded,
    institutional, civilizational, analytical, continental).

% Academics and jurists who study whether federation founding treaties create binding supranational rights or remain revocable interstate agreements. They analyze the pattern of derogations, opt-outs, and emergency suspensions to assess which reading better describes the federation's actual operating constitution, without themselves holding power to settle the dispute.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the terms under which member states admit each other's citizens for work and residence, allowing states to capture the benefits of a shared labor and mobility area while retaining a mechanism to modulate or suspend access when domestic political, security, or labor-market conditions require it.
% TRANSFER_FUNCTION: Moves the practical, day-to-day certainty of residence, work authorization, and family unity away from mobile citizens and cross-border workers and toward host-state labor markets and national governments, each time a government invokes border legitimacy to tighten or suspend movement.
% ABSENT_VOICES: Mobile citizens who relocated in reliance on free movement being durable, and the federation's own supranational courts and commissions, would object that the conditional-treaty framing retroactively downgrades what they treated as a settled right; neither group has a seat in the intergovernmental renegotiations where derogations and quotas are set.
% DISAPPEARANCE_RATIONALE: If national border-legitimacy claims disappeared and free movement became unconditional and non-derogable overnight, host-state labor-market protections would lose their policy lever, cross-border workers and binational families would gain durable legal certainty, and the entire architecture of quotas, emergency suspensions, and reentry negotiations would become moot — member-state governments would lose a major domestic political tool.
% FOUNDING_PROBLEM: Founding member states needed a way to gain the economic benefits of a shared labor and movement area without permanently surrendering the capacity to control entry during crises (security shocks, sudden labor-market shifts, fiscal strain on welfare systems) — the treaty form was chosen precisely to preserve exit and modulation options that a constitutional guarantee would foreclose.
% FOUNDING_PROBLEM_CORROBORATION: National border and interior ministries attest the problem (need for crisis-responsive control) remains live and cite recent invocations of emergency suspension as proof. Independent legal scholars and the federation's own supranational institutions attest that decades of practice have hardened free movement into a de facto constitutional expectation for citizens, making the conditional-treaty framing a retrofit that serves state discretion rather than a genuinely still-live founding concern; migrant advocacy organizations outside both governmental camps corroborate the scholars' reading.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.71) tracking the accumulating pattern of derogations and emergency border reassertions across federation history — each invocation transfers more certainty away from mobile populations toward host-state political and labor-market beneficiaries. Suppression is moderate-high (0.62) because the mechanism runs on genuine legal authority (border control is not covertly coercive; it is openly asserted as a retained sovereign power) but nonetheless forecloses exit for people who have already relocated. Theater ratio stays low-moderate (0.28): the security and labor-market justifications for border reassertion are substantially real, not purely performative, though a growing share of enforcement activity (documentation checks, quota administration) serves the treaty-conditionality claim itself.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (host-state border apparatus, national governments), this constraint reads as a genuine coordination achievement: it preserves the benefits of a shared labor area while retaining crisis-responsive control, a real coordination function under active enforcement. From the payer seats, the identical structure reads as extraction — a right they relied on being unilaterally downgraded to a policy the more powerful party can revoke. This divergence is exactly what the tangled_rope classification is built to hold: real coordination function (a shared mobility area with modulation capacity) coexisting with asymmetric extraction (mobile populations bear costs the host-state beneficiaries do not), sustained only by active enforcement of border legitimacy claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Host-state labor markets and the national governments that administer border legitimacy are declared beneficiaries: they collect protective and political value each time the mobility clause is treated as revocable. Mobile citizens, cross-border workers, and binational families are declared victims: they bear the cost of the conditional-treaty framing precisely because they built biographical or economic commitments assuming durability. Cross-border workers and binational families sit nearest the full-target end of directionality — trapped exit options, immediate/biographical time horizons, powerless — while mobile federation citizens with somewhat more resources sit slightly less extreme (moderate power, constrained rather than trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving crisis-responsive control while capturing coordination gains — has genuine live elements (security shocks, welfare-system strain are real), which is why founding_problem_status is authored as contested rather than dead: unlike a pure mandatrophy case, this constraint's coordination function has not simply evaporated. But the corroboration split (ministries say live; scholars and advocacy groups outside the beneficiary set say the framing has hardened into discretionary rent-preservation) shows the classification correctly resists collapsing this into either pure coordination (rope) or pure extraction (snare) — tangled_rope captures the genuine dual structure rather than mislabeling a live-but-contested coordination problem as either category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_vs_constitution_kernel_ambiguity,
    'Is federation membership structurally a revocable interstate treaty (this reading) or a constitutional order that has superseded ordinary treaty revocability (the integration_reading sibling)? The federation''s founding documents and decades of jurisprudence can be read either way.',
    'A definitive ruling from the federation''s highest court on whether member states retain unilateral derogation power over free movement in non-emergency conditions, and whether that ruling is actually complied with by member states afterward (compliance, not just issuance, resolves which reading describes practice).',
    'If the treaty reading is vindicated, border reassertion is a legitimate retained power and this constraint''s classification as tangled_rope (rather than snare) is well-grounded — a real coordination function persists alongside the extraction. If the constitutional reading is vindicated, the same border-reassertion practice becomes an unlawful breach and would reclassify closer to snare, since the coordination cover story would no longer have legal standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_vs_constitution_kernel_ambiguity, conceptual, 'Whether membership is a revocable treaty or a superseding constitutional order — the core kernel dispute.').

omega_variable(
    reading_selection_pressure,
    'Which reading of the kernel prevails at any given moment appears to track which member states currently hold negotiating leverage rather than a stable legal fact — is the kernel ambiguity itself being exploited strategically by governments who invoke sovereignty language only when it favors a desired policy outcome?',
    'Track the correlation between which reading a national government asserts and whether that government is a net sender or net receiver of cross-border migrants in the relevant period; a strong correlation would indicate strategic reading-selection rather than principled constitutional commitment.',
    'If reading-selection is strategic, this reduces both this story''s and the integration_reading''s claim to describe a stable structural fact, and would support treating the kernel itself (not either reading) as the object of a higher-order power analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_pressure, conceptual, 'Whether governments select readings opportunistically rather than holding one consistently.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by mobile citizens and cross-border workers structural (legal border-control apparatus, visa/quota systems) or partly internalized (people who relocated under an expectation of permanence continuing to under-hedge against reversal even after seeing other member states reimpose controls)?',
    'Survey data on relocation and contingency-planning behavior among mobile federation citizens before and after a well-publicized derogation event in a comparable member state.',
    'If suppression is partly internalized (normalized trust in the free-movement guarantee), the effective suppression these populations carry is higher than the structural measure captures, and post-derogation hardship would be compounded by a failure to have hedged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mobile populations under a conditional-treaty regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t6, federation_membership__sovereignty_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(fede_tr_t12, federation_membership__sovereignty_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(fede_tr_t18, federation_membership__sovereignty_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(fede_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t6, federation_membership__sovereignty_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(fede_be_t12, federation_membership__sovereignty_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(fede_be_t18, federation_membership__sovereignty_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(fede_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t6, federation_membership__sovereignty_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(fede_su_t12, federation_membership__sovereignty_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(fede_su_t18, federation_membership__sovereignty_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(fede_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint and federation_membership__integration_reading are the two authored readings of the single federation_membership kernel. Both describe the same underlying institutional arrangement (a federation with a free-movement provision and member states with border-control apparatus) but assign opposite legitimacy and structural weight to national vs. supranational authority, producing different beneficiary/victim sets and substantially different ε for the same observable practice (border reassertion). They are linked here rather than merged because ε-invariance requires each reading to carry its own stable extraction value; averaging or parameterizing across the readings would violate that principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
