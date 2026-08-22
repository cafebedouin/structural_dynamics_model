% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Unable-or-Unwilling Doctrine Reading of Article 51 Self-Defense
 *   domain: international law / security studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Article 51
 *   self-defense kernel: the unable-or-unwilling doctrine, under which a
 *   non-state actor's armed attack from a host state that is unwilling or
 *   unable to suppress the threat triggers a right of unilateral cross-border
 *   response. The reading is the moderate middle of the kernel family — it
 *   requires an actual attack (rejecting the expansive_preventive_reading's
 *   purely preventive reach) while dispensing with state attribution
 *   (rejecting the narrow_armed_attack_reading's effective-control
 *   requirement). The ε referent is fixed: the standing arrangement under
 *   contest — the accumulated practice of unilateral cross-border force
 *   justified by host-state unwillingness-or-inability — assessed by this
 *   reading's own lights, never by the sibling readings' preferred
 *   arrangements. The claim and the metrics are independent authored facts:
 *   the constraint is CLAIMED as a tangled_rope (a genuine coordination
 *   function welded to asymmetric extraction), and the metrics describe
 *   moderately-high extraction with rising theater and an enforcement
 *   ratchet; the engine computes per-seat classifications from the structural
 *   data and measures any divergence from the claim.
 *
 * KEY AGENTS:
 *   - intervening_counterterrorism_states: agenda-setter and primary beneficiary (institutional/arbitrage) — asserts the doctrine, unilaterally judges host-state unwillingness, collects the operational freedom; can decline invocation when its logic cuts against itself
 *   - host_states_of_nonstate_threats: primary target (moderate/trapped) — territorial control bypassed without consent or neutral determination
 *   - host_state_civilians: secondary target (powerless/trapped) — bear kinetic and destabilization costs with no seat in the determination
 *   - regional_invoking_states: secondary beneficiary (powerful/constrained) — gain a legal vocabulary, pay precedent and retaliation costs
 *   - doctrinal_objection_bloc: excluded voice (organized/constrained) — rejects the doctrine, cannot stop the practice
 *   - un_security_council: analytical observer (institutional/analytical) — designated arbiter, bypassed in operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.6).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Unable-or-Unwilling Doctrine Reading of Article 51 Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international law / security studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '16752bc0-48bb-4434-9341-91b7f2e5192c').
narrative_ontology:cs_kernel_codification('16752bc0-48bb-4434-9341-91b7f2e5192c', formalized).
narrative_ontology:cs_authority_grounding('16752bc0-48bb-4434-9341-91b7f2e5192c', practice).
narrative_ontology:cs_interpretation_layer_present('16752bc0-48bb-4434-9341-91b7f2e5192c').
narrative_ontology:cs_reading_relation('16752bc0-48bb-4434-9341-91b7f2e5192c', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('16752bc0-48bb-4434-9341-91b7f2e5192c', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('16752bc0-48bb-4434-9341-91b7f2e5192c', foundational, nonstate_armed_attack_engages_article_51).
narrative_ontology:cs_axiom_status(nonstate_armed_attack_engages_article_51, holdable).
narrative_ontology:cs_axiom_grounding('16752bc0-48bb-4434-9341-91b7f2e5192c', nonstate_armed_attack_engages_article_51, conventional).
narrative_ontology:cs_axiom('16752bc0-48bb-4434-9341-91b7f2e5192c', foundational, host_unwillingness_inability_licenses_unilateral_force).
narrative_ontology:cs_axiom_status(host_unwillingness_inability_licenses_unilateral_force, holdable).
narrative_ontology:cs_axiom_grounding('16752bc0-48bb-4434-9341-91b7f2e5192c', host_unwillingness_inability_licenses_unilateral_force, conventional).
narrative_ontology:cs_axiom('16752bc0-48bb-4434-9341-91b7f2e5192c', secondary, self_judged_necessity_permissible).
narrative_ontology:cs_axiom_status(self_judged_necessity_permissible, holdable).
narrative_ontology:cs_axiom_grounding('16752bc0-48bb-4434-9341-91b7f2e5192c', self_judged_necessity_permissible, instrumental).
narrative_ontology:cs_reference_frame('16752bc0-48bb-4434-9341-91b7f2e5192c', charter_regulated_inherent_right).
narrative_ontology:cs_drift_state('16752bc0-48bb-4434-9341-91b7f2e5192c', contemporary_state_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('16752bc0-48bb-4434-9341-91b7f2e5192c', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, regional_invoking_states).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_of_nonstate_threats).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, regional_invoking_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, inherent_right_preservation_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, due_diligence_sovereignty_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major military powers that have suffered non-state armed attacks launched from other states' territories. They unilaterally assess whether the territorial state is 'unwilling or unable' to suppress the threat, conduct cross-border force on that assessment, and notify the Security Council after the fact. They collect the operational freedom the reading grants, and they can decline to invoke it whenever its logic would cut against their own conduct as territorial hosts.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states, beneficiary).

% Regional powers that invoke the doctrine against armed groups operating from neighboring territories. They gain a recognized legal vocabulary for cross-border operations they would often conduct regardless, but each invocation also strengthens a precedent they would resist if a stronger power ever applied it to them, and their operations draw retaliatory, diplomatic, and financial costs.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, regional_invoking_states, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, regional_invoking_states, payer).

% States on whose territory armed non-state groups operate, whether because they lack the capacity to suppress them or have chosen not to. When an intervening state unilaterally declares them unwilling or unable, their territorial control is set aside without their consent and without any neutral determination. Their options are protest at the Security Council (often blocked by the intervening state's alliances or veto), slow litigation with jurisdictional limits, or militarily costly resistance.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_of_nonstate_threats, payer,
    moderate, biographical, trapped, national).

% Populations living where the strikes land. They bear casualties, displacement, and the destabilization feedback that follows cross-border operations. No mechanism seats them in the unwillingness-or-inability determination, and the territorial state's objection does not operate as a filter on their behalf.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_civilians, payer,
    powerless, immediate, trapped, local).

% A large bloc of states, including much of Latin America and the Non-Aligned Movement, that rejects the doctrine as inconsistent with the Charter's prohibition on force. They object in General Assembly debates and Sixth Committee sessions but lack the power to stop the practice; their exclusion from effective decision is what lets each invocation accumulate as precedent.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, doctrinal_objection_bloc, excluded,
    organized, generational, constrained, continental).

% Formally the Charter's designated authority for authorizing cross-border force and for reviewing self-defense claims under the Article 51 reporting requirement. In practice it receives after-the-fact notification letters and rarely adjudicates them; its authorization role is bypassed by the intervening state's unilateral assessment.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real gap the state-centric Charter framework leaves open: when a non-state armed group inflicts an armed attack from territory whose host state cannot or will not suppress it, the attacked state otherwise has no lawful response channel that neither treats the host as an aggressor nor waits on a Security Council that is often blocked. The reading coordinates a response path while retaining one limit — an actual non-state attack must have occurred.
% TRANSFER_FUNCTION: Moves decision authority over cross-border force from the collective Security Council process to the intervening state's unilateral assessment; moves the cost of suppressing transnational armed groups onto the host state's sovereignty; moves kinetic and political risk onto host-state territory and the civilians living there.
% ABSENT_VOICES: Host-state civilians have no seat anywhere in the determination. The doctrinal objection bloc objects in General Assembly forums but is excluded from effective decision. The Security Council's authorization role is formally present but bypassed in operation — it receives notification, not a request for consent.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, intervening states would face a choice: revert to the narrow attribution-based reading and leave non-state attacks from unwilling host states unanswered, or act with no legal justification at all, straining the alliances and basing agreements the current practice rests on. The anti-ISIS coalition architecture, decades of precedent-stacked invocation letters, and the operational patterns built on them would all need re-grounding, while host states would regain a shield over their territory that they currently lack.
% FOUNDING_PROBLEM: The Charter's state-centric self-defense framework offered no lawful response when a non-state armed group inflicts an attack of state-scale gravity from territory whose host state cannot or will not suppress it: the effective-control attribution standard leaves the attacked state without an answerable state, while the prohibition on force bars entry into the host's territory. The 9/11 attacks made the gap unignorable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Security Council resolutions 1373 (2001) and 2249 (2015) call on all states to suppress non-state armed threats, attesting the underlying problem; host states corroborate the threat's reality when they consent to or request assistance against groups on their own territory (Iraq against ISIS, Somalia against Al-Shabaab) even while rejecting the unilateral doctrine; the doctrinal objection bloc attests the threat is real while disputing the remedy. No source outside the beneficiary set attests that the unilateral, self-judged form of the doctrine specifically is necessary — that claim rests on the intervening states' own assertions.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the reading transfers decision authority over cross-border force to the intervening state's own assessment: the host state pays sovereignty, the intervening state collects operational freedom, and the rate of that transfer is decoupled from any neutral standard. Suppression is 0.6 as a raw structural property (unscaled by power or scope): the host state's alternatives — Security Council protest, slow litigation, costly resistance — are thin in practice, though the Charter framework itself remains formally intact, so suppression is substantial but not near-total. Theater_ratio (0.45) reflects a real functional core — the attacks are real, the armed groups real, the response gap real — wrapped in a growing share of performative legal activity: self-judging assessments, after-the-fact notification letters, and precedent-stacking that functions more to accumulate a record than to test necessity. Accessibility_collapse is 0.5: once the reading is accepted in practice, the narrow alternative (no force without attribution) becomes hard to maintain, but the Security Council route and consent-based operations remain genuinely available alternatives. Resistance is 0.6: sustained doctrinal objection from a large bloc of states, host-state protests, and scholarly opposition, none of which has stopped the practice. Suppression_requirement measurements are authored because this story specifically tracks an enforcement ratchet: the doctrine's persistence requires increasingly active assertion (invocation letters, alliance pressure, precedent citation) against a growing objection bloc, so the enforcement burden rises over the interval. All three metric series share one time grid (t=0..24 in yearly units from 2001) so the engine samples every metric at every point without substitution.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the intervening state's seat, the arrangement presents as a necessary legal instrument it built to answer a real attack-gap — closer to coordination it maintains. From the host state's seat, the same structure operates as enforced sovereignty transfer judged by the party that benefits from the judgment — closer to extraction it cannot veto. The regional invoker's seat is genuinely mixed: beneficiary when invoking, prospective payer under the precedent it creates. The engine computes these per-seat classifications from power, exit, and role data; the authored tangled_rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening_counterterrorism_states are declared beneficiaries with arbitrage-grade exit: they invoke the doctrine when useful and disclaim it when exposed, so their derived directionality sits near the full-beneficiary end and effective extraction inverts toward subsidy. Host_states_of_nonstate_threats and host_state_civilians are declared victims with trapped exit: no neutral forum, no veto, no exit from the territory — they sit near the full-target end and their effective extraction is amplified. Regional_invoking_states carry beneficiary with a secondary payer role: their derived directionality is low but not floor-level, reflecting precedent costs they bear. The doctrinal_objection_bloc is authored as an excluded stakeholder, not a beneficiary or victim — it feeds the consensus-provenance check (the practice's 'acceptance' is manufactured partly by excluding these seats) but not the directionality arithmetic. The un_security_council is the analytical seat that sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — non-state armed attacks from territory whose host cannot or will not act — remains live (ISIS, Al-Shabaab, Houthi, and affiliated threats persist), so this is not a mandatrophy case and the constraint is not a piton: its coordination function is real and exercised. The classification work this story does is preventing two opposite mislabels. Calling it pure extraction (a snare) would erase the genuine remedy gap the narrow reading leaves open — attacked states with no answerable state and a blocked Council. Calling it pure coordination (a rope) would erase the sovereignty bypass that is the reading's actual operating cost, paid by seats with no exit. The tangled_rope claim holds both: the same structure that coordinates a response channel extracts decision authority from host states. The drift risk is one-directional: extractiveness and theater both rise over the interval while the objection bloc grows, which is the signature of a coordination function being progressively loaded with self-judged extraction rather than a mandate that has died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the article_51_self_defense kernel. How would the beneficiary/victim structure and classification shift if the narrow_armed_attack_reading or the expansive_preventive_reading were the operative reading instead of this one?',
    'Classify the sibling stories directly and compare: the narrow sibling closes the sovereignty-bypass channel (intervener-side gains collapse toward that story''s own profile); the expansive sibling removes the actual-attack trigger, pushing the structure toward harder extraction for host-state seats.',
    'Under the narrow sibling this reading''s extraction channel disappears and host states regain the attribution shield; under the expansive sibling extraction rises further and host-state seats harden toward full-target directionality. The present tangled_rope classification is reading-indexed, not kernel-invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story''s classification is indexed to the unable-unwilling reading of a contested Article 51 kernel.').

omega_variable(
    unwillingness_objectivity,
    'Can ''unwilling or unable'' be given objective, adjudicable content (capacity metrics, good-faith-effort tests, host-state consultation), or is the determination inherently self-judging by the intervening state?',
    'Survey invocation practice (the US notification letters regarding Syria from 2014 onward, Turkey''s invocations regarding Iraq and Syria) for any host-state input or third-party assessment element; track ICJ and commission-of-inquiry treatment of the standard.',
    'If objective content is possible, the constraint is reformable toward a cleaner coordination rule with adjudicated determinations; if inherently self-judging, the extraction is structural — the intervening state judges its own case — and host-state seats trend toward snare treatment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwillingness_objectivity, conceptual, 'Whether the doctrine''s core trigger is adjudicable or structurally self-judging.').

omega_variable(
    consent_conflation,
    'How much of the doctrine''s operating practice rests on genuine unwilling/unable invocation rather than on host-state consent or Security Council authorization that would support the same operations without the doctrine?',
    'Case-by-case legal analysis pairing each invocation letter with the host state''s contemporaneous statements (e.g., Iraq''s consent regarding ISIS versus Syria''s objection during the same campaign).',
    'If most practice is consent-based, the doctrine''s independent extraction is smaller than the measured ε suggests and the constraint sits closer to rope; if genuinely unilateral, the sovereignty-bypass channel is the operative engine and ε understates nothing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_conflation, empirical, 'Separating doctrine-justified operations from consent- or authorization-justified ones.').

omega_variable(
    reciprocity_asymmetry,
    'Will the doctrine''s precedent eventually be invoked against powerful states or their clients, or does the power asymmetry keep application one-directional indefinitely?',
    'Track invocations by and against major powers over the coming decade; monitor whether any powerful territorial state has the doctrine applied against it without its consent.',
    'If application stays one-directional, the constraint is a power-indexed ratchet and host-state seats drift toward snare treatment; if reciprocity emerges, the constraint stabilizes as a genuine mutual coordination rule.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_asymmetry, empirical, 'Whether the doctrine''s asymmetric application is structural or contingent.').

omega_variable(
    civilian_harm_attribution,
    'Is host-state civilian harm an incidental byproduct of otherwise legitimate force, or a structural feature of bypassing the host state''s target-filtering and territorial control?',
    'Compare civilian-casualty and displacement outcomes in consent-based operations versus unwilling/unable invocations in comparable theatres, controlling for target type.',
    'If harm tracks the bypass itself, the victim declaration for host_state_civilians is structurally load-bearing and that seat''s effective extraction is understated by the scalar ε; if incidental, the seat''s costs belong to the armed conflict generally rather than to this reading specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_harm_attribution, empirical, 'Whether civilian costs are structural to the sovereignty bypass or incidental to the force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t4, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t16, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(arti_tr_t16, observed).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t24, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(arti_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t4, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t16, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(arti_be_t16, observed).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t24, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(arti_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t4, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t16, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(arti_su_t16, observed).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t24, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(arti_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, charter_force_prohibition).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the Article 51 kernel (ε-invariance): the colloquial label 'self-defense against non-state actors' conflates three structurally distinct claims with different ε values, victim sets, and failure modes. This story is the middle reading. The narrow sibling (upstream in doctrinal pedigree — the ICJ's Nicaragua line) makes the sovereignty-bypass channel structurally impossible; this reading influences the expansive sibling by normalizing the self-judged-necessity machinery the expansive reading requires, without foreclosing either sibling — they coexist as live positions held by different state coalitions. All three readings modify the same upstream constraint (the Charter prohibition on force), which is why charter_force_prohibition appears in the edge set. Each story links to its family members; none stands alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
