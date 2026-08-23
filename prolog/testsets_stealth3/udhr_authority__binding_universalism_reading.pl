% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__binding_universalism_reading, []).

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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR Binding Universalism — Consent-Independent Justiciable Rights Enforcement Against States
 *   domain: international law/political philosophy/human rights doctrine
 *
 * SUMMARY:
 *   This story instantiates the binding_universalism_reading of the
 *   udhr_authority kernel: the claim that the Universal Declaration
 *   establishes justiciable individual rights enforceable against states
 *   regardless of whether those states consented to be bound. Under this
 *   reading the standing arrangement is a consent-independent enforcement
 *   regime — adjudicative bodies whose findings override domestic
 *   legislation, advocacy machinery that treats non-ratifying states as bound
 *   anyway, and a rights floor maintained by tribunal coercion rather than
 *   treaty consent. The ε referent is that standing arrangement, assessed by
 *   this reading's own lights: extraction is measured on state autonomy as
 *   this reading itself holds it to be extracted, not on the consent-gated
 *   arrangement the aspirational sibling would endorse. The sibling readings
 *   (aspirational_sovereignty_reading, customary_emergence_reading) are
 *   separate constraint files in the same family; the ε values differ across
 *   the family because the readings instantiate structurally distinct
 *   arrangements — consent-gated moral guidance, practice-accumulated custom,
 *   and consent-independent enforcement respectively — not because one
 *   constraint is being measured differently.
 *
 * KEY AGENTS:
 *   - international_tribunals_treaty_bodies: agenda-setter and primary recipient of adjudicative authority (institutional / identity_locked)
 *   - rights_claiming_individuals: beneficiary — protected floor recipients (powerless / trapped)
 *   - transnational_advocacy_ngos: beneficiary — leverage-platform holders (organized / mobile)
 *   - state_governments: primary target — consenting states bearing adjudicated override (institutional / trapped)
 *   - non_consenting_states: target whose consent-refusal the reading declares void (powerful / trapped)
 *   - domestic_policy_majorities: secondary target — electorates whose legislation gets reversed (organized / trapped)
 *   - sovereignty_tradition_jurists: analytical observer documenting the claim/practice gap (analytical / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.74).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.72).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR Binding Universalism — Consent-Independent Justiciable Rights Enforcement Against States").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international law/political philosophy/human rights doctrine").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, 'd9d83f07-e59c-49eb-8a67-36852e2382f3').
narrative_ontology:cs_kernel_codification('d9d83f07-e59c-49eb-8a67-36852e2382f3', fixed_text).
narrative_ontology:cs_authority_grounding('d9d83f07-e59c-49eb-8a67-36852e2382f3', extraction).
narrative_ontology:cs_interpretation_layer_present('d9d83f07-e59c-49eb-8a67-36852e2382f3').
narrative_ontology:cs_reading_relation('d9d83f07-e59c-49eb-8a67-36852e2382f3', udhr_authority__aspirational_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d9d83f07-e59c-49eb-8a67-36852e2382f3', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('d9d83f07-e59c-49eb-8a67-36852e2382f3', foundational, obligation_independent_of_state_consent).
narrative_ontology:cs_axiom_status(obligation_independent_of_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('d9d83f07-e59c-49eb-8a67-36852e2382f3', obligation_independent_of_state_consent, deontological).
narrative_ontology:cs_axiom('d9d83f07-e59c-49eb-8a67-36852e2382f3', secondary, rights_claims_justiciable_without_ratification).
narrative_ontology:cs_axiom_status(rights_claims_justiciable_without_ratification, holdable).
narrative_ontology:cs_axiom_grounding('d9d83f07-e59c-49eb-8a67-36852e2382f3', rights_claims_justiciable_without_ratification, instrumental).
narrative_ontology:cs_reference_frame('d9d83f07-e59c-49eb-8a67-36852e2382f3', udhr_as_binding_universal_charter).
narrative_ontology:cs_drift_state('d9d83f07-e59c-49eb-8a67-36852e2382f3', contemporary_sovereignty_pushback, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d9d83f07-e59c-49eb-8a67-36852e2382f3', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, rights_claiming_individuals).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, transnational_advocacy_ngos).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_tribunals_treaty_bodies).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, state_governments).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, non_consenting_states).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, domestic_policy_majorities).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, individual_rights_primacy_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, consent_independent_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regional courts and UN treaty committees receive individual complaints, issue findings on state conduct, and publish authoritative interpretations of the Declaration's provisions. Their caseload, budgets, and jurisdictional reach grow with each finding accepted as binding. Declining to adjudicate would dissolve the basis of their own standing; the adjudication role constitutes what these bodies are.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_tribunals_treaty_bodies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, international_tribunals_treaty_bodies, beneficiary).

% People subject to a state's jurisdiction who invoke declared rights in litigation, complaint procedures, or asylum claims. Protection arrives only when some tribunal accepts and acts on their claim; they generally cannot relocate away from the state whose conduct they are contesting, and their access runs through NGO intermediaries and multi-year court backlogs.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, rights_claiming_individuals, beneficiary,
    powerless, biographical, trapped, global).

% Monitoring organizations that document state conduct, file shadow reports before review bodies, litigate through allied lawyers, and mobilize diplomatic pressure by citing the Declaration. They operate across jurisdictions and can shift campaigns to whichever forum responds; the Declaration supplies their advocacy a common standard that no single government's approval controls.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, transnational_advocacy_ngos, beneficiary,
    organized, generational, mobile, global).

% National executives and legislatures that implement, contest, or comply with findings directed at their laws and practices. Periodic review, reservations management, and response to adverse findings consume sustained diplomatic resources, and adjudicated conclusions can require reversal of domestic legislation. Withdrawing from review machinery carries reputational and economic costs, and under this reading withholding consent does not lift the obligation.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, state_governments, payer,
    institutional, generational, trapped, national).

% States that never ratified the relevant covenants or lodged sweeping reservations, several of them major powers. Advocacy networks and portions of the adjudicative machinery treat the Declaration's provisions as applying to their conduct regardless. Their principal lever — refusing consent — is exactly the lever this reading declares ineffective, leaving reputational pressure and bilateral friction as their remaining responses.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_consenting_states, payer,
    powerful, generational, trapped, national).

% Electoral majorities whose governments legislate on matters such as speech regulation, family policy, or immigration, and who then see adjudicated findings require reversal of those laws. They can change governments but not adjudicated outcomes; the channel from their votes to final policy passes through bodies they did not elect and cannot dismiss.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, domestic_policy_majorities, payer,
    organized, biographical, trapped, national).

% Legal scholars in the consent-based tradition who document the distance between the enforcement claim and observable treaty practice, reconstruct the doctrinal history of consent-independence, and supply the counter-arguments states cite in review forums. They hold no enforcement role and bear no compliance burden.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, sovereignty_tradition_jurists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__binding_universalism_reading, international_tribunals_treaty_bodies).
narrative_ontology:fixing_cost_class(udhr_authority__binding_universalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a uniform floor of individual protections against state abuse, addressing the collective-action problem in which every state faces internal incentives to violate the rights of dissidents, minorities, and foreigners whenever no external check exists.
% TRANSFER_FUNCTION: Moves decision authority over domestic governance from state institutions (executives, legislatures, electorates) to international adjudicative bodies and rights-claiming individuals, and moves compliance and reversal costs onto state budgets and domestic policy agendas.
% ABSENT_VOICES: Non-consenting states and domestic policy majorities are structurally absent from the conversations that produce authoritative findings: under this reading their objection is definitionally disqualified, since consent is declared irrelevant to obligation. Populations of states whose development priorities collide with rights conditionalities are similarly represented only through advocacy intermediaries.
% DISAPPEARANCE_RATIONALE: If consent-independent enforcement vanished overnight, adjudicative bodies would lose jurisdiction over unwilling states, advocacy leverage would collapse to voluntary persuasion, and rights implementation would fall back entirely to domestic politics and consent-gated treaty compliance; states would regain unrestricted discretion over the policy domains adjudication currently reaches.
% FOUNDING_PROBLEM: The interwar failure and the Second World War demonstrated that sovereign states could murder, deport, and persecute their own populations with no external check whatsoever; the arrangement was built to create an external limit on how states treat the individuals under their jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested well outside the benefiting parties: the Nuremberg trial record, postwar diplomatic archives of the Declaration's drafting, continuous documentation of mass atrocities by journalists and historians, and — notably — sovereignty-tradition jurists who dispute the consent-independent remedy while conceding the underlying problem is real and unresolved.
narrative_ontology:disappearance_verdict(udhr_authority__binding_universalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__binding_universalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__binding_universalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__binding_universalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__binding_universalism_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__binding_universalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__binding_universalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74) because consent-independence decouples obligation from acceptance: states bear compliance, reversal, and reputational costs they never agreed to, and the reading's defining move is to nullify the one exit lever (withholding consent) states otherwise hold. Suppression (0.72) reflects active enforcement machinery — courts issuing executable judgments, treaty-body review cycles, aid conditionality — overriding domestic decisions rather than resting on participant preference; the suppression_requirement series is authored deliberately because enforcement-capacity build-up is the dynamic this story traces (the reading's entire structural delta is tribunals gaining coercive authority). Theater (0.35) is moderate: individual-petition adjudication and court judgments with real consequences sit alongside ritualized reporting cycles and review sessions whose recommendations are routinely ignored. Accessibility_collapse (0.62) is elevated but not mountain-grade: the obvious alternative (refuse consent) is nullified by the reading itself, yet federal arrangements, treaty reservations, derogations, and selective compliance remain partial refuges. Resistance (0.70) is high and persistent: non-ratification by major powers, sweeping reservations, withdrawal from review bodies, and sovereignty-coalition pushback. The claimed type (tangled_rope) is authored from structure — a genuine protective coordination function operating through the same machinery that transfers decision authority away from states — independently of these metric values; the engine computes per-seat types from the structural data. All three series share one time grid (1948–2025, seven points) so no metric row is sampled against another metric's scalar substitution.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp and structural. From the adjudicative seat, the arrangement is the coordination it administers: its caseload is the rights floor functioning, and its growth tracks protection delivered. From the state seats, the same machinery is adjudicated override of self-government: findings reverse legislation electorates chose, and the review cycle consumes diplomatic resources indefinitely. The lateral same-level contrast is the sharpest divergence in the story: state_governments and non_consenting_states hold identical nominal standing (sovereign equals under the UN Charter) yet face different exit structures — consenting states entered the machinery knowingly and carry conventional withdrawal costs, while non-consenting states have their exit lever (consent-refusal) declared void by this very reading, making them the paradigmatic trapped targets despite greater raw power. The identity-lock on the adjudicative bodies compounds the gap: their institutional self-concept is constituted by adjudication, so jurisdictional retrenchment is experienced as self-annihilation rather than correction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the adjudicative complex, rights-claiming individuals, and advocacy networks near the beneficiary end of directionality: the tribunal bodies collect authority, jurisdiction, and budget from each cycle of enforcement; individuals receive the protective floor; NGOs convert the Declaration into cross-jurisdictional leverage. Victim declarations place state_governments, non_consenting_states, and domestic_policy_majorities near the target end: they bear compliance costs, overridden legislation, and obligations they refused, with exit options ranging from costly (consenting states) to structurally void (non-consenting states, by the reading's own premise). Trapped exit positioning keeps the state seats near the full-target end rather than allowing mobility to damp their effective extraction. No directionality overrides are authored: the beneficiary/victim declarations plus exit data determine each seat's position cleanly, and the derivation chain needs no correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an external check on state treatment of individuals — remains live and is corroborated from outside the benefiting parties, so no mandatrophy resolution is declared. The tangled_rope classification guards both mislabeling directions. Reading the arrangement as pure extraction (snare) would erase the demonstrable protective function: individuals do obtain remedies through adjudication that domestic channels denied them, and the rights floor measurably constrains state conduct in reviewed domains. Reading it as pure coordination (rope) would erase the consent-independent transfer of decision authority to unelected adjudicators — the asymmetry that distinguishes this reading from its consent-gated siblings. The rising theater_ratio after 2005 is the early-warning signature worth monitoring: if the founding problem were ever genuinely resolved (states reliably self-limiting), continued enforcement cycling would drift toward theatrical maintenance of a solved mandate, and the classification should then migrate toward piton. The current data show the opposite of resolution — enforcement reach and state resistance both climbing — which is why the mandate is scored live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_udhr_authority,
    'This constraint instantiates the binding_universalism_reading of kernel udhr_authority; how would the classification shift if the arrangement were instead read through the sibling readings?',
    'Compare computed classifications across the three family stories: aspirational_sovereignty_reading (consent-gated, no coercive enforcement) and customary_emergence_reading (practice-accumulated, consent-mediated binding) against this story''s consent-independent enforcement profile.',
    'Under the aspirational sibling, extraction on state autonomy collapses toward zero and the arrangement reads as voluntary moral coordination; under the customary sibling, extraction is consent-mediated and moderate, with non_consenting_states dropping out of the victim set entirely. Only this reading authorizes tribunal coercion over states that refused consent — the disagreement is located precisely in whether consent conditions obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_udhr_authority, conceptual, 'Committer structure: this story is one of three readings of the UDHR-authority kernel; sibling readings instantiate structurally distinct constraints with different epsilon and victim sets.').

omega_variable(
    consent_independence_enforcement_reality,
    'Does enforcement actually reach states that withheld consent, or does consent-independence hold only where states have in fact submitted to review machinery?',
    'Track adjudicative outcomes against non-ratifying and reservation-heavy states: count findings issued, compliance induced, and concrete consequences imposed where no treaty relationship exists, over a multi-decade window.',
    'If enforcement never bites without prior submission, the reading''s distinctive extraction on non_consenting_states is nominal; effective extraction concentrates on consenting states and the story''s structure converges toward the customary sibling''s consent-mediated profile, lowering measured epsilon on the consent-independent margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_independence_enforcement_reality, empirical, 'Gap between the consent-independent claim and the consent-dependent reality of enforcement reach.').

omega_variable(
    authority_ground_lineage_vs_expansion,
    'Is the adjudicative complex''s authority grounded in the Declaration''s founding moral lineage, or does it expand by denying states any revision route — drift denial as the source of authority?',
    'Code tribunal and treaty-body legitimation statements across decades: do they invoke founding-moment continuity and principled jurisdictional limits, or jurisdiction-maximizing interpretations that systematically foreclose state opt-outs and consent-based corrections?',
    'Lineage-grounded authority supports the genuine coordination half of the tangled-rope reading and stabilizes the current classification; expansion-grounded authority raises effective extraction on all state seats and pushes the arrangement toward capture-flavored operation in which the adjudicative complex is the primary rent collector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_ground_lineage_vs_expansion, conceptual, 'Whether adjudicator legitimacy is inherited from the founding tradition or self-expanding through jurisdiction maximization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__binding_universalism_reading, theater_ratio, 1948, 0.55).
narrative_ontology:measurement_basis(udhr_tr_t1948, observed).
narrative_ontology:measurement(udhr_tr_t1960, udhr_authority__binding_universalism_reading, theater_ratio, 1960, 0.5).
narrative_ontology:measurement_basis(udhr_tr_t1960, observed).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__binding_universalism_reading, theater_ratio, 1966, 0.44).
narrative_ontology:measurement_basis(udhr_tr_t1966, observed).
narrative_ontology:measurement(udhr_tr_t1976, udhr_authority__binding_universalism_reading, theater_ratio, 1976, 0.34).
narrative_ontology:measurement_basis(udhr_tr_t1976, observed).
narrative_ontology:measurement(udhr_tr_t1990, udhr_authority__binding_universalism_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement_basis(udhr_tr_t1990, observed).
narrative_ontology:measurement(udhr_tr_t2005, udhr_authority__binding_universalism_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement_basis(udhr_tr_t2005, observed).
narrative_ontology:measurement(udhr_tr_t2025, udhr_authority__binding_universalism_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(udhr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__binding_universalism_reading, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement_basis(udhr_be_t1948, observed).
narrative_ontology:measurement(udhr_be_t1960, udhr_authority__binding_universalism_reading, base_extractiveness, 1960, 0.31).
narrative_ontology:measurement_basis(udhr_be_t1960, observed).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__binding_universalism_reading, base_extractiveness, 1966, 0.38).
narrative_ontology:measurement_basis(udhr_be_t1966, observed).
narrative_ontology:measurement(udhr_be_t1976, udhr_authority__binding_universalism_reading, base_extractiveness, 1976, 0.46).
narrative_ontology:measurement_basis(udhr_be_t1976, observed).
narrative_ontology:measurement(udhr_be_t1990, udhr_authority__binding_universalism_reading, base_extractiveness, 1990, 0.57).
narrative_ontology:measurement_basis(udhr_be_t1990, observed).
narrative_ontology:measurement(udhr_be_t2005, udhr_authority__binding_universalism_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement_basis(udhr_be_t2005, observed).
narrative_ontology:measurement(udhr_be_t2025, udhr_authority__binding_universalism_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(udhr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__binding_universalism_reading, suppression_requirement, 1948, 0.14).
narrative_ontology:measurement_basis(udhr_su_t1948, observed).
narrative_ontology:measurement(udhr_su_t1960, udhr_authority__binding_universalism_reading, suppression_requirement, 1960, 0.21).
narrative_ontology:measurement_basis(udhr_su_t1960, observed).
narrative_ontology:measurement(udhr_su_t1966, udhr_authority__binding_universalism_reading, suppression_requirement, 1966, 0.3).
narrative_ontology:measurement_basis(udhr_su_t1966, observed).
narrative_ontology:measurement(udhr_su_t1976, udhr_authority__binding_universalism_reading, suppression_requirement, 1976, 0.43).
narrative_ontology:measurement_basis(udhr_su_t1976, observed).
narrative_ontology:measurement(udhr_su_t1990, udhr_authority__binding_universalism_reading, suppression_requirement, 1990, 0.56).
narrative_ontology:measurement_basis(udhr_su_t1990, observed).
narrative_ontology:measurement(udhr_su_t2005, udhr_authority__binding_universalism_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement_basis(udhr_su_t2005, observed).
narrative_ontology:measurement(udhr_su_t2025, udhr_authority__binding_universalism_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(udhr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__binding_universalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__customary_emergence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the authority of the UDHR' decomposes into three structurally distinct claims per the epsilon-invariance principle. aspirational_sovereignty_reading authors the consent-gated arrangement (near-zero coercive extraction on states; no enforcement machinery). customary_emergence_reading authors the practice-accumulated arrangement (moderate, consent-mediated extraction; bindingness earned through state practice and opinio juris). This story authors the consent-independent enforcement arrangement (high extraction on state autonomy; enforcement reaches — or claims to reach — non-consenting states). The upstream/downstream gradient runs aspirational -> customary -> binding: each later claim cites the earlier one's moral or evidentiary base, and this reading's adjudicative output feeds the customary reading's state-practice evidence. Linkage via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
