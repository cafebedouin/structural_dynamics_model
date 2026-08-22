% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria as Self-Executing Statehood Test (Declaratory Reading)
 *   domain: legal/political
 *
 * SUMMARY:
 *   The declaratory reading of the Montevideo kernel holds that an entity
 *   possessing a permanent population, a defined territory, an effective
 *   government, and capacity for foreign relations is a state as a matter of
 *   legal fact — recognition declares what already exists and cannot withhold
 *   what the criteria establish (Montevideo Art. 3). This file authors that
 *   one reading as a clean, epsilon-invariant constraint; the constitutive
 *   and hybrid readings are separate stories with their own victim sets and
 *   are not averaged in. The epsilon referent is the declaratory doctrine as
 *   the standing arrangement under contest, assessed by the reading's own
 *   lights — not the arrangement any sibling would install. Family epsilon
 *   differences: the constitutive reading authors epsilon over
 *   recognition-gated statehood, where de facto authorities are the victim
 *   class and the existing community holds the gate; the hybrid reading
 *   authors epsilon over criteria-plus-legitimacy, where illiberal
 *   criteria-meeters enter the victim set; this reading's epsilon is authored
 *   over criteria-sufficiency itself, whose extraction surface is the
 *   doctrine-practice gap and the criteria's gameability. Structurally the
 *   doctrine coordinates the system around an objective membership test while
 *   degrading parent-state claims, dumping the cost of its own non-execution
 *   on the weakest seats, and offering great powers a laundering instrument.
 *   The claim/metric independence rule is observed: claimed_type is
 *   tangled_rope (genuine coordination plus asymmetric costs plus active
 *   doctrinal maintenance) and the metrics describe actual operation; where
 *   the engine's per-seat computation diverges from the claim, that
 *   divergence is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - de_facto_criteria_meeting_entities: Dual-positioned seat (beneficiary/payer) — holds the doctrine's legal promise as its principal diplomatic asset while bearing the doctrine-practice gap (moderate/trapped)
 *   - - populations_of_recognition_denied_entities: Primary target among the weak — lives the gap as travel, finance, and status limbo (powerless/trapped)
 *   - - parent_states_of_secessionist_entities: Payer — loses recognition leverage and claim-control once criteria are met; exit identity-locked (institutional/identity_locked)
 *   - - existing_state_community: Beneficiary — collects legal certainty and transactional flexibility; its non-recognition creates no obligations (institutional/constrained)
 *   - - great_power_sponsors: Beneficiary via manipulation — sponsors criteria-compliant client entities and launders domination (powerful/arbitrage)
 *   - - stateless_nations_failing_criteria: Payer and structurally excluded — dispositive objectivity locks them out entirely (powerless/trapped)
 *   - - newly_decolonized_states: Historical beneficiary bloc — entered through the criteria admission machine of the 1960s-70s (organized/constrained)
 *   - - international_courts_and_arbitrators: Agenda-setter — the seat where the doctrine is actually operative (institutional/analytical)
 *   - - international_law_scholarship: Analytical observer — maintains, repairs, and contests the doctrine (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.55).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.55).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria as Self-Executing Statehood Test (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "legal/political").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '3fe2964d-1732-464c-9b1a-c98131a192b4').
narrative_ontology:cs_kernel_codification('3fe2964d-1732-464c-9b1a-c98131a192b4', formalized).
narrative_ontology:cs_authority_grounding('3fe2964d-1732-464c-9b1a-c98131a192b4', expertise).
narrative_ontology:cs_interpretation_layer_present('3fe2964d-1732-464c-9b1a-c98131a192b4').
narrative_ontology:cs_reading_relation('3fe2964d-1732-464c-9b1a-c98131a192b4', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('3fe2964d-1732-464c-9b1a-c98131a192b4', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('3fe2964d-1732-464c-9b1a-c98131a192b4', foundational, criteria_sufficiency_establishes_statehood).
narrative_ontology:cs_axiom_status(criteria_sufficiency_establishes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('3fe2964d-1732-464c-9b1a-c98131a192b4', criteria_sufficiency_establishes_statehood, conventional).
narrative_ontology:cs_axiom('3fe2964d-1732-464c-9b1a-c98131a192b4', foundational, recognition_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('3fe2964d-1732-464c-9b1a-c98131a192b4', recognition_declaratory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('3fe2964d-1732-464c-9b1a-c98131a192b4', objective_criteria_self_executing_test).
narrative_ontology:cs_drift_state('3fe2964d-1732-464c-9b1a-c98131a192b4', contemporary_recognition_politics, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3fe2964d-1732-464c-9b1a-c98131a192b4', '2026-08-04T00:00:00Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_criteria_meeting_entities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, existing_state_community).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, newly_decolonized_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states_of_secessionist_entities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, populations_of_recognition_denied_entities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, stateless_nations_failing_criteria).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, great_power_sponsors).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, de_facto_criteria_meeting_entities).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, declaratory_theory_of_recognition).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, effectiveness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern territories with permanent populations, defined borders, and functioning institutions — Somaliland, Taiwan, and the post-Soviet de facto states are the recurring cases. They hold the four-criteria test as their principal legal argument: their diplomatic case is that they already satisfy it. What flows to them is a legal fact on paper; what fails to flow is everything recognition delivers — UN seats, finance access, treaty networks. Exit from the status question is not available to them: the question is their existence. Their position is genuinely dual: the doctrine is their strongest asset and its non-execution is their standing injury.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_criteria_meeting_entities, beneficiary,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, de_facto_criteria_meeting_entities, payer).

% Live under governments that meet the criteria while their passports, currencies, and trade are not accepted abroad. They bear the gap between the legal declaration and the delivered status: travel barriers, exclusion from international finance, and a generation-long limbo in which their state is simultaneously a legal fact and a practical non-entity. Their exit is emigration — individually available, collectively corrosive.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, populations_of_recognition_denied_entities, payer,
    powerless, biographical, trapped, regional).

% States whose constitutional identity and territorial claims run through territory now controlled by criteria-satisfying de facto authorities — Serbia and Kosovo, Azerbaijan and the former Artsakh, Somalia and Somaliland, Cyprus and the north, China and Taiwan. Once an entity satisfies the criteria, their recognition leverage — the ability to condition relations, membership, and legitimacy on concessions — is legally degraded. Their claim is fused with national identity: abandoning it is constitutionally and politically unavailable, so they litigate, blockade, and veto admission instead.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states_of_secessionist_entities, payer,
    institutional, generational, identity_locked, national).

% The established membership of the international system. It collects the arrangement's working surplus: a predictable test it can apply in its courts without political commitment, the ability to transact with contested entities without extending recognition, and freedom from any obligation created by its own non-recognition. Its exit from the system of which it is the constitution is not meaningful; its costs are the occasional obligation to explain why law and practice diverge.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_state_community, beneficiary,
    institutional, civilizational, constrained, global).

% The admission wave of the 1960s and 1970s entered through the criteria-plus-effectiveness route without needing each great power's blessing; the doctrine was the machine that turned colonial administrative boundaries into member states. Acting as a bloc, they continue to defend the declaratory position because it remains the small state's shield against recognition leverage.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, newly_decolonized_states, beneficiary,
    organized, generational, constrained, global).

% Powers that manufacture criteria compliance for client entities — South Africa's Bantustans, Turkey's sponsorship of the north Cyprus entity, Russia's 2014 and 2022 creations. The criteria's objectivity is their instrument: a sponsored entity that fields a population, a territory, and an administration acquires a legal argument no political objection can answer head-on. Their arbitrage is selective invocation — declaratory criteria when sponsoring, constitutive non-recognition when opposing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, great_power_sponsors, beneficiary,
    powerful, generational, arbitrage, global).

% Nations without a state — Kurdish, Sahrawi, and dozens of smaller cases — that fail one or more criteria (defined territory, effective government) and are therefore locked out entirely: under the dispositive test, no amount of justice in their claim or legitimacy in their movement matters. They are not parties to the doctrine's framework; the test is administered about them and over them. Exit is the generation-long project of manufacturing criteria compliance under conditions designed to prevent it.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, stateless_nations_failing_criteria, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, stateless_nations_failing_criteria, excluded).

% ICJ chambers, domestic courts, and arbitral tribunals apply the four-criteria test wherever statehood is a premise — immunity, act of state, succession, treaty interpretation. They are the seat where the doctrine actually operates, and they administer it while documenting its gap with political practice. They apply the test; they neither collect from it nor bear it.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_courts_and_arbitrators, agenda_setter,
    institutional, civilizational, analytical, global).

% The interpretive community that maintains, defends, and contests the declaratory position. It produces the doctrine's repair work — effectiveness caveats, non-recognition duties, ex injuria exceptions — and its self-critique. Its stake is professional: careers and schools of thought are built both on defending the doctrine and on exposing its gap with practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, existing_state_community).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single objective test — permanent population, defined territory, effective government, capacity for foreign relations — that any court, chancery, or third party can apply to determine legal statehood, so that treaty capacity, succession, immunity, and adjudication do not require case-by-case consensus of the existing community of states.
% TRANSFER_FUNCTION: Moves legal standing — the capacity to hold state rights and duties, and the strength of territorial claims — out of the existing community's discretionary gift and into entities satisfying the criteria; correspondingly removes parent states' ability to withhold that standing as bargaining leverage. A second, less advertised transfer runs through the doctrine-practice gap: the costs of the doctrine's non-execution (limbo, exclusion from institutions and finance) fall on criteria-meeting entities and their populations, while the flexibility of dealing with them without recognizing them accrues to the established community.
% ABSENT_VOICES: Stateless nations that fail one or more criteria — they are the objects of the test and had no seat in its codification or its maintenance; their objection (that dispositive objectivity entrenches the map at the moment of measurement) is heard nowhere in the doctrine's own framework. The populations of recognition-denied entities likewise sit outside the conversation in which their status is adjudicated. Parent-state constitutional claims are overridden by a doctrine most of them never ratified (the United States never ratified Montevideo).
% DISAPPEARANCE_RATIONALE: Adjudication would rearrange immediately: domestic and international courts lose their workable test for statehood-dependent questions (immunity, act of state, succession, treaty capacity) and revert to case-by-case recognition politics; the decolonization-era precedent structure loses its doctrinal foundation. Geopolitics would move less — recognition politics already runs much of the practice — which is exactly the doctrine-practice gap the theater series tracks. The parties dispute how much would rearrange; the adjudicative rearrangement alone satisfies the verdict.
% FOUNDING_PROBLEM: Interwar recognition chaos: recognition wielded as a political weapon — non-recognition of the USSR, the Stimson doctrine, great powers conditioning recognition on concessions — left new entities' legal status hostage to great-power discretion and left courts without a workable statehood test. The declaratory theory, codified at Montevideo in 1933 (Art. 3), was built to make statehood a matter of fact rather than favor.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by ICJ jurisprudence (the Kosovo Advisory Opinion's canvass of the declaratory position), by the comparative scholarly literature across jurisdictions, and — most tellingly — by the maneuvering of the great powers themselves, who invoke criteria satisfaction when sponsoring clients and constitutive non-recognition when opposing rivals: parties who lose from the doctrine attest the problem it solves by how carefully they route around it. No great-power chancery endorses the doctrine uniformly; the attestation is behavioral, not declaratory.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.55 reflects three channels: parent-state claim degradation (the doctrine working as designed, but a real transfer of claim-control), great-power laundering of client entities through gameable criteria (Bantustans to the 2022 creations), and the doctrine-practice gap that strips criteria-meeting entities of the doctrine's fruits while its existence is invoked to deny them alternative remedies. Suppression 0.55 is doctrinal foreclosure rather than physical coercion: within this reading's framework, recognition deals cannot purchase statehood and legitimacy claims cannot supplement the criteria, and the lockout is absolute for criteria-failing nations; note the coercive machinery that surrounds the doctrine (embargos on non-recognized entities, admission vetoes) is mostly the community's constitutive practice resisting this doctrine — resistance, not suppression. Theater 0.44: the doctrine is ceremonially affirmed everywhere and operationally decisive mainly in courts, while geopolitical statehood decisions run on recognition politics. Accessibility collapse 0.38: the alternatives (constitutive practice, hybrid legitimacy screens) remain fully live — the declaratory claim to exclusivity is precisely what practice declines to grant. Resistance 0.62: parent-state resistance is coalition-organized (EU non-recognition of the north Cyprus entity, UN admission vetoes, the non-recognition-duty apparatus), which is why resistance is high despite the payers' institutional standing: they can act together, and do. The temporal series runs on one shared grid (1933-2025, eight points, every tracked metric authored at every point). Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the doctrine required less active defense during decolonization (history pushed its way — the 1960 dip) and progressively more after 1975 as manipulation cases forced effectiveness caveats, non-recognition duties, and ex injuria overrides — a hardening maintenance burden ending at the base scalar.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit four different arrangements under the same doctrine. The parent-state seat experiences expropriation: a legal fact extinguishing claims held for generations, with identity-fused exit. The de facto seat experiences an unkept promise: law declares its members states; the system declines to deliver. The established community experiences convenience: legal certainty and flexibility without obligation, which is why the gains pool there. The great-power seat experiences a tool: criteria that can be manufactured. Per-seat classification is the engine's computation from these structural positions; the authored tangled_rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the existing community collects the coordination surplus and the flexibility of non-recognition; newly decolonized states collected admission without unanimous consent; de facto entities hold the doctrine's promise as their principal legal asset. Victim declarations: parent states bear claim-degradation with identity-locked exit; recognition-denied populations bear the gap's costs with no exit but emigration; criteria-failing nations bear the lockout that dispositive objectivity creates. One directionality override: the structural derivation would place the moderate de facto entity seat near the beneficiary end (roughly 0.2) from its beneficiary role alone, but its benefit is systematically discounted by the practice gap — the legal fact it gains is the very fact practice strips of effect — leaving it slightly net-benefiting at d = 0.35. Suppression is authored as a raw structural property and is not scaled by power or scope; only extraction is scaled, by directionality and by the doctrine's universal claimed scope, which amplifies effective extraction at the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine is routinely presented as settled, obvious law — 'statehood is a question of fact' — a framing that invites false-summit treatment: a constructed 1933 treaty doctrine with identifiable beneficiaries, payers, and a manipulation channel is not a natural feature of the international system, which is why claimed_type is tangled_rope rather than mountain and emerges_naturally is false. Equally, mislabeling it as pure extraction would erase the genuine coordination function (objective standard, adjudicability, decolonization-era enablement) on which the asymmetric costs ride. No mandatrophy resolution: the founding problem (recognition wielded as a weapon) is live — the 2022 client-entity recognitions and the Western non-recognition apparatus are the founding problem recurring — so mandatrophy_resolved is not declared. The rising theater series tracks the doctrine's form degrading while its mandate persists: the mismatch consumer reads founding_problem_status=live against world_rearranges and should find no zombie flag, consistent with the computed path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_fourth_element_circularity,
    'Is the fourth Montevideo criterion (capacity to enter into relations with other states) an independent factual test, or does it presuppose the sovereignty and recognition it is supposed to be independent of?',
    'Doctrinal analysis of adjudicated applications in which criterion 4 was assessed without reference to recognition practice; comparative court treatment of entities with effective governments but no admitted foreign-relations capacity.',
    'If criterion 4 smuggles recognition back in, the declaratory reading collapses toward the constitutive one at the decisive margin, the victim set expands (criteria-meeting entities become recognition-dependent after all), and this story''s epsilon understates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_fourth_element_circularity, conceptual, 'Whether the criteria test is fully objective or recognition-laden at the fourth element.').

omega_variable(
    doctrine_practice_gap_attribution,
    'Is the measured extraction a property of the declaratory doctrine itself, or of the constitutive practice that systematically overrides it — and would full self-execution raise extraction (by unblocking the manipulation channel) or lower it (by relieving the gap victims)?',
    'Counterfactual structural comparison: model the doctrine operating without constitutive override (no criteria-satisfying entity ever denied) and re-price each extraction channel; use historical episodes where criteria satisfaction was decisive (Bangladesh 1971-74) as partial natural experiments.',
    'If full self-execution would raise net extraction through the manipulation channel, the doctrine''s coordination component shrinks and the reading computes closer to pure extraction at sponsor seats; if it would lower extraction, the gap is the culprit and the doctrine is partially vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_gap_attribution, conceptual, 'Whether extraction belongs to the doctrine or to its non-execution in practice.').

omega_variable(
    client_entity_manipulation_prevalence,
    'What share of criteria-satisfying new entities since 1933 were sponsored manufactures (the Bantustans, the north Cyprus entity, the 2014 and 2022 Russian creations) rather than organic effectiveness — how dominant is the great-power laundering channel?',
    'Comparative dataset of post-1933 criteria-satisfying entities coded for sponsor involvement, durability of the criteria satisfaction, and recognition outcome.',
    'If manufactured entities are a large share, the criteria function substantially as a laundering instrument and effective extraction rises sharply at great-power seats; if rare, the coordination function dominates the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(client_entity_manipulation_prevalence, empirical, 'Prevalence of sponsored criteria-compliant entities versus organically effective ones.').

omega_variable(
    lockout_attribution_kernel_vs_reading,
    'Does the lockout of criteria-failing nations belong to the kernel (the four criteria themselves) or to this reading (their dispositive sufficiency) — would the constitutive or hybrid sibling open a discretionary route this reading forecloses?',
    'Compare the sibling readings'' treatment of criteria-failing peoples: constitutive recognition politics and hybrid legitimacy screens both leave discretionary routes that the sufficiency axiom closes; assess whether any such route has ever actually admitted a criteria-failing nation.',
    'If the lockout is reading-specific, it counts against this reading''s cost balance; if kernel-owned, all readings share it and it should not differentiate the declaratory seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lockout_attribution_kernel_vs_reading, conceptual, 'Whether the criteria-failing lockout is owned by the kernel or by this reading''s sufficiency axiom.').

omega_variable(
    decolonization_credit_attribution,
    'Was the decolonization-era admission wave driven by the declaratory doctrine''s criteria machine, or by great-power consensus that the constitutive machinery would have delivered regardless?',
    'Historical process-tracing of 1960s-70s admissions: cases where criteria satisfaction preceded and forced consensus versus cases where consensus preceded and the criteria ratified it.',
    'If consensus was doing the work, the doctrine''s coordination credit falls, its rope-component shrinks, and the historical beneficiary claim of the newly decolonized bloc weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decolonization_credit_attribution, empirical, 'Whether decolonization admissions credit the doctrine or the consensus of the moment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_declaratory_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.2).
narrative_ontology:measurement(montevideo_declaratory_tr_t1945, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(montevideo_declaratory_tr_t1960, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(montevideo_declaratory_tr_t1975, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(montevideo_declaratory_tr_t1990, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(montevideo_declaratory_tr_t2005, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(montevideo_declaratory_tr_t2020, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(montevideo_declaratory_tr_t2025, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(montevideo_declaratory_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.32).
narrative_ontology:measurement(montevideo_declaratory_be_t1945, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1945, 0.36).
narrative_ontology:measurement(montevideo_declaratory_be_t1960, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(montevideo_declaratory_be_t1975, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(montevideo_declaratory_be_t1990, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(montevideo_declaratory_be_t2005, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(montevideo_declaratory_be_t2020, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(montevideo_declaratory_be_t2025, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_declaratory_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.3).
narrative_ontology:measurement(montevideo_declaratory_su_t1945, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(montevideo_declaratory_su_t1960, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(montevideo_declaratory_su_t1975, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(montevideo_declaratory_su_t1990, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(montevideo_declaratory_su_t2005, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(montevideo_declaratory_su_t2020, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(montevideo_declaratory_su_t2025, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'the Montevideo statehood criteria' is one kernel instantiating three structurally distinct constraints. This file is the declaratory reading (criteria sufficient, self-executing); the constitutive reading (recognition constitutes statehood; de facto authorities as victim class, the existing community holding the gate) and the hybrid reading (criteria plus normative legitimacy; illiberal criteria-meeters entering the victim set) are separate stories with their own epsilon, beneficiaries, and victims. The readings share the criteria text but author different epsilon because the extraction surface differs: recognition-gating for the constitutive reading, legitimacy-screening for the hybrid, and the doctrine-practice gap plus criteria gameability for this one. Edges are declared in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
