% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Settlement: Religious Differentiation as Sovereignty Instrument
 *   domain: historical/political-economic/religious
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the reformation_composite kernel:
 *   the political_realignment_reading, on which the Reformation's operative
 *   constraint is the arrangement by which emerging territorial states used
 *   religious differentiation to assert sovereignty against imperial and
 *   papal authority. Its primary observable is cuius regio eius religio and
 *   its successor settlements (Augsburg 1555, Westphalia 1648). Per the
 *   epsilon-invariance principle, the colloquial label 'the Reformation'
 *   covers multiple structurally distinct claims; the
 *   theological-fragmentation and technological-mediation readings are
 *   SEPARATE constraint stories with their own epsilon values, beneficiary
 *   sets, and classifications, linked to this one through
 *   network.affects_constraints. This file authors epsilon ONLY for the
 *   sovereignty-assertion arrangement as this reading sees it: a structure
 *   that genuinely coordinates confessional coexistence while transferring
 *   jurisdiction, property, and revenue from supranational authorities to
 *   dynastic holders. KEY AGENTS (by structural relationship): -
 *   territorial_rulers: Primary beneficiary and agenda-setter
 *   (institutional/arbitrage) — set, enforce, and collect from the settlement
 *   - papal_authority: Primary target (institutional/trapped) — loses
 *   jurisdiction and revenue, cannot exit its own claim - imperial_authority:
 *   Secondary target (institutional/constrained) — loses confessional command
 *   inside its own constitution - dissenting_subjects: Diffuse targets
 *   (powerless/constrained) — bear conformity, visitation, and expulsion -
 *   secularized_clergy: Institutional target (organized/identity_locked) —
 *   lose institution and vocation together - radical_congregationalists:
 *   Excluded voice (powerless/trapped) — tolerationist alternative kept out
 *   by the same machinery - westphalian_congress_mediators: Analytical
 *   observer (institutional/analytical) — see both strands at the drafting
 *   table
 *
 * KEY AGENTS:
 *   - territorial_rulers: Primary beneficiary/agenda-setter (institutional/arbitrage) — declare and enforce territorial confession, secularize church property, collect diverted revenue
 *   - papal_authority: Primary target (institutional/trapped) — loses appeals, annates, and appointment leverage; refuses to ratify its losses
 *   - imperial_authority: Secondary target (institutional/constrained) — formalized princely right of resistance ends unified confessional command
 *   - dissenting_subjects: Diffuse target (powerless/constrained) — conform-or-emigrate obligation, visitations, expulsions
 *   - secularized_clergy: Institutional target (organized/identity_locked) — dissolved houses, annexed estates, vow-versus-conformity bind
 *   - radical_congregationalists: Excluded voice (powerless/trapped) — persecuted by both blocs, absent from both settlements
 *   - westphalian_congress_mediators: Analytical observer (institutional/analytical) — codify the separation of dynastic interest from confessional settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.72).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.7).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Settlement: Religious Differentiation as Sovereignty Instrument").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical/political-economic/religious").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'c9cb2406-0c0d-4943-be62-837be94457a0').
narrative_ontology:cs_kernel_codification('c9cb2406-0c0d-4943-be62-837be94457a0', formalized).
narrative_ontology:cs_authority_grounding('c9cb2406-0c0d-4943-be62-837be94457a0', extraction).
narrative_ontology:cs_interpretation_layer_present('c9cb2406-0c0d-4943-be62-837be94457a0').
narrative_ontology:cs_reading_relation('c9cb2406-0c0d-4943-be62-837be94457a0', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9cb2406-0c0d-4943-be62-837be94457a0', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('c9cb2406-0c0d-4943-be62-837be94457a0', foundational, religious_differentiation_is_sovereignty_instrument).
narrative_ontology:cs_axiom_status(religious_differentiation_is_sovereignty_instrument, holdable).
narrative_ontology:cs_axiom_grounding('c9cb2406-0c0d-4943-be62-837be94457a0', religious_differentiation_is_sovereignty_instrument, empirically_contingent).
narrative_ontology:cs_axiom('c9cb2406-0c0d-4943-be62-837be94457a0', secondary, ecclesiastical_property_follows_territorial_jurisdiction).
narrative_ontology:cs_axiom_status(ecclesiastical_property_follows_territorial_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('c9cb2406-0c0d-4943-be62-837be94457a0', ecclesiastical_property_follows_territorial_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('c9cb2406-0c0d-4943-be62-837be94457a0', integrated_imperial_christendom).
narrative_ontology:cs_drift_state('c9cb2406-0c0d-4943-be62-837be94457a0', post_westphalia_settlement, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('c9cb2406-0c0d-4943-be62-837be94457a0', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, dissenting_subjects).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, secularized_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elect, announce, and enforce a confession for their territory: issue church ordinances, fund consistories and visitations, appoint clergy, and collect revenues (annates, first-fruits, dispensation fees, tithe flows) that previously went to Rome or through imperial channels. Secularize monastic and episcopal property into dynastic holdings. Because they are the rule-makers, their exit is arbitrage: houses shift confession when alliance or inheritance calculus favors it (Palatine conversions, Saxon realignments) without leaving the arrangement.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_rulers, beneficiary).

% Loses jurisdictional appeals, curial revenue, and appointment leverage across northern and central Europe as territories declare confessional independence. Cannot exit its own claim to universal jurisdiction without dissolving what it is; responds with counter-reformation machinery, nuncio diplomacy, and doctrinal definition, and ultimately refuses signature on the settlement that codifies its losses.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% The emperor loses the ability to command a unified confessional policy inside the empire: the settlement formalizes a princely right of resistance and freezes confessional maps against imperial revision. Attempts at reversal (the Schmalkaldic defeat and imposed terms of 1547-48, the Edict of Restitution of 1629) either backfire or are undone at the next general settlement. Bound by the electoral structure, the emperor cannot leave the constitution he nominally heads.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, imperial_authority, payer,
    institutional, generational, constrained, continental).

% Must conform to the ruler's confession or emigrate: they bear visitations, fines, forced conversions, and expulsions, and their parishes and schools are re-staffed from above. Those whose ruler's confession matches their own receive an incidental peace dividend, but no subject chooses the rule. Emigration exists (Huguenot and Salzburg-scale flows come later) but costs land, guild membership, and kin networks. Resistance takes league, revolt, and flight forms and is met with force.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, dissenting_subjects, payer,
    powerless, generational, constrained, regional).

% Monasteries are dissolved and episcopal estates annexed; individual clerics face a bind between vows and mandated conformity. Their identity is fused with an institutional vocation that the settlement abolishes in place, so exit means abandoning the self, not just the post. A minority converts and retains office, showing the lock breaks selectively for the ambitious; the rest lose livelihood and institution simultaneously.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, secularized_clergy, payer,
    organized, biographical, identity_locked, regional).

% Anabaptist, Spiritualist, and congregational movements reject both the papal and the princely establishment and propose voluntary gathered churches instead. They are persecuted by both confessional blocs, hold no seat at Augsburg or Westphalia, and their tolerationist alternative never enters the settlement vocabulary; their exclusion is maintained by the same enforcement machinery that maintains the territorial rule.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, radical_congregationalists, excluded,
    powerless, biographical, trapped, regional).

% French and Swedish mediators at Osnabrueck and Muenster compile the full structure of claims, separate the dynastic-interest strand from the confessional-settlement strand in drafting, and codify the resulting sovereignty order. They see both what the arrangement coordinates and whom it strips, without holding either position.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, westphalian_congress_mediators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Territorializes religion to solve a real collective-action problem: in a fracturing empire, a rule for which confession governs where lets rival confessions coexist without each locality relitigating uniformity by force. The settlement freezes confessional maps so that coexistence is administered rather than fought.
% TRANSFER_FUNCTION: Moves ecclesiastical jurisdiction, land, appointment rights, and revenue streams (annates, first-fruits, dispensation and appeal fees, tithe administration) from supranational church and imperial structures to territorial dynasties; moves conformity obligations onto subject populations; moves enforcement workload onto princely consistorial machinery.
% ABSENT_VOICES: Radical congregationalists and tolerationist voices are structurally absent: persecuted by both blocs, seated at neither Augsburg nor Westphalia. Subject populations without princely protection are spoken for by their rulers. Had they been present, they would have objected that the settlement trades one compulsion for another rather than dissolving compulsion.
% DISAPPEARANCE_RATIONALE: If the territorial-confessional rule vanished overnight, the imperial constitution would need some other device to manage confessional coexistence; dynastic revenues built on secularized church property would lose their legal foundation; appointment and appeal flows would revert or renegotiate; and the sovereignty doctrines later read back from Westphalia would lack their codified anchor.
% FOUNDING_PROBLEM: Governing a polity splitting along confessional lines under two rival authority claims (universal church, emerging territorial state): how to contain open confessional civil war inside an imperial framework before a permanent settlement exists.
% FOUNDING_PROBLEM_CORROBORATION: The papacy attests the shift from outside the beneficiary set: Innocent X's bull Zelo domus Dei (1648) declares the Westphalian instruments null and void, precisely because the settlement codified losses rather than solving a shared problem. Imperial capitulation texts and Reichshofrat case files corroborate the transformation of the containment question into a sovereignty question. Twentieth-century confessionalization historiography corroborates from the analytical seat. The rulers themselves attest continued liveness by citing persistent confessional threat; no single seat's attestation settles it, hence contested.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the settlement decouples transferred jurisdiction and property from any service rendered to the paying seats: secularized lands and diverted curial revenue accrue to dynasties regardless of pastoral performance. Suppression (0.70) is structural and legal — conformity statutes, consistorial discipline, visitation regimes, expulsion edicts — with alternatives only partly closed (emigration corridors and free-city exceptions persist, hence accessibility_collapse 0.50 rather than mountain-grade). Resistance is high (0.75): the arrangement met the Schmalkaldic War, repeated imperial attempts at reversal, peasant and urban revolts, and finally a thirty-year general war; constructs that must be defended this hard are not natural facts. Theater_ratio (0.30) is moderate-low: the containment function was really performed, but a growing share of doctrinal language operated as cover for fiscal and jurisdictional seizure, which the rising theater series tracks. The temporal series run on ONE shared grid (1517, 1534, 1546, 1555, 1576, 1618, 1648) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: the story's traceable dynamic is enforcement-capacity maturation (church ordinances, consistories, visitation cycles, the 1629 Edict of Restitution), a rising ratchet rather than a static picture. The trajectory is monotonic, not cyclical: accumulation, not intermittent reinforcement, drives this constraint.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the ruler seat the arrangement is coordination it built and administers: a workable answer to confessional civil war, with enforcement costs it genuinely bears. From the papal and imperial seats the identical structure operates as expropriation — property, jurisdiction, and revenue stripped by a rule those seats cannot exit or revise. From the subject seat it is compulsory conformity with an incidental peace dividend whose size depends on whether the local ruler's confession happens to match one's own. Same-level actor dynamics matter among the rulers themselves: princes at nominally equal imperial rank experience the constraint differently depending on confessional-majority alignment, electoral status, and proximity to imperial courts — factors that differentiate their arbitrage options despite equal formal standing. Coalition dynamics appear on the target side too: princely leagues (Schmalkaldic League, Protestant Union) converted individually exposed rulers into organized force, which is why resistance stays high even as extraction accumulates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Territorial_rulers sit near the beneficiary end (low d): they collect the transfers, and their arbitrage-grade exit — they ARE the rule — keeps them there despite bearing enforcement costs. Papal_authority and imperial_authority sit near the full-target end (high d): trapped and constrained exits respectively, with identity-scale stakes for a civilizational-horizon papacy. Secularized_clergy approach full-target through exit modulation: identity_locked agents derive toward maximal d because their fusion with the abolished institution removes the exit that would otherwise dampen extraction. Dissenting_subjects derive high d from victim status plus constrained exit; the incidental peace dividend for confessionally matched subjects is real but does not move the class-level derivation, since no subject authors the rule. Radical_congregationalists are excluded rather than coordinated: their exclusion is the enforcement object, and they feed the suppression picture rather than the directionality arithmetic. No directionality_overrides are needed: the structural data already yields the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. A pure-snare reading would erase the genuine coordination achievement: the Augsburg settlement froze open confessional war for roughly six decades and gave coexistence an administrative form. A pure-rope reading would erase the asymmetry: the same instrument that coordinated coexistence also executed the largest property and jurisdiction transfer in European history to a single beneficiary class. Holding both keeps the engine's per-seat computation honest. On obsolescence: the founding problem (contain confessional civil war inside the imperial framework) was progressively transformed rather than solved — by 1648 the arrangement's operative function is codification of sovereignty, not containment of an immediate emergency. The founding_problem_status is therefore contested rather than dead: the parties genuinely dispute whether the arrangement still serves containment or has become sovereignty machinery wearing containment's clothes. The mismatch consumer reads status x disappearance_verdict; contested x world_rearranges correctly flags transformation-without-capture rather than a zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_primacy_contestation,
    'Is the political-realignment reading the operative instantiation of the reformation_composite kernel, or do the theological-fragmentation and technological-mediation readings identify structurally different constraints with different beneficiary sets and different epsilon?',
    'Compile the two sibling stories and compare victim/beneficiary sets: if the siblings'' seats (doctrinal minorities; print-market participants) diverge structurally from this reading''s seats (supranational authorities, compelled subjects), the kernel decomposes into distinct constraints rather than one.',
    'If a sibling reading is primary, this story''s epsilon referent narrows to the sovereignty-assertion strand alone and its classification may drop toward rope; if this reading is primary, theological content is instrument rather than substance and theater_ratio trends higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_primacy_contestation, conceptual, 'Committer-frame uncertainty: which reading of the reformation_composite kernel this constraint instantiates, and what sibling readings would change structurally.').

omega_variable(
    conversion_sincerity_ambiguity,
    'Were princely conversions driven by conviction, fiscal-jurisdictional interest, or dynastic alliance calculus — and does the mix vary by house and decade?',
    'Archival correspondence, privy council minutes, and disposition records of confiscated property weighed against contemporaneously stated doctrinal rationales.',
    'If interest dominates, the coordination story is closer to cover and the arrangement trends toward snare; if conviction dominates in pivotal houses, part of the measured extraction is incidental to a genuine confessional commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conversion_sincerity_ambiguity, empirical, 'Whether the settlement''s extraction was intended rent-seeking or emergent from sincerely motivated differentiation.').

omega_variable(
    subjects_net_position_ambiguity,
    'Did territorialization of religion leave subject populations net better off (domesticated confessional conflict, locally accountable church order) or worse (forced conformity, confiscation-funded warfare)?',
    'Comparative demographic and fiscal histories of conforming versus dissenting communities; migration-flow analysis across confessional borders.',
    'Determines whether dissenting_subjects derive near full-target or nearer symmetric, changing the computed per-seat classification for the largest seat in the story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subjects_net_position_ambiguity, empirical, 'Net welfare direction for the compelled subject class under the territorial rule.').

omega_variable(
    settlement_vs_war_finance_attribution,
    'Does the sovereignty outcome belong to the cuius regio arrangement itself, or to the parallel military-fiscal revolution that funded the standing armies able to enforce it?',
    'Counterfactual comparison with polities that asserted sovereignty without confessional differentiation (concordatarian France, the act-of-supremacy variant''s different timing in England).',
    'If war finance dominates, this constraint''s persistence depends more on external infrastructure than on its own enforcement, lowering the suppression properly attributable to this story and shifting weight toward the fiscal-military constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settlement_vs_war_finance_attribution, conceptual, 'Attribution boundary between the confessional-settlement constraint and the fiscal-military constraints that armed it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(refo_tr_t1534, reformation_composite__political_realignment_reading, theater_ratio, 1534, 0.16).
narrative_ontology:measurement(refo_tr_t1546, reformation_composite__political_realignment_reading, theater_ratio, 1546, 0.19).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.22).
narrative_ontology:measurement(refo_tr_t1576, reformation_composite__political_realignment_reading, theater_ratio, 1576, 0.25).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.28).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.3).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1534, reformation_composite__political_realignment_reading, base_extractiveness, 1534, 0.46).
narrative_ontology:measurement(refo_be_t1546, reformation_composite__political_realignment_reading, base_extractiveness, 1546, 0.53).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.61).
narrative_ontology:measurement(refo_be_t1576, reformation_composite__political_realignment_reading, base_extractiveness, 1576, 0.65).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.69).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1534, reformation_composite__political_realignment_reading, suppression_requirement, 1534, 0.42).
narrative_ontology:measurement(refo_su_t1546, reformation_composite__political_realignment_reading, suppression_requirement, 1546, 0.51).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.56).
narrative_ontology:measurement(refo_su_t1576, reformation_composite__political_realignment_reading, suppression_requirement, 1576, 0.63).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.67).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, resource_allocation).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, technological_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the reformation_composite kernel per the epsilon-invariance principle. The colloquial label 'the Reformation' conflates three structurally distinct claims: (1) this political-realignment arrangement (epsilon ~0.72; beneficiaries: territorial rulers; victims: papal/imperial authority and compelled subjects); (2) the theological-fragmentation claim (different victim set: doctrinal minorities and ecumenical unity projects; epsilon authored separately); (3) the technological-mediation claim (different beneficiary set: printers, publishers, vernacular market participants; epsilon authored separately). This upstream story typically influences the siblings because political-protection narratives are cited as evidence within them; each member of the family links to the others via network.affects_constraints, and no single file hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
