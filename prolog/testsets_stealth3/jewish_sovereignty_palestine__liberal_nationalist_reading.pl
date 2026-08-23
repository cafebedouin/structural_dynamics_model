% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Liberal-Nationalist Self-Determination Title to Jewish Statehood (Reading of the Sovereignty Kernel)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested sovereignty kernel:
 *   the liberal-nationalist claim that the Jewish people possess a collective
 *   self-determination right whose exercise through statehood in the
 *   ancestral homeland is legitimate, bounded by the premise that the
 *   Palestinian claim is co-equal and must be met with partition or
 *   binational terms. The epsilon referent is the STANDING arrangement under
 *   contest — actually-existing Israeli sovereignty as exercised, with its
 *   occupation-era accretions — judged by this reading's own compromise
 *   standard, never the binational alternative the reading would endorse. The
 *   claim and the metrics are independent authored facts: the claimed type
 *   (tangled_rope) states what I believe is structurally true of this
 *   reading's constraint; the metric values state what I believe is
 *   descriptively true of the standing arrangement's operation, and where
 *   they pull against each other that divergence is the datum. The story
 *   links to its four sibling readings via network.affects_constraints per
 *   the family-decomposition rule.
 *
 * KEY AGENTS:
 *   - - jewish_collective_as_rights_bearing_nation: Primary beneficiary (institutional/constrained) — receives recognized statehood, refuge, and national expression; cannot relocate the national project
 *   - - israeli_state_apparatus: Agenda setter and enforcer (institutional/constrained) — administers borders, citizenship, security; collects the arrangement's practical fruits while paying its enforcement bill
 *   - - palestinian_co_equal_claimants: Co-equal rival seat (organized/trapped) — holds formal parity, bears deferred statehood and restricted return; cannot exit the arrangement's jurisdiction
 *   - - displaced_palestinian_refugees: Heaviest historical cost-bearer (powerless/trapped) — the founding transfer fell on them; blocked return sustains their position
 *   - - occupied_territory_residents: Daily cost-bearers (moderate/trapped) — live under the permit-and-checkpoint security administration the state maintains
 *   - - israeli_palestinian_citizens: Inside-the-polity dual seat (moderate/constrained) — citizenship protections received, identity costs paid from within
 *   - - international_recognition_regime: External agenda setter and incidental beneficiary (institutional/mobile) — recognition, aid, and doctrinal vindication; lowest-cost seat
 *   - - religious_zionist_maximalists: Excluded constituency (organized/identity_locked) — covenantal claim the compromise frame excludes; fights the frame rather than exiting
 *   - - one_state_binational_advocates: Excluded alternative (moderate/constrained) — proposal dissolves the frame's arithmetic; carried by neither camp
 *   - - international_law_scholars: Analytical observer (analytical/analytical) — tests the arrangement's conformity to the doctrines it invokes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.64).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.7).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Liberal-Nationalist Self-Determination Title to Jewish Statehood (Reading of the Sovereignty Kernel)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '824abd79-08f0-4219-be10-f1b3ab4745db').
narrative_ontology:cs_kernel_codification('824abd79-08f0-4219-be10-f1b3ab4745db', distributed).
narrative_ontology:cs_authority_grounding('824abd79-08f0-4219-be10-f1b3ab4745db', expertise).
narrative_ontology:cs_interpretation_layer_present('824abd79-08f0-4219-be10-f1b3ab4745db').
narrative_ontology:cs_reading_relation('824abd79-08f0-4219-be10-f1b3ab4745db', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('824abd79-08f0-4219-be10-f1b3ab4745db', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('824abd79-08f0-4219-be10-f1b3ab4745db', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('824abd79-08f0-4219-be10-f1b3ab4745db', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('824abd79-08f0-4219-be10-f1b3ab4745db', foundational, collective_self_determination_universal_right).
narrative_ontology:cs_axiom_status(collective_self_determination_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('824abd79-08f0-4219-be10-f1b3ab4745db', collective_self_determination_universal_right, deontological).
narrative_ontology:cs_axiom('824abd79-08f0-4219-be10-f1b3ab4745db', foundational, symmetric_accommodation_of_rival_claim_required).
narrative_ontology:cs_axiom_status(symmetric_accommodation_of_rival_claim_required, holdable).
narrative_ontology:cs_axiom_grounding('824abd79-08f0-4219-be10-f1b3ab4745db', symmetric_accommodation_of_rival_claim_required, deontological).
narrative_ontology:cs_axiom('824abd79-08f0-4219-be10-f1b3ab4745db', secondary, ancestral_continuity_supports_territorial_claim).
narrative_ontology:cs_axiom_status(ancestral_continuity_supports_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('824abd79-08f0-4219-be10-f1b3ab4745db', ancestral_continuity_supports_territorial_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('824abd79-08f0-4219-be10-f1b3ab4745db', universal_rights_partition_frame).
narrative_ontology:cs_drift_state('824abd79-08f0-4219-be10-f1b3ab4745db', contemporary_post_oslo_stagnation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('824abd79-08f0-4219-be10-f1b3ab4745db', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_rights_bearing_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_co_equal_claimants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, displaced_palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, occupied_territory_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, international_recognition_regime).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, wilsonian_national_self_determination_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_nation_state_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A transnational people organized through the State of Israel and diaspora institutions. The arrangement delivers what the collective sought: recognized sovereign statehood in the historic homeland, a guaranteed refuge, and national expression. The collective cannot relocate its national project elsewhere — the homeland connection is the point — but it retains wide freedom to renegotiate terms through democratic and diplomatic channels. Costs flow back as security burden, international censure during enforcement episodes, and persistent internal disagreement over what terms are owed to the rival claimant.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_rights_bearing_nation, beneficiary,
    institutional, generational, constrained, global).

% Administers the arrangement: draws borders, runs citizenship and land law, commands the security forces that police the lines, and conducts the diplomacy that maintains recognition. Receives the practical fruits — territory under administration, tax base, strategic depth — and pays the enforcement bill: permanent mobilization, occupation administration costs, and diplomatic exposure. It can reshape terms unilaterally in the short run but cannot abandon the territory without dissolving itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_apparatus, beneficiary).

% The national movement and institutions of the rival claimant, holding parallel recognition (PLO, observer-state status). The arrangement grants their claim formal parity in principle while subordinating it in operation: statehood deferred pending negotiation, territorial extent negotiated down from the whole, refugee return restricted. They organize politically and diplomatically, retain civic and armed resistance capacity, and cannot exit — the arrangement governs their land and status wherever they stand.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_co_equal_claimants, payer,
    organized, generational, trapped, regional).

% Descendants of those displaced in 1948 and 1967, living under UNRWA care in camps across the Levant or scattered further afield. The arrangement's founding transfer fell on them directly, and its continuation blocks the return that would reverse it. They hold no state, no veto, and no enforcement lever; their leverage runs through host-state politics, UN machinery, and solidarity movements. Ordinary exit already happened to them — the open question is whether the arrangement ever reverses it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, displaced_palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Palestinians living under the security administration in the West Bank and, under blockade, Gaza. They run civil affairs through the Palestinian Authority where permitted, move through a permit-and-checkpoint system the administering state controls, and experience settlement expansion, land requisition, and episodic military operations as recurring costs. Individual emigration is possible and occurring; collective relief from the regime governing their towns has no address.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, occupied_territory_residents, payer,
    moderate, biographical, trapped, regional).

% Arab citizens of the state itself: formal voting rights, courts, and social services, alongside documented gaps in land allocation, planning, budget share, and a constitutional order that defines the state in another people's national terms. They receive citizenship protections while carrying the arrangement's identity costs from inside the polity. Individual exit (renunciation, emigration) exists; collective exit does not apply.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_palestinian_citizens, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_palestinian_citizens, beneficiary).

% The ensemble of patron states and UN bodies whose recognition, aid, and legal instruments keep the arrangement internationally legible. They condition support on peace-process participation, fund the humanitarian and institutional scaffolding on the Palestinian side, and periodically condemn enforcement episodes without withdrawing baseline recognition. Their doctrinal investment in universal self-determination is vindicated by the arrangement's existence; they can shift recognition policy at comparatively low cost, and episodically do.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_recognition_regime, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, international_recognition_regime, beneficiary).

% Settlement-movement and religious-national constituency for whom the land's wholeness is a covenantal obligation rather than a negotiable term. The arrangement's compromise premise — trading territory for legitimacy — asks them to treat sacred title as a bargaining chip. They operate inside the state's politics through parties, municipalities, and settlement institutions, yet the reading's defining frame excludes their core claim from the negotiating table. Leaving the collective's project would mean leaving the religious identity itself; they contest the frame rather than exit it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, religious_zionist_maximalists, excluded,
    organized, generational, identity_locked, regional).

% Advocates of a single shared polity with equal civic status for both peoples, drawing from both national camps and from outside them. Their proposal dissolves the two-homelands arithmetic the arrangement is built on, so neither camp's institutions carry it: it wins essays and loses agendas. They press from academic and activist margins; their realistic option is conversion into one of the seated camps' positions, which their premise forbids.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, one_state_binational_advocates, excluded,
    moderate, generational, constrained, regional).

% Jurists and political theorists who assess whether the arrangement conforms to the self-determination and occupation-law doctrines it invokes. They publish opinions the parties cite selectively, staff commissions of inquiry, and bear no costs from the arrangement's operation; their seat is the analytical vantage from which the structure's internal coherence is tested.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_rights_bearing_nation).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives two national collectives claiming the same territory a shared normative language — universal self-determination — under which both claims are statable, comparable, and tradable, and gives the international system a rule (parity of peoples; partition or power-sharing) for handling the pair together rather than selecting a winner by force.
% TRANSFER_FUNCTION: Transfers recognized sovereignty, territorial control, and demographic primacy in the homeland to the Jewish collective via statehood, while transferring to the Palestinian collective the costs of accommodation: deferred statehood, reduced territorial extent, restricted return, and life under the resulting security regime.
% ABSENT_VOICES: At the founding moments the resident Palestinian population was barely consulted — Balfour 1917, the 1922 Mandate, and the 1947 partition recommendation all proceeded over local objection — and those displaced in the war held no seat anywhere. Today the structurally absent seats are the religious-maximalist constituency, whose covenantal claim the compromise frame declines to hear, and binational one-state advocates, whom neither camp's institutions admit. Both object, one from inside the state and one from its margins.
% DISAPPEARANCE_RATIONALE: If this legitimation structure vanished overnight, the state would lose its rights-based grounding and stand on raw possession; recognition, treaties, aid frameworks, and the entire two-claimant diplomatic architecture would collapse into either theological or conquest framings, and every regional actor's posture would reset. Too many dependent institutions organize around the arrangement for it to vanish quietly.
% FOUNDING_PROBLEM: Nineteenth-century European statelessness and persecution of the Jewish minority, culminating in the Holocaust: a people without sovereign protection, barred from full belonging in host societies, with nowhere guaranteed to flee.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: EU Fundamental Rights Agency surveys record that large majorities of European Jews report harassment and consider emigration after violent episodes; French aliyah spikes track attack waves in host-country statistics; and Palestinian historiography that rejects the remedy (e.g., Rashid Khalidi) nonetheless documents the founding vulnerability as real. No corroborating source attests that the problem is solved.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.64: the standing arrangement contains a legitimate core by this reading's own lights — a persecuted people exercised a universally-statable right and obtained refuge — layered over substantial uncompensated imposition on the co-equal claimant (deferral of statehood, restricted return, occupation-era accumulation). Compromise was EXPECTED by the reading and not executed, which holds epsilon above the midpoint without reaching snare levels. Suppression 0.70 is a raw structural property, deliberately NOT scaled by power or scope in authorship: the enforcement machinery (permit regime, checkpoint network, blockade, wartime mobilization) is mature and load-bearing. Theater 0.42: the post-Oslo negotiation layer became increasingly performative — process substituting for outcome — peaking near 0.54 before open warfare stripped diplomatic pretense and lowered the performative share by 2026. Accessibility_collapse 0.40: alternatives (binational, confederal, cantonal) demonstrably survive once the constraint is understood — this is not a natural-law profile. Resistance 0.70: sustained armed, civic, diplomatic, litigious, and boycott fronts meet the arrangement continuously. CYCLICAL DYNAMICS: the suppression series shows one full enforcement cycle — rise through 1967, relaxation through Oslo, re-hardening after 2000 — and the oscillation itself functions as an extraction mechanism (intermittent reinforcement): each negotiation revival resets escalation expectations while the underlying extraction ratchets monotonically upward (epsilon never returns to its pre-spike floor except transiently in 1947/1993). Base metrics are measured at interval end, in the re-hardened phase of the cycle. Identity-lock note: the excluded maximalist seat's exit is fused theologically — the land covenant is constitutive of religious selfhood, so exit equals identity death; if that fusion broke, that seat's relationship to the arrangement would reprice entirely. COALITION NOTE: the payer seats are plural and heterogeneous (refugees, occupied residents, citizens, national movement); coordinated action across them (unified Palestinian front, pan-Arab initiatives) is the principal channel by which their aggregate leverage could exceed any seat's individual power rating.
 *
 * PERSPECTIVAL GAP:
 *   Three seats experience structurally different constraints under one roof. The beneficiary/agenda seats (collective, state, recognition regime) encounter a coordination achievement they built and maintain — the reading computes coordination-heavy from their low directionality and superior exits. The payer seats (claimants, refugees, occupied residents) encounter enforced deferral and restriction — trapped exits amplify their effective extraction toward the full-target end. The EXCLUDED maximalist seat encounters the same arrangement as suppression of ITS claim: the compromise premise that disciplines the payers' rivals simultaneously forecloses the maximalist's covenantal terms. One structure, three experienced constraints; the engine computes this divergence from the authored power/exit data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the Jewish collective (d near 0) and, via its secondary beneficiary role, for the state apparatus and recognition regime. Victim declarations drive high directionality for the three Palestinian payer seats, amplified by trapped exits (nowhere else for the claim to live) and damped only for the mobile recognition regime, whose costs are discretionary and reversible. The israeli_palestinian_citizens dual seat nets out near-symmetric: citizenship benefits against identity costs. NO DIRECTIONALITY OVERRIDES ARE AUTHORED: the derivation chain (beneficiary/victim declarations + power level + exit options) produces the correct d for every seat. One known limitation worth recording: the override mechanism keys on power_atom alone, so it could not differentiate the two institutional seats (state apparatus vs. recognition regime) even where their structural relationships differ; that differentiation is carried by secondary_role declarations instead, which is why the regime's mobile exit and discretionary costs matter more than its nominal agenda-setting power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — statelessness and persecution exposure — is corroborated as LIVE by sources outside the beneficiary set (host-state hate-crime data, aliyah push factors, and Palestinian historiography that disputes the remedy while attesting the original vulnerability). The mismatch consumer therefore reads founding_problem_status=live x disappearance_verdict=world_rearranges: no zombie flag, no resolved mandatrophy. The theater series warrants a narrower caution: the growth of performative activity is concentrated in the peace-process layer (proxy goals replacing the resolution function locally), which is metric-substitution drift in a subsystem rather than obsolescence of the whole — flagged for temporal tracking, not for mandate retirement. The scaffold question does not arise: the arrangement carries no sunset clause and its justification is steady-state refuge, not transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_commitment_structure,
    'This story instantiates the liberal_nationalist_reading of kernel jewish_sovereignty_palestine. Do the standing arrangement''s actual mechanics match this reading''s structure (co-equal claimant honored, compromise-binding), or do a sibling reading''s mechanics better fit observed practice?',
    'Cross-reading corpus comparison plus implementation tests: was partition ever executed as parity (1947 plan, Oslo final-status schedule), and do current trajectories (annexation legislation, settlement build-out, wartime displacement) track the compromise requirement or abandon it?',
    'If practice abandons co-equal claimant status, this reading''s epsilon is understated and computed types shift snare-ward across payer seats; if compromise mechanics bind, the tangled_rope structure holds and sibling stories inherit weaker victim claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_commitment_structure, conceptual, 'Committer-frame uncertainty: which reading of the sovereignty kernel matches the arrangement''s operative structure.').

omega_variable(
    co_equal_status_binding_vs_aspirational,
    'Within this reading, is the Palestinian claim''s co-equal status a binding term the arrangement must honor, or an aspiration acknowledged rhetorically while enforcement proceeds on other terms?',
    'Trace binding instances and their enforcement: 1947 acceptance logic, Oslo''s mutual-recognition exchange and final-status timetable, freeze-linked aid conditionality — measure whether violations carried enforceable costs or only rhetorical ones.',
    'Aspirational-only status converts the coordination half of the ledger into performance, raising effective extraction above the authored 0.64 and driving payer-seat directionalities toward full-target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_equal_status_binding_vs_aspirational, empirical, 'Whether the reading''s co-equal claimant premise binds the arrangement or decorates it.').

omega_variable(
    founding_problem_recurrence,
    'Does the founding vulnerability this arrangement answers — minority persecution and statelessness exposure — persist as a live generator of demand, or has integration and deterrence retired it?',
    'Longitudinal antisemitism indices, aliyah push-factor studies, and diaspora security-spending trends read against assimilation outcomes.',
    'If retired, the arrangement''s mandate outlives its function and mandatrophy analysis flips to resolved; if recurrent (current data indicate recurrence), the founding problem stays live and the arrangement''s core justification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_recurrence, empirical, 'Liveness of the founding problem the reading claims to answer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1917, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1917, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t1917, observed).
narrative_ontology:measurement(jewi_tr_t1936, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1936, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1936, observed).
narrative_ontology:measurement(jewi_tr_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1947, 0.16).
narrative_ontology:measurement_basis(jewi_tr_t1947, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.24).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.46).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2005, 0.49).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2018, 0.54).
narrative_ontology:measurement_basis(jewi_tr_t2018, observed).
narrative_ontology:measurement(jewi_tr_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1917, 0.3).
narrative_ontology:measurement_basis(jewi_be_t1917, observed).
narrative_ontology:measurement(jewi_be_t1936, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1936, 0.46).
narrative_ontology:measurement_basis(jewi_be_t1936, observed).
narrative_ontology:measurement(jewi_be_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1947, 0.41).
narrative_ontology:measurement_basis(jewi_be_t1947, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2005, 0.57).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement_basis(jewi_be_t2018, observed).
narrative_ontology:measurement(jewi_be_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2026, 0.64).
narrative_ontology:measurement_basis(jewi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1917, 0.25).
narrative_ontology:measurement_basis(jewi_su_t1917, observed).
narrative_ontology:measurement(jewi_su_t1936, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1936, 0.55).
narrative_ontology:measurement_basis(jewi_su_t1936, observed).
narrative_ontology:measurement(jewi_su_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1947, 0.6).
narrative_ontology:measurement_basis(jewi_su_t1947, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement_basis(jewi_su_t1993, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2018, 0.67).
narrative_ontology:measurement_basis(jewi_su_t2018, observed).
narrative_ontology:measurement(jewi_su_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2026, 0.7).
narrative_ontology:measurement_basis(jewi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legitimacy of Jewish sovereignty in Palestine' decomposes into five structurally distinct claims (epsilon-invariance rule: differing observables yield differing epsilon, therefore differing constraints). This file instantiates the liberal-nationalist member: legitimacy sourced in universal self-determination, bounded by a co-equal rival claim, compromise-expected. The settler-colonial sibling authors the same territory arrangement as a displacement regime regardless of intent (constitutive victims, higher epsilon); the religious sibling authors divine-promise title (victim structure indexed to the rival's theological standing); the cultural sibling may authorize no sovereignty at all (minimal enforcement, low epsilon); the post-zionist sibling authors the legacy framework itself as present-day obstruction. The liberal member is upstream of the family: its international-law grounding supplies the recognition architecture every sibling contests, and its practice-drift feeds the post-zionist critique downstream. Family links declared via affects_constraints in all five files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
