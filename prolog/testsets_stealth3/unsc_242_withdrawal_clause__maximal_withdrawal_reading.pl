% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Maximal Reading (French Definite Article Controls)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   Security Council Resolution 242 (22 November 1967) opens its operative
 *   section with a withdrawal clause whose English text reads 'Withdrawal of
 *   Israel armed forces from territories occupied in the recent conflict' —
 *   indefinite article — while the equally authentic French text reads
 *   'Retrait des forces armées israéliennes des territoires occupés lors du
 *   récent conflit', where 'des territoires' admits a definite reading ('from
 *   THE territories'). This story instantiates the MAXIMAL reading of that
 *   clause: withdrawal is mandatory from all occupied territories, warranted
 *   by the Charter Article 2(4) territorial-integrity default and the
 *   inadmissibility-of-acquisition principle, with the French definite
 *   article controlling. Per the kernel-reading rules, the contest is NOT
 *   described inside this constraint: the partial_withdrawal_reading and the
 *   interpretive_authority_structure are separate stories linked through
 *   network.affects_constraints and the reading_relations below. Epsilon's
 *   referent is fixed to the standing arrangement under contest — the
 *   occupation itself (annexed East Jerusalem and Golan, West Bank settlement
 *   enterprise and dual legal regime) — assessed by this reading's own
 *   lights, per the OQ-258 referent ruling; it is not the cost of the
 *   withdrawal demand to the occupier. Claim/metric independence: the
 *   decomposition manifest hypothesized rope for this reading; my structural
 *   read finds a genuine coordination function (a determinate, universally
 *   citable retrocession rule anchoring the 2(4) default) fused with
 *   asymmetric extraction (one party surrenders territory and settlement
 *   value while dispossessed claimants recover legal position) that requires
 *   active enforcement against open resistance — I therefore claim
 *   tangled_rope and author the metrics I take to be descriptively true. The
 *   divergence from the manifest hypothesis is data, not error. KEY AGENTS
 *   (by structural relationship): - israeli_state: Primary target
 *   (powerful/constrained) — bears the full-retrocession demand -
 *   palestinian_residents_of_occupied_territories: Primary beneficiary
 *   (powerless/trapped) — holds the enforceable legal position the reading
 *   confers - syrian_golan_displaced: Secondary beneficiary
 *   (powerless/trapped) — restitution claim on the annexed plateau -
 *   west_bank_settler_population: Cost-bearing constituency
 *   (organized/identity_locked) — evacuation exposure, identity-fused with
 *   retention - us_permanent_member: Patron-agenda blocker
 *   (institutional/mobile) — veto shield sustains non-execution -
 *   un_security_council: Author-administrator (institutional/constrained) —
 *   owns the text, cannot execute past the veto - icj: Interpretive
 *   administrator (institutional/analytical) — supplies the reading's
 *   judicial warrant - arab_league_member_states: Collective
 *   beneficiary-champion (organized/constrained) - plo_representation:
 *   Claimant representative (moderate/constrained) — dual seat: collects
 *   standing, spends capital advancing the reading -
 *   drafting_history_scholars: Analytical observers (analytical/analytical) —
 *   hold the documentary record both readings cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.75).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.7).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Maximal Reading (French Definite Article Controls)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '6a5231d8-862e-4e05-b171-c7ab8938f1eb').
narrative_ontology:cs_kernel_codification('6a5231d8-862e-4e05-b171-c7ab8938f1eb', fixed_text).
narrative_ontology:cs_authority_grounding('6a5231d8-862e-4e05-b171-c7ab8938f1eb', lineage).
narrative_ontology:cs_interpretation_layer_present('6a5231d8-862e-4e05-b171-c7ab8938f1eb').
narrative_ontology:cs_reading_relation('6a5231d8-862e-4e05-b171-c7ab8938f1eb', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('6a5231d8-862e-4e05-b171-c7ab8938f1eb', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('6a5231d8-862e-4e05-b171-c7ab8938f1eb', foundational, french_definite_article_controls_withdrawal_scope).
narrative_ontology:cs_axiom_status(french_definite_article_controls_withdrawal_scope, holdable).
narrative_ontology:cs_axiom_grounding('6a5231d8-862e-4e05-b171-c7ab8938f1eb', french_definite_article_controls_withdrawal_scope, conventional).
narrative_ontology:cs_axiom('6a5231d8-862e-4e05-b171-c7ab8938f1eb', foundational, territorial_conquest_confers_no_title).
narrative_ontology:cs_axiom_status(territorial_conquest_confers_no_title, holdable).
narrative_ontology:cs_axiom_grounding('6a5231d8-862e-4e05-b171-c7ab8938f1eb', territorial_conquest_confers_no_title, deontological).
narrative_ontology:cs_reference_frame('6a5231d8-862e-4e05-b171-c7ab8938f1eb', charter_default_full_retrocession_baseline).
narrative_ontology:cs_drift_state('6a5231d8-862e-4e05-b171-c7ab8938f1eb', contemporary_post_icj_advisory_opinion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6a5231d8-862e-4e05-b171-c7ab8938f1eb', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, palestinian_residents_of_occupied_territories).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, syrian_golan_displaced).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_league_member_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, west_bank_settler_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, us_permanent_member).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, plo_representation).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, charter_article_2_4_territorial_integrity).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, inadmissibility_of_acquisition_of_territory_by_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopted Resolution 242 in November 1967 and owns its record: every reaffirmation, every blocked enforcement attempt, every presidential statement sits in its archive. It can act only when the five permanent members concur, and attempts to compel withdrawal have repeatedly been vetoed. Stepping away from the resolution entirely would discard its own authorship legacy, so it keeps the text alive through ritual reaffirmation and occasional initiative.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Interprets the resolution and the Charter norms behind it when asked — most consequentially in its July 2024 advisory opinion treating the acquisition of territory by force as inadmissible and the continued presence in the occupied territories as unlawful. Its pronouncements raise the diplomatic price of retention but carry no execution power of their own.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj, observer).

% Holds the territories whose retrocession the clause demands: the West Bank including East Jerusalem and the Golan Heights, having returned Sinai under the 1979 treaty with Egypt. It rejects the all-territories reading as a misreading of the English text and of drafters' intent, has annexed East Jerusalem and the Golan, and settles civilians across the West Bank. Its exposure runs through every global body it belongs to, but its day-to-day position is defended by military control on the ground and by a permanent-member patron's veto.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_state, payer,
    powerful, generational, constrained, global).

% Live under the occupation the clause addresses: military administration across much of the West Bank, annexed East Jerusalem, severed mobility, and expanding settlements on their land. They hold no state lever of their own; their route to restitution runs through the legal-diplomatic track this clause anchors — General Assembly votes, advisory-opinion accessions, treaty-body findings. Emigration is physically possible, but their claim attaches to the land and cannot be carried elsewhere.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, palestinian_residents_of_occupied_territories, beneficiary,
    powerless, generational, trapped, regional).

% Were displaced from the Golan Heights in 1967 and remain barred from returning; the plateau was annexed in 1981 under a law no state recognizes. Their restitution claim rides the same clause and the same legal track, at smaller scale and lower diplomatic salience than the Palestinian file.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, syrian_golan_displaced, beneficiary,
    powerless, generational, trapped, local).

% Championed the all-territories reading from the Khartoum summit onward and sponsor the annual General Assembly reaffirmations. Egypt recovered Sinai through the 1979 treaty — a negotiated partial outcome — while the collective position continues to insist on full withdrawal as the legal baseline. Their organizational weight converts into resolutions, not enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_league_member_states, beneficiary,
    organized, generational, constrained, regional).

% Several hundred thousand civilians living beyond the Green Line under the arrangement the clause targets for dissolution. Evacuation would strip them of homes, communities, and sunk generational investment; their presence is bound up with a national-religious mission in which remaining on the land is the point, so leaving the arrangement's reach would mean abandoning the identity project itself. They wield decisive weight inside the occupying state's domestic politics.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, west_bank_settler_population, payer,
    organized, biographical, identity_locked, local).

% Veto-wielding co-author of the original text and patron of the occupying state. It has blocked enforcement attempts aimed at compelling withdrawal while officially supporting a negotiated two-state outcome. It could release the shield at any time — the highest-optionality seat in the system — and collects the alliance stability and regional leverage the shield protects.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, us_permanent_member, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, us_permanent_member, beneficiary).

% Speaks for the Palestinian claim internationally as observer and recognized delegation; sponsors the maximal reading in every forum and negotiates on its basis. It gains standing from the clause's enforceable legal position while lacking any enforcement lever of its own; its influence runs through persuasion, recognition votes, and litigation support.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, plo_representation, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, plo_representation, agenda_setter).

% Reconstruct the negotiating record: the British drafters' choice of the indefinite English article, the French rendering 'des territoires', the private assurances exchanged in 1967-68, and the declassified archives. They publish the evidence every seat selectively cites; they collect nothing and pay nothing.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, drafting_history_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, palestinian_residents_of_occupied_territories).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one determinate, universally citable rule for post-conflict territorial retrocession — resolving the clause's ambiguity toward full withdrawal so that occupiers, claimants, adjudicators, and third states share a single reference point, and anchoring the Charter Article 2(4) default against fait-accompli consolidation.
% TRANSFER_FUNCTION: Moves territorial control — with the sovereignty, resources, and settlement value attached to it — from the occupying state back toward the pre-war line, and moves intangible goods the other way: enforceable legal position and agenda standing to the dispossessed claimants, reputational and isolation costs to the occupier.
% ABSENT_VOICES: The inhabitants of the territories themselves. Resolution 242 was drafted and voted by states; the Palestinian population of the West Bank, Gaza, and East Jerusalem had no representative in the chamber (observer status came only in 1974), and the displaced of the Golan had none at all. Jordan and Egypt spoke for populations they did not fully represent, which is why the clause frames the problem state-centrically — withdrawal between governments — and relegates the people concerned to a preambular 'just settlement of the refugee problem'. Their exclusion shaped the clause's architecture and still shapes what the maximal reading can deliver them.
% DISAPPEARANCE_RATIONALE: If the maximal reading vanished overnight, the partial reading would stand as the sole authoritative gloss: the occupier's retention would face no authoritative legal counterclaim, annexation would consolidate without the delegitimation the clause supplies, the dispossessed would lose their enforceable position and fall back on power politics alone, and every future conquest would price the Article 2(4) default as negotiable. The diplomatic-legal architecture built on the clause — treaty preambles, advisory opinions, annual votes — would rearrange around possessor-favoring defaults.
% FOUNDING_PROBLEM: The June 1967 war left one belligerent holding Sinai, Gaza, the West Bank including East Jerusalem, and the Golan Heights. The Council needed a formula that would reverse the conquests without rewarding the blockade-and-first-strike sequence that preceded them, pair withdrawal with recognition and secure boundaries, and say — over the objection of the victor — how much territory 'withdrawal' covered.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the ICJ's July 2024 advisory opinion and its 1980-81 annexation condemnations state the withdrawal problem as unresolved; successive Secretary-General reports document non-implementation annually; the occupier's own official positions — rejecting the all-territories reading while continuing the occupation — corroborate that the founding dispute is open; and the declassified drafting record, documented in treaty-interpretation scholarship, attests that the scope question was contested from the first week. No seat inside the beneficiary set is needed to establish liveness.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.75: the standing occupation as this reading assesses it is comprehensive — annexation of East Jerusalem (1980) and the Golan (1981), a settlement enterprise housing several hundred thousand civilians, dual legal regimes, and controlled land and water resources — each element counted as continuing violation of the 2(4) default. Suppression 0.70 is the coercive force the arrangement must muster to hold its demand against entrenched possession: total retrocession cannot be suggested, only imposed, and the veto structure raises the required force further. The suppression here is structural (patron veto, ground control), not internalized — the occupier's rejection is overt doctrine, not captured preference, so no internalized-suppression omega is required. Theater 0.58: since the mid-1980s the clause's life has been increasingly ritual — annual reaffirmations, anniversary statements, opinion after opinion unimplemented — though the 2024 ICJ advisory opinion is functional output whose implementation remains nil. Accessibility_collapse 0.35: understanding the maximal reading does not close the occupier's alternatives; retention remains practiced and patronized, so alternatives persist robustly at the level where the clause operates. Resistance 0.78: open doctrinal rejection, facts-on-ground construction, and veto shielding. The measurement series run on one shared eight-point grid (1967-2025 mapped to 0-58) so every tracked metric is authored at every examined time point; the series show a mild cycle — each negotiation window (Camp David 1979, Oslo 1993-94, disengagement 2005) briefly lowers measured extraction and theater before settlement accumulation and annexationist legislation resume. The oscillation is a side effect of external negotiation rounds, not an intermittent-reinforcement mechanism. Coalition note: the powerless seats are beneficiaries, not victims; their coalition (League, observer delegation, General Assembly majority) converts organizational weight into citations and votes but not enforcement, which is why the payer seat's resistance faces no countervailing organized force.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the occupier's seat the clause reads as a confiscatory demand authored by adversaries and indifferent to the security concerns that drove its position — it computes high extraction borne. From the dispossessed claimants' seats the same clause is the floor of legality, the minimum owed — they compute subsidy. The administrator seats (Council, Court) experience stewardship burden without material flow. The settler seat adds identity-lock dynamics: its exit is not merely costly but unthinkable, because presence on the land is constitutive of a national-religious mission — relational and ideological fusion, not career path dependence. Break the identity frame and that seat's resistance calculus changes faster than any enforcement could achieve. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive derivation: israeli_state (payer, powerful, constrained exit) derives near the full-target end; west_bank_settler_population (payer, identity_locked) pins at the target end since no arbitrage exists; palestinian_residents and syrian_golan_displaced (beneficiaries, trapped) derive near the beneficiary end — trapped exit deepens their dependence on the clause's legal track; arab_league_member_states derive low d as collective beneficiaries; us_permanent_member (agenda_setter/beneficiary, mobile exit) derives near-beneficiary, collecting the shield's alliance rents; un_security_council and icj sit mid-low as administrators collecting norm-order standing rather than material rents; plo_representation mixes collection and expenditure; drafting_history_scholars are analytical. No directionality_overrides are authored: the derivation chain reproduces the true relationships, and overrides key on power atoms, which would smear the story's three distinct institutional seats (Council, Court, patron) into one correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure modes. Read as pure extraction, the clause looks like a device punishing one state for others' benefit; the tangled-rope structure keeps the genuine coordination function visible — a determinate retrocession rule that occupiers, claimants, adjudicators, and third states can all cite, anchoring the anti-conquest default system-wide — with the occupier's costs riding the same structure. Read as pure coordination, the clause's non-execution would be excused as transition friction; the theater series dates the ritual drift instead. Mandatrophy: the founding problem (reversing the 1967 conquests) is live — attested by the ICJ's 2024 advisory opinion, annual Secretariat reporting, and, perversely, by the occupier's own continuing occupation and official rejection of the maximal reading. Founding_problem_status live crossed with disappearance_verdict world_rearranges is the consistent cell, so no zombie flag arises. Piton risk is real but channel-specific: inertia concentrates in the enforcement organs (reaffirmation ritual), not in the norm's citation — the enforcement_capacity_vs_norm_consensus omega carries that open question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_withdrawal_scope,
    'This story instantiates the maximal reading of the unsc_242_withdrawal_clause kernel; the partial_withdrawal_reading sibling would relocate the beneficiary structure (occupier retains strategic territory under a discretionary-scope gloss) and author a far lower epsilon over the same standing occupation. Which reading a seat adopts is the primary driver of every classification this family emits.',
    'Family-level comparison: compile both reading stories and the interpretive_authority_structure story; classify per seat; the divergence pattern across readings is the measurement — no within-story resolution exists.',
    'If the partial reading prevails institutionally, this constraint''s beneficiary set shrinks to rhetorical holders, its enforcement requirement collapses, and its classification drifts toward inertial maintenance; if the maximal reading prevails, the occupier''s effective extraction rises toward the full-target end.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_withdrawal_scope, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings change beneficiary structure and epsilon.').

omega_variable(
    interpretive_authority_location,
    'Who may resolve the textual contest — the ICJ by judicial interpretation, the drafting states by authorial intent, or the occupying state by appeal to settled practice?',
    'Track which authority''s gloss acquires citation dominance in subsequent resolutions, treaties, and advisory opinions; the 2024 ICJ advisory opinion is the strongest recent datum.',
    'Judicial dominance consolidates this reading''s warrant; authorial-intent dominance hands the seat to the partial reading; customary-practice dominance dissolves the clause into status-quo recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_location, conceptual, 'Location of interpretive authority over the ambiguous clause.').

omega_variable(
    article_asymmetry_provenance,
    'Was the French ''des territoires'' a faithful rendering of a stronger drafters'' intent, or a translation-layer artifact over a deliberately vague English original — and does the documentary record (Caradon team papers, private assurances exchanged in 1967-68) support definite-article scope?',
    'Full declassification and scholarly consilience of the 1967-68 drafting record, including the UK mission''s contemporaneous glosses and the French delegation''s instructions.',
    'A faithful-rendering finding strengthens this reading''s textual warrant; a translation-artifact finding shifts the contest wholly onto the authority axis and weakens the French-controls axiom without refuting the Charter-default argument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_asymmetry_provenance, empirical, 'Documentary provenance of the English/French article asymmetry.').

omega_variable(
    epsilon_referent_fixation,
    'Epsilon here is authored over the standing occupation — the arrangement under contest — as this reading assesses it (comprehensive annexation, settlement, dual regime), not over the withdrawal demand''s cost to the occupier; a reader measuring the clause''s demand-side cost instead is measuring a different constraint.',
    'Referent audit: confirm every seat''s computed extraction is taken against the declared referent; demand-side costing belongs to a separate story if anyone authors it.',
    'A misfixed referent would invert the beneficiary/target mapping and flip the reading''s classification; the fixation keeps the family''s epsilon values comparable across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_fixation, conceptual, 'Fixes the epsilon referent to the standing occupation per the kernel-reading rule.').

omega_variable(
    enforcement_capacity_vs_norm_consensus,
    'Does the clause''s half-century of unexecuted reaffirmation evidence a living norm awaiting enforcement capacity, or hollow ritual over a norm the system will not pay to execute?',
    'Compare compliance movement in windows where enforcement capacity existed (the 1979-82 treaty track, the Oslo window) against veto-blocked windows; if movement tracks capacity rather than citation volume, the norm is capacity-starved, not dead.',
    'A capacity-starved finding supports the retained tangled-rope structure with piton-drift risk confined to enforcement channels; a ritual finding dates a theater-driven transition earlier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_norm_consensus, empirical, 'Whether non-execution reflects missing capacity or a dead norm.').

omega_variable(
    beneficiary_interest_divergence,
    'Do the declared beneficiaries share one interest? Palestinian residents seek statehood and self-determination, the Golan displaced seek return, Arab League members seek the legal baseline — the clause serves them jointly on withdrawal scope but diverges on refugees, recognition, and final-status trade-offs.',
    'Observe coalition behavior in final-status negotiations: convergence on the maximal reading''s scope demand versus divergence on everything downstream of it.',
    'Divergence would fragment the beneficiary seat''s directionality and weaken the reading''s political carrying capacity even where its legal warrant holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_interest_divergence, empirical, 'Cohesion of the beneficiary coalition behind the maximal scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_maximal_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t0, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t12, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t12, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t14, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t14, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t27, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 27, 0.3).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t27, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t38, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 38, 0.38).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t38, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t47, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 47, 0.48).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t47, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t57, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 57, 0.55).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t57, observed).
narrative_ontology:measurement(unsc242_maximal_tr_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 58, 0.58).
narrative_ontology:measurement_basis(unsc242_maximal_tr_t58, observed).

% Extraction over time
narrative_ontology:measurement(unsc242_maximal_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(unsc242_maximal_be_t0, observed).
narrative_ontology:measurement(unsc242_maximal_be_t12, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(unsc242_maximal_be_t12, observed).
narrative_ontology:measurement(unsc242_maximal_be_t14, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement_basis(unsc242_maximal_be_t14, observed).
narrative_ontology:measurement(unsc242_maximal_be_t27, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 27, 0.52).
narrative_ontology:measurement_basis(unsc242_maximal_be_t27, observed).
narrative_ontology:measurement(unsc242_maximal_be_t38, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 38, 0.54).
narrative_ontology:measurement_basis(unsc242_maximal_be_t38, observed).
narrative_ontology:measurement(unsc242_maximal_be_t47, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 47, 0.62).
narrative_ontology:measurement_basis(unsc242_maximal_be_t47, observed).
narrative_ontology:measurement(unsc242_maximal_be_t57, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 57, 0.72).
narrative_ontology:measurement_basis(unsc242_maximal_be_t57, observed).
narrative_ontology:measurement(unsc242_maximal_be_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 58, 0.75).
narrative_ontology:measurement_basis(unsc242_maximal_be_t58, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_maximal_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(unsc242_maximal_su_t0, observed).
narrative_ontology:measurement(unsc242_maximal_su_t12, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(unsc242_maximal_su_t12, observed).
narrative_ontology:measurement(unsc242_maximal_su_t14, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement_basis(unsc242_maximal_su_t14, observed).
narrative_ontology:measurement(unsc242_maximal_su_t27, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 27, 0.45).
narrative_ontology:measurement_basis(unsc242_maximal_su_t27, observed).
narrative_ontology:measurement(unsc242_maximal_su_t38, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 38, 0.5).
narrative_ontology:measurement_basis(unsc242_maximal_su_t38, observed).
narrative_ontology:measurement(unsc242_maximal_su_t47, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 47, 0.6).
narrative_ontology:measurement_basis(unsc242_maximal_su_t47, observed).
narrative_ontology:measurement(unsc242_maximal_su_t57, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 57, 0.68).
narrative_ontology:measurement_basis(unsc242_maximal_su_t57, observed).
narrative_ontology:measurement(unsc242_maximal_su_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 58, 0.7).
narrative_ontology:measurement_basis(unsc242_maximal_su_t58, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% The colloquial label 'Resolution 242's withdrawal clause' decomposes per the epsilon-invariance principle into three structurally distinct stories: the maximal reading (this file — mandatory full retrocession, high epsilon over the standing occupation, claimed tangled_rope), the partial reading (discretionary scope, occupier retention tolerated, sharply lower epsilon and inverted beneficiary structure), and the interpretive_authority_structure (who may resolve the textual contest — a constraint about adjudication seats rather than about territory). The upstream Charter 2(4) default feeds both scope readings; each scope reading in turn pressures the authority contest. Family members link mutually through affects_constraints; an orphan scope-reading would be a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
