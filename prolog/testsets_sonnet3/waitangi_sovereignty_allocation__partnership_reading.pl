% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Doctrine (Principles-Based Reading)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This story authors the partnership reading of the Waitangi sovereignty
 *   kernel: the view that the Treaty established an ongoing relationship of
 *   good faith, active protection, and consultation between Crown and Māori,
 *   developed largely through judicial and Tribunal doctrine since the 1970s
 *   (Treaty of Waitangi Act 1975, NZ Maori Council v Attorney-General 1987,
 *   subsequent Tribunal jurisprudence). This is a hybrid structure: it
 *   genuinely coordinates an ongoing relationship and channels real redress
 *   to settling iwi, but it also extracts finality from settling iwi in
 *   exchange for that redress, moves at a pace and scope the Crown ultimately
 *   controls, and leaves urban Māori and not-yet-settled iwi outside its
 *   benefit structure. The rangatiratanga reading (Māori text retained full
 *   authority) and the Crown sovereignty reading (English text ceded complete
 *   sovereignty) are NOT this constraint — they are separate sibling stories
 *   with their own ε and stakeholder sets, linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.48).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.42).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Doctrine (Principles-Based Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '751e3719-763f-4854-b92d-8dbdab41d970').
narrative_ontology:cs_kernel_codification('751e3719-763f-4854-b92d-8dbdab41d970', distributed).
narrative_ontology:cs_authority_grounding('751e3719-763f-4854-b92d-8dbdab41d970', practice).
narrative_ontology:cs_interpretation_layer_present('751e3719-763f-4854-b92d-8dbdab41d970').
narrative_ontology:cs_reading_relation('751e3719-763f-4854-b92d-8dbdab41d970', waitangi_sovereignty_allocation__crown_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('751e3719-763f-4854-b92d-8dbdab41d970', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('751e3719-763f-4854-b92d-8dbdab41d970', foundational, treaty_ambiguity_generates_ongoing_relational_duty).
narrative_ontology:cs_axiom_status(treaty_ambiguity_generates_ongoing_relational_duty, holdable).
narrative_ontology:cs_axiom_grounding('751e3719-763f-4854-b92d-8dbdab41d970', treaty_ambiguity_generates_ongoing_relational_duty, conventional).
narrative_ontology:cs_axiom('751e3719-763f-4854-b92d-8dbdab41d970', foundational, principles_doctrine_constrains_without_overriding_parliamentary_sovereignty).
narrative_ontology:cs_axiom_status(principles_doctrine_constrains_without_overriding_parliamentary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('751e3719-763f-4854-b92d-8dbdab41d970', principles_doctrine_constrains_without_overriding_parliamentary_sovereignty, conventional).
narrative_ontology:cs_reference_frame('751e3719-763f-4854-b92d-8dbdab41d970', treaty_of_waitangi_act_1975_incorporation).
narrative_ontology:cs_drift_state('751e3719-763f-4854-b92d-8dbdab41d970', contemporary_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('751e3719-763f-4854-b92d-8dbdab41d970', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_iwi).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, judiciary_and_waitangi_tribunal).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, iwi_awaiting_settlement).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, urban_maori_without_land_base).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, future_generations_bound_by_full_and_final_settlements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_iwi).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, non_maori_new_zealanders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains parliamentary sovereignty and sets the terms of consultation, negotiation, and settlement policy through the Office of Treaty Settlements. Absorbs the principles doctrine into legislation and administrative practice at its own pace, deciding which consultation obligations to codify and which to leave discretionary. Benefits from a mechanism that grants legitimacy and social peace without ceding the power to legislate contrary to Treaty principles if it chooses to.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_government, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_government, beneficiary).

% Develops and applies the Treaty principles doctrine through case law and Tribunal reports, translating textual ambiguity into working obligations of good faith, active protection, and redress. Has no enforcement power over Parliament itself and must operate within whatever incorporating statutes exist; its findings carry moral and political weight but not automatic legal force absent legislative incorporation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, judiciary_and_waitangi_tribunal, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, judiciary_and_waitangi_tribunal, observer).

% Negotiate historical claims through the Tribunal and direct Crown negotiation, receiving financial redress, cultural redress, and co-governance arrangements in exchange for signing full and final settlement deeds that extinguish further claims for that grievance. Gain real resources and recognition but accept a ceiling on future claims and remain within a framework where the Crown retains ultimate sovereign authority over the settlement terms offered.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_iwi, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_iwi, payer).

% Remain in the Tribunal queue or in unresolved direct negotiations, subject to the Crown's negotiation timetable and funding priorities. Cannot compel faster resolution and bear the ongoing cost of unresolved dispossession while the doctrine's promise of good faith consultation is administered at a pace the Crown controls.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, iwi_awaiting_settlement, payer,
    organized, generational, trapped, regional).

% Disconnected from iwi structures through historical urbanization and land loss, they are largely outside the iwi-based settlement architecture that channels partnership benefits. The partnership doctrine's redress mechanisms are structured around iwi and hapū identity, leaving this population without a clear seat at the negotiating table despite bearing colonial harms.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, urban_maori_without_land_base, excluded,
    powerless, biographical, trapped, national).

% Inherit settlement deeds negotiated by prior generations that permanently extinguish claims regardless of whether the redress proves adequate over time, or whether new information about historical Crown conduct emerges. Cannot renegotiate the finality clauses their ancestors accepted under Crown-set terms.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, future_generations_bound_by_full_and_final_settlements, payer,
    powerless, civilizational, trapped, national).

% Fund settlements through general taxation and experience shifts in resource allocation, co-governance, and public discourse as the partnership doctrine expands. Some perceive the doctrine as legitimate historical redress; others contest its scope and permanence, but as individuals have little direct voice in how the Crown administers the doctrine.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, non_maori_new_zealanders, observer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, non_maori_new_zealanders, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, diffuse).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working mechanism for the Crown and Māori to resolve historical grievances and structure an ongoing relationship without either side abandoning its core position — the Crown keeps parliamentary sovereignty, Māori gain enforceable-in-practice consultation and redress rights, and both avoid the alternative of unresolved, escalating constitutional conflict.
% TRANSFER_FUNCTION: Moves financial and cultural redress, co-governance arrangements, and procedural consultation rights from the Crown to settling iwi, funded by general taxation; in return, settling iwi transfer finality — the extinguishment of further legal claims over the settled grievance — back to the Crown.
% ABSENT_VOICES: Urban Māori without an iwi affiliation and iwi still awaiting settlement have limited standing in a framework built around iwi-based negotiation; they would argue the doctrine's benefits are unevenly and slowly distributed. Non-Māori taxpayers who contest settlement scope are also outside the Crown-iwi negotiating table despite funding the outcomes.
% DISAPPEARANCE_RATIONALE: If the partnership doctrine were abandoned, the Waitangi Tribunal's interpretive framework would lose its legal anchor, ongoing negotiations would stall or revert to pure political discretion, existing co-governance arrangements would face legal challenge, and Crown-Māori relations would likely revert toward the unmediated assertion of parliamentary sovereignty that generated the historical grievances in the first place.
% FOUNDING_PROBLEM: The Treaty text itself was ambiguous and its Māori and English versions diverged, but by the late twentieth century it was clear that decades of Crown breach and unilateral land alienation had produced grievances requiring some institutional mechanism for redress and ongoing consultation that did not require Māori to renounce a relationship with the Crown or the Crown to renounce sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal itself, an institution created by statute and staffed partly by independent legal and historical experts, attests through its reports that Crown breaches were extensive and that partial redress remains ongoing and incomplete. Independent historians and international human rights bodies (including UN Special Rapporteur reports on indigenous peoples) corroborate that the founding grievances were real and substantial; they diverge from Crown Law Office and some settled-iwi leadership framing on whether the current doctrine adequately resolves them or merely manages them at a politically tolerable pace.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects a genuinely mixed structure: it is meaningfully lower than a pure extraction reading because the doctrine does deliver real redress and procedural rights, but it is not low because the finality/extinguishment mechanism and the Crown's retained discretion over pace and scope constitute a real cost borne asymmetrically by claimant iwi and future generations. Suppression (0.42, gently declining over the interval) reflects a legal architecture that has become progressively more entrenched in statute and administrative practice, reducing (not eliminating) the discretionary space in which the Crown can simply ignore the doctrine — hence the modest downward drift rather than a rise. Theater ratio rises over the interval (0.2 to 0.4) as consultation processes and Waitangi Day observances proliferate in volume while the underlying settlement pace and quantum remain politically capped, consistent with a maturing but partially performative practice layer growing around a genuine but bounded core function.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown sits closest to the beneficiary end: it retains ultimate sovereign authority, sets the settlement calendar and quantum ceilings, and gains social and international legitimacy from the doctrine's existence. The judiciary and Tribunal are structurally positioned as agenda-setters without direct extraction — their institutional interest is in the doctrine's coherence and legitimacy rather than personal rent. Settling iwi are genuine partial beneficiaries (real resources and recognition flow to them) but also partial payers (they surrender further legal claims through finality clauses) — hence the dual role. Iwi awaiting settlement and future generations bound by finality clauses sit closer to the target end: they bear the doctrine's costs (delay, or an inherited settlement they had no voice in) without commensurate present benefit. Urban Māori are excluded rather than coordinated, since the doctrine's negotiating architecture is iwi-based.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — historical Crown breach requiring redress and an ongoing consultative relationship — is genuinely contested as either fully live or substantially resolved: for iwi still awaiting settlement it is clearly live; for settled iwi bound by finality it is formally closed but its adequacy remains contested; for the Crown, the doctrine increasingly functions as a legitimating administrative practice as much as an active redress engine. This mixed status is why the story is authored as tangled_rope rather than either pure rope (would ignore the extraction of finality and the exclusion of urban Māori) or pure snare (would ignore the real, substantial redress and consultation gains that have occurred). The doctrine's tangled-rope character is itself the argument against collapsing it into either the crown_sovereignty_reading's near-mountain framing of unconstrained parliamentary supremacy or the rangatiratanga_reading's framing of an unresolved sovereignty transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partnership_doctrine_as_reading_or_settlement,
    'Is the partnership doctrine a genuine third structural resolution of the Treaty''s sovereignty question, or a judicially and politically convenient compromise that avoids resolving the underlying contradiction between the English and Māori texts?',
    'Comparative analysis of how the doctrine is invoked in litigation and settlement negotiation over time: if the doctrine consistently yields determinate outcomes independent of which party invokes it, it functions as a genuine framework; if outcomes track which party has more political leverage in a given period, it functions primarily as a flexible legitimating device.',
    'If the doctrine is a genuine independent resolution, tangled_rope with moderate extraction is the correct classification. If it is primarily a legitimating device deployed to manage rather than resolve the sovereignty contest, effective extraction and suppression would both be higher, moving the classification toward snare from the perspective of iwi awaiting settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_doctrine_as_reading_or_settlement, conceptual, 'Whether partnership doctrine is a genuine structural resolution or a discretionary management device.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three kernel readings (crown_sovereignty, partnership, rangatiratanga) disagree, structurally — is it the scope of ceded authority (Article I/II textual divergence), the enforceability of principles against Parliament, or the finality of historical settlement?',
    'Textual and jurisprudential comparison of the three readings'' treatment of (a) whether Article II''s Māori text controls, (b) whether Treaty principles bind or merely inform Parliament, and (c) whether settlements can be reopened.',
    'Locating the disagreement precisely determines which reading''s classification would shift under new legislation (e.g., statutory entrenchment of principles would move partnership_reading toward rope; explicit legislative override of principles would move it toward the crown_sovereignty_reading''s framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Precise structural locus of disagreement among the three kernel readings.').

omega_variable(
    urban_maori_exclusion_severity,
    'Is the exclusion of urban Māori without iwi affiliation from the settlement architecture a fixable administrative gap or a structural feature of an iwi-based redress model that cannot be reformed without redesigning the whole doctrine?',
    'Track whether pan-Māori or urban-focused redress mechanisms (e.g., Whānau Ora, urban Māori authorities) are meaningfully incorporated into future settlements or remain marginal supplements.',
    'If genuinely fixable, this is a correctable gap within the tangled_rope structure. If structural, it suggests a deeper victim class permanently outside the doctrine''s coordination function regardless of doctrinal maturation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(urban_maori_exclusion_severity, empirical, 'Whether urban Māori exclusion is a fixable gap or structural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(wait_tr_t2015, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.33).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(wait_be_t2015, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement(wait_su_t2015, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the waitangi_sovereignty_allocation kernel. crown_sovereignty_reading holds the English Article I text as controlling, establishing unconstrained Westminster parliamentary supremacy over the Treaty relationship. rangatiratanga_reading holds the Māori Article II text as controlling, under which the Crown gained only kāwanatanga (governorship) while iwi retained full authority (tino rangatiratanga) over lands, resources, and taonga — a reading under which most subsequent Crown exercises of authority over Māori resources would register as substantially higher extraction. This partnership_reading occupies a structural middle position: it treats the ambiguity itself as generative of an ongoing relational obligation rather than resolving it toward either pole. Each reading is authored with its own ε, its own beneficiary/victim structure, and its own claimed_type; they are not to be averaged or reconciled into a single value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
