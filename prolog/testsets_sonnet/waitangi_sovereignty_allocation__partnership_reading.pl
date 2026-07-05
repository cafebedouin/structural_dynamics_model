% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Treaty of Waitangi Principles Doctrine — Crown-Māori Partnership Reading
 *   domain: constitutional/indigenous_rights
 *
 * SUMMARY:
 *   This story instantiates the partnership reading of the Waitangi
 *   sovereignty allocation kernel: the doctrine, developed principally
 *   through the Court of Appeal's 1987 Lands case and subsequent
 *   jurisprudence, that the Treaty founded an ongoing relationship of
 *   partnership between Crown and Māori requiring good-faith consultation,
 *   active protection of Māori interests, and redress for breach — while
 *   leaving parliamentary sovereignty formally intact. This is structurally
 *   distinct from the crown_sovereignty_reading (which treats Article I as a
 *   complete and unqualified cession leaving no constitutional partnership
 *   obligation) and the rangatiratanga_reading (which treats the Māori-text
 *   Article II as retaining full authority, such that Crown power is a
 *   subordinate grant rather than a plenary sovereign holding it must merely
 *   consult about). The partnership reading occupies a middle position: it
 *   generates real, judicially enforceable obligations (consultation, active
 *   protection, the principles doctrine feeding into statute) without
 *   displacing the Crown's ultimate sovereign authority — Parliament can
 *   still legislate inconsistently with Treaty principles if it does so
 *   expressly. This middling structural position is why extraction and
 *   suppression sit at moderate rather than extreme values: the doctrine
 *   constrains the Crown without controlling it, and it distributes real
 *   redress unevenly across iwi and hapu depending on their capacity to
 *   organize within the settlement process the doctrine created.
 *
 * KEY AGENTS:
 *   - crown_executive: agenda_setter/beneficiary (institutional/arbitrage) — administers settlements, retains sovereignty
 *   - iwi_claimant_groups: beneficiary/payer (organized/constrained) — receive redress within Crown-set fiscal envelope
 *   - iwi_without_settlement_leverage: payer (powerless/trapped) — consultation right exists but produces little
 *   - hapu_excluded_from_large_natural_groupings: excluded (powerless/trapped) — subsumed into iwi-level mandate
 *   - settler_state_legitimacy: beneficiary (institutional/analytical) — biculturalism narrative underwrites state standing
 *   - courts_and_tribunal: observer/agenda_setter (institutional/analytical) — develop and apply the principles doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.42).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.38).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Principles Doctrine — Crown-Māori Partnership Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'f49e2495-2c38-4f20-81f7-931e7c5c973b').
narrative_ontology:cs_kernel_codification('f49e2495-2c38-4f20-81f7-931e7c5c973b', fixed_text).
narrative_ontology:cs_authority_grounding('f49e2495-2c38-4f20-81f7-931e7c5c973b', practice).
narrative_ontology:cs_interpretation_layer_present('f49e2495-2c38-4f20-81f7-931e7c5c973b').
narrative_ontology:cs_reading_relation('f49e2495-2c38-4f20-81f7-931e7c5c973b', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('f49e2495-2c38-4f20-81f7-931e7c5c973b', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('f49e2495-2c38-4f20-81f7-931e7c5c973b', foundational, ongoing_relational_obligation_despite_ambiguity).
narrative_ontology:cs_axiom_status(ongoing_relational_obligation_despite_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('f49e2495-2c38-4f20-81f7-931e7c5c973b', ongoing_relational_obligation_despite_ambiguity, conventional).
narrative_ontology:cs_axiom('f49e2495-2c38-4f20-81f7-931e7c5c973b', foundational, sovereignty_constrained_but_not_overridden_by_consultation_duty).
narrative_ontology:cs_axiom_status(sovereignty_constrained_but_not_overridden_by_consultation_duty, holdable).
narrative_ontology:cs_axiom_grounding('f49e2495-2c38-4f20-81f7-931e7c5c973b', sovereignty_constrained_but_not_overridden_by_consultation_duty, conventional).
narrative_ontology:cs_reference_frame('f49e2495-2c38-4f20-81f7-931e7c5c973b', post_lands_case_principles_doctrine).
narrative_ontology:cs_drift_state('f49e2495-2c38-4f20-81f7-931e7c5c973b', contemporary_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f49e2495-2c38-4f20-81f7-931e7c5c973b', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, settler_state_legitimacy).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, iwi_claimant_groups).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, iwi_without_settlement_leverage).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, hapu_excluded_from_large_natural_groupings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, iwi_claimant_groups).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, general_public_taxpayers).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, partnership_of_good_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the consultation and settlement process, decides which claims are heard through the Waitangi Tribunal and Office of Treaty Settlements, and sets the fiscal envelope for redress. Retains full parliamentary sovereignty throughout — the partnership frame constrains how it must act, not whether it retains ultimate authority. Gains domestic and international legitimacy from being seen to honor the partnership without ceding sovereign power.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_executive, beneficiary).

% Negotiate settlements through the Tribunal process, receiving financial redress, cultural recognition, and co-governance arrangements over specific resources. Must accept the Crown's fiscal envelope and the finality clauses that close off future claims. Benefit from real material and symbolic redress but operate inside a process the Crown designed and can adjust; cannot exit the negotiation and pursue full restitution through any other channel because the Tribunal's recommendations are not binding and courts defer to Parliament on ultimate sovereignty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, iwi_claimant_groups, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, iwi_claimant_groups, payer).

% Smaller or historically fragmented iwi with weaker documentary records, disputed mandate, or overlapping claims find themselves negotiating from a position of limited leverage inside a process built around large natural groupings. Their good-faith consultation right exists on paper but produces materially smaller settlements or prolonged non-resolution; they cannot bypass the Crown-administered process to seek alternative redress.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, iwi_without_settlement_leverage, payer,
    powerless, generational, trapped, regional).

% Sub-tribal hapu whose distinct interests are subsumed within larger iwi-level settlement mandates for administrative convenience. They would object to being represented by a governance structure that does not reflect their specific relationship to particular lands or waters, but the large natural groupings policy structurally routes their voice through an iwi body that may not prioritize it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, hapu_excluded_from_large_natural_groupings, excluded,
    powerless, generational, trapped, local).

% The New Zealand state's international and domestic standing as a functioning bicultural democracy benefits from the partnership narrative — it allows the state to present ongoing colonization-era dispossession as being actively remedied through a good-faith, evolving relationship, which underwrites investment confidence, social cohesion messaging, and comparative standing against other settler states.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settler_state_legitimacy, beneficiary,
    institutional, civilizational, analytical, national).

% Fund settlements and Tribunal operations through general taxation. Broadly support or contest the partnership frame depending on political alignment; individually cannot exit the fiscal consequences of settlement policy, but as an electorate can pressure governments to narrow or expand the fiscal envelope through ordinary political contest.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, general_public_taxpayers, payer,
    moderate, generational, constrained, national).

% The Waitangi Tribunal investigates claims and issues recommendations; the courts have developed the principles doctrine (active protection, good faith, redress, informed decision-making) as a common-law gloss on the Treaty's constitutional status. Neither can bind Parliament; both shape how the Crown's obligations are articulated and can raise the political cost of noncompliance without possessing final authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, courts_and_tribunal, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, courts_and_tribunal, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, diffuse).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, good-faith process through which historical and ongoing grievances arising from Crown breaches can be investigated, acknowledged, and redressed, and through which Crown and Māori interests can be reconciled in ongoing governance without requiring a wholesale renegotiation of sovereignty.
% TRANSFER_FUNCTION: Moves fiscal redress, resource co-governance rights, and formal acknowledgment of breach from the Crown to settling iwi; simultaneously moves finality (the closing-off of further legal claims) from iwi to the Crown, and moves negotiating leverage away from iwi and hapu without strong documentary mandates or large-natural-grouping status toward those with it.
% ABSENT_VOICES: Sub-tribal hapu excluded from large natural groupings, and iwi with weak settlement leverage, would object that the partnership's consultation obligations are satisfied at the iwi-aggregate level while their specific interests go unheard; they are formally represented but structurally routed through bodies that may not carry their priorities into negotiation.
% DISAPPEARANCE_RATIONALE: If the partnership principles doctrine were repealed or judicially abandoned overnight, the Waitangi Tribunal's interpretive authority would collapse to a purely advisory historical function, existing settlement finality clauses would face renewed legal uncertainty, and the Crown would lose the primary doctrinal basis compelling consultation before legislation or resource decisions affecting Māori interests — co-governance arrangements and active-protection duties embedded in statute and case law would be stripped of their constitutional anchor.
% FOUNDING_PROBLEM: The Treaty's 1840 text was signed in two versions with materially different meanings (Article I ceding kāwanatanga vs. sovereignty; Article II retaining tino rangatiratanga vs. property rights), and for over a century the Crown treated the Treaty as a legal nullity while dispossessing Māori of land and authority. The founding problem the partnership reading was built to solve was reconciling this textual ambiguity and historical breach with an ongoing constitutional relationship, without either restoring full rangatiratanga or leaving breach unaddressed.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal, an independent statutory body established to investigate Crown breach and outside the direct control of either the Crown executive or claimant iwi, has repeatedly found the founding problem live — ongoing breach continues in resource management, health outcomes, and unresolved historical claims. Some legal scholars and Crown officials characterize the core historical breach as substantially addressed through the settlement process for groups that have settled; iwi without settlements and several independent academic reviews of the large natural groupings policy corroborate that the problem remains live and unevenly resolved.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1975, pre-Tribunal era, doctrine barely articulated) to 0.42 (2025) as the settlement process has matured into a large, administratively complex apparatus in which fiscal envelopes, finality clauses, and large-natural-groupings policy channel real but bounded redress — the rise reflects the doctrine becoming a governance technology with its own distributive effects, not a simple increase in Crown predation. Suppression falls from 0.55 to 0.38 over the same period as litigation and legislative recognition (State-Owned Enterprises Act s9, Resource Management Act references, Treaty of Waitangi Act amendments) reduced the Crown's ability to simply ignore the doctrine — good-faith consultation became progressively harder to evade, which is suppression declining, not extraction declining. Theater ratio rises modestly (0.1 to 0.3) reflecting some settled critique that consultation processes can become procedurally elaborate without correspondingly substantive outcomes for weaker claimant groups, though the coordination function (structured redress, resource co-governance) remains real and is not merely performative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown sits near the beneficiary end: it designs the process, controls the fiscal envelope, and gains legitimacy from the partnership frame while retaining final sovereign authority — the doctrine constrains its discretion but never removes its capacity to act. Well-organized iwi claimant groups sit closer to symmetric-to-beneficiary: they receive real redress but inside terms the Crown sets, and finality clauses convert future claims into a one-time settlement. Iwi without settlement leverage and excluded hapu sit near the target end: the consultation right exists formally but their weak bargaining position within a Crown-administered process yields materially smaller redress or prolonged unresolved status, and they cannot exit to an alternative venue since courts defer to Parliament and the Tribunal's findings are non-binding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a breached, textually ambiguous Treaty with ongoing constitutional governance — remains substantively live for many iwi and hapu (per Tribunal findings) even as the Crown and settled iwi increasingly treat major elements as resolved via settlement finality. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: treating the whole doctrine as pure extraction (which would erase the real coordination function and real redress it has produced) and treating it as pure coordination (which would erase the asymmetric outcomes for hapu without large-natural-grouping standing and the Crown's retained capacity to override the doctrine by express legislation). The doctrine's active enforcement dependency — it requires ongoing litigation, Tribunal findings, and political pressure to remain binding in practice — is exactly the requires_active_enforcement structural fact the tangled_rope classification requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partnership_vs_sovereignty_primacy,
    'Does the principles doctrine genuinely constrain the Crown in a way that could someday override express legislative intent, or does ''Parliament can always legislate inconsistently with Treaty principles if it says so expressly'' mean the partnership reading is ultimately subordinate to and revocable by the crown_sovereignty_reading whenever the Crown chooses to invoke it?',
    'Track whether courts ever find an implied limitation on Parliament''s ability to legislate contrary to Treaty principles even with express words, versus continued deference to the express-words override. A shift toward implied limitation would structurally elevate this reading; continued deference would confirm its subordinate, defeasible character.',
    'If the doctrine is always defeasible by express legislation, this reading''s constraint on Crown power is real but conditional — closer to a strong political/legal norm than an entrenched constitutional limit, which would push the classification toward more extractive over time if the Crown increasingly legislates around it. If courts developed an entrenchment doctrine, the reading would strengthen toward genuine coordination limiting sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_vs_sovereignty_primacy, conceptual, 'Whether the partnership reading is a durable constitutional limit or a defeasible political convention.').

omega_variable(
    large_natural_groupings_distributive_effect,
    'Is the large natural groupings settlement policy a necessary administrative simplification given resource constraints, or a structural mechanism that systematically disadvantages hapu and smaller iwi relative to larger, better-organized claimant bodies?',
    'Comparative analysis of settlement outcomes (per-capita redress, resource co-governance scope, time-to-settlement) between claimant groups organized at hapu level versus large natural groupings level, controlling for claim strength and documentary record.',
    'If the policy is primarily administrative necessity with roughly proportionate outcomes, the victim classification for excluded hapu weakens. If it systematically produces disproportionate outcomes correlated with organizational scale rather than claim merit, it strengthens the tangled_rope reading''s asymmetric-extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(large_natural_groupings_distributive_effect, empirical, 'Whether large natural groupings policy is neutral administration or structural disadvantage for smaller claimant groups.').

omega_variable(
    sibling_reading_framing_dependency,
    'Two coherent framings of the same kernel — the crown_sovereignty_reading''s textual-cession framing and the rangatiratanga_reading''s Māori-text framing — would each classify the underlying Crown-Māori relationship very differently (rope-like unqualified sovereignty exercise versus snare-like ongoing dispossession of retained authority). This partnership reading was selected because it reflects the doctrine actually adopted and enforced by New Zealand courts since Lands (1987), not because it is the most textually defensible reading of the 1840 instrument itself.',
    'Compare judicial and legislative uptake across the three readings over time; the partnership reading''s dominance in enforceable doctrine (versus the other two readings'' comparative marginalization in binding law despite textual arguments) is the signal guiding this selection.',
    'If courts moved decisively toward the rangatiratanga_reading (e.g., recognizing tino rangatiratanga as a freestanding source of authority rather than an interest the Crown must merely consult about), this constraint''s beneficiary/victim structure and extraction level would shift substantially, and this story would need revision or supersession rather than amendment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_framing_dependency, conceptual, 'Documents why the partnership reading was selected as the operative structural framing among three live readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement_basis(wait_tr_t1975, observed).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement_basis(wait_tr_t1985, observed).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement_basis(wait_tr_t1995, observed).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement_basis(wait_tr_t2005, observed).
narrative_ontology:measurement(wait_tr_t2015, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(wait_tr_t2015, observed).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(wait_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement_basis(wait_be_t1975, observed).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement_basis(wait_be_t1985, observed).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement_basis(wait_be_t1995, observed).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement_basis(wait_be_t2005, observed).
narrative_ontology:measurement(wait_be_t2015, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(wait_be_t2015, observed).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(wait_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement_basis(wait_su_t1975, observed).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement_basis(wait_su_t1985, observed).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement_basis(wait_su_t1995, observed).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(wait_su_t2005, observed).
narrative_ontology:measurement(wait_su_t2015, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement_basis(wait_su_t2015, observed).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(wait_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the waitangi_sovereignty_allocation kernel. crown_sovereignty_reading treats Article I as complete, unqualified cession under Westminster parliamentary supremacy with no ongoing constitutional partnership obligation (lower extraction from the Crown's perspective, since no consultation duty constrains it). rangatiratanga_reading treats the Māori-text Article II as retaining full authority (tino rangatiratanga) with the Crown holding only kāwanatanga — a narrower governorship grant — which would treat much of the Crown's current exercise of authority as itself extractive of retained Māori sovereignty. This partnership_reading occupies the doctrinally dominant middle position actually enforced by courts and legislature, generating moderate, judicially-enforceable but non-entrenching constraints on Crown power. Each story carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
