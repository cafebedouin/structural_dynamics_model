% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty Primacy Reading of the Secession Legitimacy Boundary
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the treaty-primacy reading of a contested
 *   secession-legitimacy kernel: when a province (or region) moves toward
 *   secession, this reading holds that neither federal nor provincial
 *   authority can validly settle the question because both derive whatever
 *   authority they hold over the contested territory from treaties that
 *   predate them, and that treaty holder consent is a precondition no
 *   secession process may bypass. This is a live legal-political argument
 *   advanced primarily by treaty nations and increasingly recognized in
 *   constitutional jurisprudence recognizing pre-existing Aboriginal and
 *   treaty rights, but it is contested by separatist movements who argue
 *   their referendum-expressed will is self-legitimating (the
 *   popular_sovereignty_reading) and by federalists who locate the whole
 *   question inside a constitutional-amendment process (the
 *   constitutional_impossibility_reading) that does not center treaty consent
 *   as a separate gate. The ε authored here (0.58) reflects the standing
 *   arrangement as this reading's own proponents would assess it: real
 *   coordination benefit for treaty nations who obtain leverage they did not
 *   have to litigate case-by-case, but real extraction risk in that both
 *   federal and provincial actors can and do invoke the principle
 *   instrumentally — raising it when convenient to block a secession they
 *   oppose for unrelated reasons, and downplaying or contesting it when a
 *   negotiated arrangement would otherwise suit them.
 *
 * KEY AGENTS:
 *   - treaty_nations_within_contested_territory: primary rights-holder and structural beneficiary of the veto, but also drawn unavoidably into every dispute
 *   - federal_crown_authority: institutional beneficiary that can invoke the principle strategically
 *   - provincial_governments_seeking_delay: secondary beneficiary using the principle as insulation
 *   - separatist_movement_constituents: primary payer under this reading — their referendum-based claim to legitimacy is denied standing
 *   - treaty_negotiation_bureaucracy: administers what 'consent' procedurally means and controls pace
 *   - international_observers_and_courts: analytical seat assessing legitimacy across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.58).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.62).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c').
narrative_ontology:cs_kernel_codification('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', distributed).
narrative_ontology:cs_authority_grounding('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', lineage).
narrative_ontology:cs_interpretation_layer_present('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c').
narrative_ontology:cs_reading_relation('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', foundational, treaty_rights_precede_and_bind_successor_sovereigns).
narrative_ontology:cs_axiom_status(treaty_rights_precede_and_bind_successor_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', treaty_rights_precede_and_bind_successor_sovereigns, deontological).
narrative_ontology:cs_axiom('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', foundational, territorial_sovereignty_claims_require_original_titleholder_consent).
narrative_ontology:cs_axiom_status(territorial_sovereignty_claims_require_original_titleholder_consent, holdable).
narrative_ontology:cs_axiom_grounding('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', territorial_sovereignty_claims_require_original_titleholder_consent, conventional).
narrative_ontology:cs_reference_frame('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', nation_to_nation_treaty_precedence).
narrative_ontology:cs_drift_state('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', contemporary_secession_movements, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3b9be9bf-b8e8-456d-9cdc-82e9cd1bda2c', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown_authority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments_seeking_delay).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_within_contested_territory).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, separatist_movement_constituents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_within_contested_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty relationships that predate the province and the federation both, negotiated nation-to-nation with the Crown. Under this reading, no secession proceeding can lawfully alter their land or jurisdiction without their consent. They benefit from the veto in principle, but in practice they bear the cost of being drawn into every secession dispute as a party whose consent is invoked, litigated, and sometimes simply assumed or ignored by both federal and separatist negotiators. Their exit from the dispute is not available — the territory under contest is their homeland.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_within_contested_territory, beneficiary,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_within_contested_territory, payer).

% Invokes treaty primacy as a structural barrier against provincial secession, since disturbing treaty relationships would require a process the federal government itself administers and can slow-walk. Benefits from having a durable constitutional-order argument that does not depend on federal popularity or provincial electoral outcomes. Can time its assertions of the treaty-consent requirement to suit its own negotiating position.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown_authority, agenda_setter).

% Provincial administrations opposed to a secession movement within their own jurisdiction can invoke the treaty-consent requirement as an additional, powerful legal obstacle without having to make the case themselves — the objection comes from the treaty nations, insulating the province from being cast as the sole obstructionist.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments_seeking_delay, beneficiary,
    institutional, biographical, mobile, regional).

% A settler-majority electorate that has voted for secession finds its referendum result insufficient on its own; under this reading, their expressed will can be lawfully blocked by treaty nations whose consent they neither sought at the outset nor structured their movement around obtaining. Their exit is legally constrained — the reading denies the referendum result standing to unilaterally settle territorial questions.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, separatist_movement_constituents, payer,
    organized, biographical, constrained, regional).

% The apparatus of federal-Indigenous relations (land claims offices, self-government negotiation tables, court-mandated consultation processes) administers what 'consent' means procedurally and controls the pace and terms of any consent-seeking process, giving it substantial power over how and whether a secession claim can ever be lawfully perfected.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_negotiation_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).

% International legal bodies and comparative constitutional scholars assess whether the treaty-primacy reading is a genuine application of self-determination and treaty law or a jurisdictional maneuver; they have no direct stake but their rulings and scholarship shape which reading gains legitimacy in future disputes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_observers_and_courts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents federal, provincial, and settler-majority actors from unilaterally redrawing sovereignty lines over land that is subject to pre-existing, nation-to-nation treaty commitments — coordinating around the principle that treaty relationships cannot be extinguished by a third party's internal political process.
% TRANSFER_FUNCTION: Moves veto leverage from whichever party would otherwise decide a secession question (federal government, provincial electorate) to treaty nations; in the same motion, it can transfer delay and negotiating cost onto separatist constituents and onto the treaty nations themselves, who become an unavoidable party to every dispute regardless of whether they sought that role.
% ABSENT_VOICES: Individual treaty nation members who oppose their own leadership's negotiating posture are rarely heard separately from their nation's institutional voice. Settler residents of contested territory who are also treaty beneficiaries (through historic intermarriage or adoption) occupy an ambiguous position not captured by either 'separatist' or 'treaty nation' framing.
% DISAPPEARANCE_RATIONALE: If the treaty-primacy principle were abandoned, secession negotiations would proceed as a two-party (or federal-provincial-referendum) matter, treaty nations would lose their formal veto standing in constitutional secession law, and territorial boundaries in treaty areas would become negotiable without their consent — a foundational reordering of who counts as a party to sovereignty questions.
% FOUNDING_PROBLEM: Historic treaties were negotiated as nation-to-nation agreements predating both federal confederation and provincial boundary-drawing in many regions; the founding problem this reading addresses is that neither federal nor provincial governments have ever held original title or sovereignty independent of what the treaties ceded or preserved, so any secession process that ignores the treaties treats a derivative authority as though it were original.
% FOUNDING_PROBLEM_CORROBORATION: Attested by treaty text and by court rulings that recognize treaty and Aboriginal rights as pre-existing and not created by the Crown (independent judicial corroboration outside the treaty nations themselves); also corroborated, more ambivalently, by federal government legal filings that concede treaty status even while contesting its practical scope. No corroboration exists from separatist movement leadership, who dispute the practical reach of the principle in secession contexts specifically.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the principle, while grounded in genuine pre-existing legal rights, is also available for strategic invocation by federal and provincial actors who are not themselves treaty holders — the coordination function (respecting prior treaty commitments) and an extraction function (using treaty nations' standing as leverage in disputes unrelated to their own interests) coexist in the same structure, which is exactly the tangled-rope signature. Suppression (0.62) reflects that this reading, once operative, forecloses referendum results from being self-executing and requires ongoing consultation processes that can be slow-walked or weaponized. Resistance is high (0.72) because separatist movements actively contest the principle's application to their specific case, arguing it was not designed to be a general secession veto. Theater ratio is moderate (0.3): consultation processes are sometimes genuine engagement and sometimes performative box-checking that satisfies procedural form without substantive negotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   Treaty nations are coded as both beneficiary and payer: they gain formal veto standing (low d on that axis) but bear the practical cost of being an unavoidable, often under-resourced party to every dispute invoking their status, frequently without having sought that role (raising effective d on the cost axis). Federal and provincial governments sit near the beneficiary end because they can invoke the principle without bearing its administrative or political costs directly. Separatist constituents sit near the target end: their expressed democratic will is denied unilateral legitimating force by this reading, which is precisely the reading's structural point.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (treaties predate and were not extinguished by confederation or provincial creation) remains fully live — it is not an obsolete mandate propping up dead function. But the specific application to secession disputes is newer and contested, meaning the reading could still be an instance of a genuine principle being extended into a context (blocking settler-majority secession votes) that its original negotiators may not have anticipated. This is not mandatrophy in the classic sense (function died, form persists) but a live question of scope: is the treaty-consent gate a permanent structural feature of any future secession, or a contingent argument advanced strategically in specific disputes?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_veto_or_strategic_tool,
    'Is the treaty-consent requirement, as actually invoked in secession disputes, a genuine exercise of pre-existing treaty rights by treaty nations acting in their own interest, or is it substantially a tool federal and provincial actors deploy strategically to block secession outcomes they oppose for unrelated reasons?',
    'Track case-by-case whether treaty nations independently initiate the consent objection versus whether federal/provincial parties raise it on the nations'' behalf without prior consultation; examine whether treaty nations receive resources and standing proportionate to the leverage attributed to them, or whether they bear costs of being invoked without corresponding benefit.',
    'If largely nation-initiated and resourced, this reading functions closer to a genuine rope protecting real prior rights; if largely third-party-invoked, the coordination story is closer to cover for federal/provincial obstruction, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_veto_or_strategic_tool, empirical, 'Whether the treaty veto is exercised by treaty nations or on their behalf by non-treaty parties.').

omega_variable(
    scope_of_original_treaty_intent,
    'Did the historic treaties, as negotiated, contemplate or imply a veto over future secession questions between the Crown''s successor governments, or is this an extension of treaty principles into a domain the original parties did not address?',
    'Historical and textual analysis of treaty negotiation records, oral history from treaty nations regarding original understanding, and comparative analysis of how courts have extended treaty principles to novel constitutional questions elsewhere.',
    'If treaties are read as contemplating broad sovereignty protection including secession scenarios, the reading is more clearly a direct application of settled law; if this is a novel extension, the reading is more clearly an evolving legal argument whose legitimacy is still being established.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_original_treaty_intent, conceptual, 'Whether secession-blocking is within or beyond the scope of original treaty intent.').

omega_variable(
    committer_structure_note,
    'This constraint is one reading among four of the secession_legitimacy_boundary kernel (treaty_primacy_reading here; sibling readings are constitutional_impossibility_reading, popular_sovereignty_reading, and grievance_threshold_reading, each a separate constraint story). Where is the disagreement between this reading and the popular_sovereignty_reading actually located?',
    'The disagreement is located specifically at the question of who counts as an original sovereignty-holder whose consent is a precondition for altering territorial status: this reading holds treaty nations are prior and necessary parties; the popular_sovereignty_reading holds the referendum-expressed will of the current provincial population is sufficient on its own. Adjudicating between them requires resolving whether treaty rights survive and bind successor governments'' internal political processes, which is a legal-historical question distinct from either reading''s own framing.',
    'If the treaty-primacy reading prevails, popular_sovereignty_reading''s referendum-based legitimacy claim is directly foreclosed in treaty territory; if popular_sovereignty_reading prevails, this reading''s veto is reduced to a political/moral claim without binding legal force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_note, conceptual, 'Location of the structural disagreement between treaty_primacy_reading and popular_sovereignty_reading within the shared kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the natural-language concept 'legitimate secession' under the secession_legitimacy_boundary kernel. Each sibling authors its own ε from its own reading's premises: this reading (treaty_primacy_reading) holds treaty consent as a hard prior gate (ε=0.58, tangled_rope — real coordination value for treaty nations plus real strategic-extraction risk from non-treaty invokers); the popular_sovereignty_reading is directly foreclosed by this reading wherever treaty territory is implicated, since it treats provincial referendum results as self-legitimating without any treaty-consent gate; the constitutional_impossibility_reading coexists with this reading because both agree secession requires more than unilateral provincial action, differing only on what that additional requirement is (constitutional amendment process vs. treaty consent); the grievance_threshold_reading is influenced but not foreclosed, since a grievance-threshold argument could still be raised by treaty nations themselves as an alternative or supplementary basis for their own claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
