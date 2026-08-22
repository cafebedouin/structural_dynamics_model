% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Subordination Doctrine — Federal Consent Gate on Exit (Constitutional-Subordination Reading)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the sovereignty boundary of a
 *   continental federation: provinces exercise only the powers the
 *   constitution grants them, hold no inherent sovereignty, and may leave the
 *   federation only through a process in which the federal government
 *   participates and consents. This file instantiates the
 *   constitutional_subordination reading of that boundary — the reading that
 *   holds the subordination itself to be legitimate constitutional
 *   architecture, the federal veto over exit to be valid, equalization and
 *   federal climate policy to be proper exercises of federal authority, and
 *   separatism to be constitutionally void absent negotiated amendment. The
 *   epsilon referent is the standing arrangement as this reading assesses it:
 *   the arrangement's actual operation (transfer flows, policy override,
 *   gated exit), not the compact or resource-sovereignty arrangements the
 *   sibling readings would install. Sibling readings are separate constraints
 *   in separate files with their own epsilon values; nothing about them is
 *   averaged into this one. The claimed type and the metrics below were
 *   authored independently: the claim states what this reading takes the
 *   structure to be, the metrics state what the structure's operation
 *   descriptively shows.
 *
 * KEY AGENTS:
 *   - - federal_government: Primary agenda-setter and beneficiary (institutional/arbitrage) — holds the exit veto, transfer conditionality, and dormant disallowance powers
 *   - - resource_exporting_provinces: Primary target (powerful/constrained) — funds the transfer system and absorbs federal regulation of provincially owned resources
 *   - - autonomist_provinces: Secondary target (organized/identity_locked) — twice voted on exit, twice remained inside
 *   - - have_not_provinces: Beneficiary (moderate/constrained) — budgets planned around the transfer system
 *   - - national_minority_communities: Beneficiary (organized/constrained) — shielded from provincial majorities by federal institutions
 *   - - supreme_court_of_canada: Enforcement seat (institutional/analytical) — draws the line case by case and defined the secession process
 *   - - indigenous_nations: Excluded party (organized/trapped) — territories and treaties sit under a boundary they did not draw and do not administer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.54).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.48).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.54).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Subordination Doctrine — Federal Consent Gate on Exit (Constitutional-Subordination Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '4b44894c-965f-4b54-bfc5-7f31f32ac837').
narrative_ontology:cs_kernel_codification('4b44894c-965f-4b54-bfc5-7f31f32ac837', fixed_text).
narrative_ontology:cs_authority_grounding('4b44894c-965f-4b54-bfc5-7f31f32ac837', lineage).
narrative_ontology:cs_interpretation_layer_present('4b44894c-965f-4b54-bfc5-7f31f32ac837').
narrative_ontology:cs_reading_relation('4b44894c-965f-4b54-bfc5-7f31f32ac837', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('4b44894c-965f-4b54-bfc5-7f31f32ac837', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('4b44894c-965f-4b54-bfc5-7f31f32ac837', foundational, provinces_hold_delegated_not_inherent_authority).
narrative_ontology:cs_axiom_status(provinces_hold_delegated_not_inherent_authority, holdable).
narrative_ontology:cs_axiom_grounding('4b44894c-965f-4b54-bfc5-7f31f32ac837', provinces_hold_delegated_not_inherent_authority, conventional).
narrative_ontology:cs_axiom('4b44894c-965f-4b54-bfc5-7f31f32ac837', secondary, exit_requires_federally_consented_amendment).
narrative_ontology:cs_axiom_status(exit_requires_federally_consented_amendment, holdable).
narrative_ontology:cs_axiom_grounding('4b44894c-965f-4b54-bfc5-7f31f32ac837', exit_requires_federally_consented_amendment, conventional).
narrative_ontology:cs_reference_frame('4b44894c-965f-4b54-bfc5-7f31f32ac837', centralized_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('4b44894c-965f-4b54-bfc5-7f31f32ac837', post_patriation_charter_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b44894c-965f-4b54-bfc5-7f31f32ac837', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, have_not_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_minority_communities).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_exporting_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, autonomist_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_paramountcy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, pogg_national_concern_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the boundary through Parliament, cabinet, and the justice department: drafts constitutional amendments, holds the disallowance and reservation powers (dormant but unrepealed), attaches conditions to intergovernmental transfers, and litigates to defend federal regulatory reach. Converts provincial fiscal contribution into allocation control and holds the gate on any province's departure. Leaving the arrangement would mean proposing its own dissolution; instead it shifts among instruments — spending, regulation, litigation — as provincial resistance moves.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, federal_government, beneficiary).

% Own their natural resources under the constitution's resource-ownership clause yet face federal environmental assessment, emissions pricing, and pipeline regulation reaching those resources. Revenues and employment concentrate in the regulated sector, and they contribute heavily to the transfer system. Their levers are political — referendums on equalization, lawsuits against federal statutes, first-ministers' bargaining, alliances with other aggrieved provinces — not legal exit, which the constitution closes to them.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_exporting_provinces, payer,
    powerful, biographical, constrained, national).

% Maintain a full provincial state apparatus — immigration selection, language law, civil law, pension plan — justified by national survival within the federation. The electorate has twice voted on secession; both times the vote stayed inside, and the courts have since defined any future departure as a negotiated constitutional process requiring federal participation. Political life is organized around the question of the province's own constitutional status, which makes the relationship itself part of the province's identity; walking away would dissolve the organizing question, not just the arrangement.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, autonomist_provinces, payer,
    organized, generational, identity_locked, national).

% Receive the largest share of equalization transfers, which fund public services at levels comparable to wealthier provinces. Budgets are planned around the transfer formula; departing the arrangement would mean forfeiting transfers and negotiating trade access from scratch. They defend the transfer system in first-ministers' meetings but hold little power over its design.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, have_not_provinces, beneficiary,
    moderate, biographical, constrained, national).

% Francophone communities outside Quebec and anglophone communities inside Quebec rely on federal institutions — official-languages law, charter protections, federal funding — as a counterweight to provincial majorities. Redrawing the sovereignty boundary along provincial lines would place them under a majority they did not choose. Their practical option is internal migration, at the cost of community continuity built over generations.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Adjudicates where the boundary runs: decided the patriation question, defined the secession process that the clarity regime codified, and tests federal statutes against provincial jurisdiction, most recently on environmental assessment. Its interpretations bind both orders of government. It watches the political struggle from a seat none of the combatants occupy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, observer).

% Hold treaties made with the Crown before and alongside Confederation and assert sovereignty older than either order of government. Any redrawing of the federal-provincial boundary crosses their territories and engages their rights, yet they were not parties to the 1867 settlement and hold no guaranteed seat in the amending formula's routine operation. They intervene in reference cases and negotiate modern treaties, but the boundary question proceeds largely without them.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    organized, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a continental federation together as a single economic and diplomatic unit: prevents exit competition among provinces, guarantees interprovincial mobility and trade, pools defense and currency, and enables redistribution across regions with unequal fiscal capacity.
% TRANSFER_FUNCTION: Moves fiscal capacity from higher-revenue provinces to lower-revenue provinces through federal transfers; moves decision authority over cross-cutting domains — environment, trade, criminal law, emergencies — from provincial legislatures to federal institutions; moves the secession decision from provincial majorities to a federally participated amendment process.
% ABSENT_VOICES: Indigenous nations: treaty-holders whose territories sit under both orders of government and whose rights any boundary redraw engages, yet who were absent from the 1867 settlement and hold no routine seat in the amending formula. Ordinary provincial electorates also deliberate the boundary only at crisis moments; between crises the question is administered by executives and courts.
% DISAPPEARANCE_RATIONALE: If the subordination rule vanished overnight, provinces would assert inherent sovereignty immediately: departure processes would open in Quebec and Alberta, contributing provinces would withhold transfers, federal climate regulation over provincially owned resources would collapse, and the common market would fragment into bilateral provincial deals. Every budget, statute, and intergovernmental agreement presupposes the boundary.
% FOUNDING_PROBLEM: The 1860s problem: British North American colonies facing annexation pressure from the United States, unable to fund their own defense, blocked by intercolonial tariff walls, and destabilized by serial ministerial crises — addressed by pooling sovereignty in a strong central federation with subordinate provinces.
% FOUNDING_PROBLEM_CORROBORATION: Confederation-era historians outside the benefiting parties corroborate the original problems — annexation fear, defense costs, tariff walls — from archival sources. Provincial governments and sovereigntist movements attest that the founding problem as framed is dead and the arrangement now self-perpetuates; federal authorities attest that successor problems (common market, redistribution, cross-border externalities) keep it live. No source outside the benefiting parties attests the original problem as still live — hence contested.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.54: the arrangement moves billions annually out of higher-capacity provinces, overrides provincial policy in domains touching provincially owned resources, and forecloses exit — material extraction by any measure — while the reading assesses part of the transfer flow as the price of the coordination itself. Suppression 0.48: coercion is legal-institutional (court enforcement of paramountcy, the clarity regime gating secession, conditions attached to transfers) rather than physical; the old disallowance weapon is dormant but unrepealed. Theater_ratio 0.33: the machinery mostly works — real judgments, real transfers — but a growing symbolic layer (failed distinct-society clauses, ceremonial reconciliation, provincial sovereignty acts as counter-theater) performs the boundary rather than operating it. Accessibility_collapse 0.60: unilateral exit is closed by the secession reference, but negotiated asymmetry, bilateral devolution, and the amending formula keep channels partly open. Resistance 0.60: two secession referendums, a majority equalization-abolition vote in a contributing province, and sustained litigation against federal assessment statutes. The suppression series traces an enforcement migration, not simple intensification: disallowance-era coercion peaks early, decays as courts and fiscal conditionality take over, then partially rebuilds as the clarity regime codifies the exit gate and emergency and spending instruments return to use. The extractiveness series oscillates on crisis-centralization cycles — wars and pandemics ratchet extraction up, provincial pushback and accommodation packages pull it partway back down — with each cycle settling on a higher plateau than the last; the oscillation tracks external shocks, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the endorsing seats compute differently from the same structure. Resource-exporting provinces experience the boundary as a lock: they fund the system, absorb regulation of assets they own, and hold political but not legal levers. Autonomist provinces experience it as a doubled bind: the relationship is the organizing question of their political identity, so exit is unthinkable and remaining is contested. Have-not provinces and national minority communities experience the same boundary as shelter — the transfer system and federal institutions are what stand between them and provincial-majority rule. Two same-nominal-level actors differentiate sharply: resource-exporting and have-not provinces hold identical constitutional status but opposite directionalities, differentiated by fiscal capacity and by what exit would cost each. The court seat sees enforcement mechanics rather than burden; the excluded seat sees a line drawn across its territory without its consent. The authored claim comes from the endorsing reading's seat and does not adjudicate this divergence — the engine computes it.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government sits nearest the beneficiary pole: it wrote the rules, collects the conditionality leverage, and holds the veto — d near 0.0. Have-not provinces receive net transfers and defend the system: d near 0.1. National minority communities gain protection from the federal layer: d near 0.15. Resource-exporting provinces pay transfers, absorb policy override over owned resources, and cannot legally exit: d near 0.85. Autonomist provinces carry the identity-lock premium — trapped not only by law but by a self-concept constituted through the constitutional question: d near 0.8, and if that identity frame ever broke (generational turnover, referendum fatigue), their exit options would shift from identity_locked toward merely constrained and their computed extraction would ease. The court holds an analytical seat; indigenous nations, though excluded from administration, are structurally targeted by any boundary redraw and would compute high if seated. The receipt surface records where the gains land: the federal government — it converts provincial contribution into allocation control, conditionality leverage, and the exit veto itself; transfer dollars pass through to have-not provinces, but the extractive lever stays federal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — annexation risk, unaffordable colonial defense, intercolonial tariff walls — is dead as originally framed. What persists are successor problems the founders did not face: a common market worth defending, regional fiscal disparity requiring pooled redistribution, cross-border environmental externalities no province can regulate alone. Because the mandate migrated rather than died, mandatrophy is not resolved and the founding-problem status is contested. Classifying the structure as a hybrid preserves both truths against two symmetrical mislabels: the reading's own temptation to dress the arrangement as pure coordination (which would erase the extraction the sibling readings organize around), and the siblings' temptation to dress it as pure extraction (which would erase the coordination that keeps ten provinces in one economic space and four million people inside a country they twice declined to leave). The hybrid claim also keeps the enforcement requirement visible: this is not a self-sustaining equilibrium but an actively adjudicated line, and if enforcement capacity decayed faster than preference consolidated, the structure would drift toward inertial persistence — performance of subordination without the machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the provincial_sovereignty_boundary kernel: does the constitutional-subordination reading, the compact-federalism reading, or the resource-sovereignty-primacy reading correctly characterize where provincial authority ends?',
    'Constitutional amendment outcomes, Supreme Court composition shifts, and referendum results: each reading predicts a different resolution path for the next boundary crisis — federally consented amendment, compact renegotiation, or a resource-jurisdiction carve-out.',
    'If compact_federalism prevails, the exit gate becomes a negotiable compact term and the federal veto weakens; if resource_sovereignty_primacy prevails, federal climate regulation over provincially owned resources loses legitimacy and the victim set contracts to non-resource domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the sovereignty kernel governs the boundary.').

omega_variable(
    equalization_coordination_or_extraction,
    'Is the equalization arm of the arrangement a coordination mechanism correcting fiscal-capacity disparity across regions, or extraction from contributing provinces routed through Ottawa?',
    'Fiscal-capacity convergence studies and mobility-response data: if recipient provinces converge without behavioral distortion and contributors show no systematic net loss once common-market gains are counted, the coordination reading strengthens.',
    'A coordination verdict supports the hybrid balance claimed here; an extraction verdict pushes the fiscal arm toward pure-extraction dynamics and raises effective extraction for contributing seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_coordination_or_extraction, empirical, 'Whether the transfer system coordinates or extracts.').

omega_variable(
    consent_gate_enforceability,
    'Can the federal consent gate on secession actually hold against a determined provincial majority, or is the clarity regime a procedural screen over an unavoidable negotiation?',
    'Scenario analysis of a clear secession-majority vote: financial-market reaction, third-party state recognition behavior, and the federal government''s actual bargaining posture would reveal whether the gate binds or merely delays.',
    'If the gate is unenforceable, measured suppression is largely performative, persistence rests on preference rather than coercion, and the arrangement''s dependence on active enforcement drops sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_gate_enforceability, empirical, 'Whether the exit gate binds in the crisis case it exists for.').

omega_variable(
    indigenous_sovereignty_frame,
    'Does the two-order sovereignty boundary misdescribe a territory where Indigenous sovereignty predates and parallels both federal and provincial orders?',
    'Modern-treaty jurisprudence and UNDRIP implementation: if courts and legislation increasingly recognize a third order with territorial implications, the binary boundary framing collapses.',
    'Recognition would restructure the party sets — indigenous_nations move from excluded voices to direct parties — and reopen the boundary question this reading treats as settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_frame, conceptual, 'Whether the binary federal-provincial frame is complete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 155).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(prov_tr_t0, observed).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.17).
narrative_ontology:measurement_basis(prov_tr_t25, observed).
narrative_ontology:measurement(prov_tr_t50, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(prov_tr_t50, observed).
narrative_ontology:measurement(prov_tr_t75, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 75, 0.23).
narrative_ontology:measurement_basis(prov_tr_t75, observed).
narrative_ontology:measurement(prov_tr_t100, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 100, 0.27).
narrative_ontology:measurement_basis(prov_tr_t100, observed).
narrative_ontology:measurement(prov_tr_t125, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 125, 0.31).
narrative_ontology:measurement_basis(prov_tr_t125, observed).
narrative_ontology:measurement(prov_tr_t155, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 155, 0.33).
narrative_ontology:measurement_basis(prov_tr_t155, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(prov_be_t0, observed).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.43).
narrative_ontology:measurement_basis(prov_be_t25, observed).
narrative_ontology:measurement(prov_be_t50, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 50, 0.46).
narrative_ontology:measurement_basis(prov_be_t50, observed).
narrative_ontology:measurement(prov_be_t75, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 75, 0.53).
narrative_ontology:measurement_basis(prov_be_t75, observed).
narrative_ontology:measurement(prov_be_t100, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 100, 0.56).
narrative_ontology:measurement_basis(prov_be_t100, observed).
narrative_ontology:measurement(prov_be_t125, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 125, 0.49).
narrative_ontology:measurement_basis(prov_be_t125, observed).
narrative_ontology:measurement(prov_be_t155, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 155, 0.54).
narrative_ontology:measurement_basis(prov_be_t155, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(prov_su_t0, observed).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(prov_su_t25, observed).
narrative_ontology:measurement(prov_su_t50, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 50, 0.51).
narrative_ontology:measurement_basis(prov_su_t50, observed).
narrative_ontology:measurement(prov_su_t75, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 75, 0.49).
narrative_ontology:measurement_basis(prov_su_t75, observed).
narrative_ontology:measurement(prov_su_t100, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 100, 0.41).
narrative_ontology:measurement_basis(prov_su_t100, observed).
narrative_ontology:measurement(prov_su_t125, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 125, 0.45).
narrative_ontology:measurement_basis(prov_su_t125, observed).
narrative_ontology:measurement(prov_su_t155, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 155, 0.48).
narrative_ontology:measurement_basis(prov_su_t155, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The colloquial label 'provincial sovereignty' decomposes into three structurally distinct claims with different epsilon values: this file authors the constitutional-subordination reading (delegated authority, gated exit, epsilon ~0.54 from the endorsing seat); compact_federalism authors residual-sovereignty-from-compact (negotiable exit, different victim set — the federal center becomes the extractor); resource_sovereignty_primacy authors section-92A-based territorial sovereignty (epsilon concentrated on federal climate authority over resources). The subordination reading is upstream in the family: it supplies the constitutional text and judicial doctrine that the other two readings must amend, reinterpret, or carve exceptions from, so its influence edges point at both siblings. Measuring 'provincial sovereignty' with the observables of one reading changes epsilon because it changes the constraint — the decomposition, not a measurement parameter, resolves the ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
