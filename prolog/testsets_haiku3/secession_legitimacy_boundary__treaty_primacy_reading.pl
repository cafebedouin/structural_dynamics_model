% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty Primacy Constraint on Secession Legitimacy
 *   domain: political_economy/federalism/indigenous_rights
 *
 * SUMMARY:
 *   This is the treaty primacy reading of the contested kernel about
 *   secession legitimacy (kernel_id: secession_legitimacy_boundary). The
 *   constraint asserts that Indigenous treaty rights, dating to
 *   pre-Confederation treaties between Indigenous nations and the British
 *   Crown, predate and supersede both federal and provincial constitutional
 *   authority. Under this reading, no secession is legitimate without the
 *   consent of treaty-holding Indigenous nations whose territories intersect
 *   with secessionist boundaries. The reading frames Indigenous nations as
 *   essential veto holders in any constitutional reorganization of the state,
 *   anchoring legitimacy to prior treaty relationships rather than to federal
 *   constitutional text, democratic will, or grievance thresholds. The
 *   measurement interval (0–40) tracks extractiveness rise (0.52→0.68) as the
 *   constraint's enforcement machinery has been elaborated through court
 *   decisions (Sparrow, Delgamuukw, Haida Nation), Indigenous mobilization,
 *   and federal recognition of treaty supremacy in the Constitution Act,
 *   1982. This is a period of consolidation and institutional embedding
 *   rather than breakdown. Theater ratio growth (0.28→0.41) reflects the
 *   increasing use of treaty language in constitutional discourse and
 *   defensive elaboration of the constraint's legitimacy even as its
 *   substantive veto authority remains contested by secessionist movements.
 *
 * KEY AGENTS:
 *   - treaty_holding_indigenous_nations: Organized/civilizational — set and defend the terms of consent, hold civilization-scale interest in protecting treaty relationships across generations
 *   - federal_government: Institutional/generational — custodian of treaty interpretation, consolidates authority by invoking treaty supremacy
 *   - provincial_secessionists: Powerful/biographical — blocked from unilateral exit, face raised transaction costs
 *   - non_treaty_provincial_residents: Moderate/biographical — pay through blocked secession but benefit from stability
 *   - other_provincial_governments: Institutional/generational — all provinces become more stable as unilateral exit is foreclosed
 *   - courts_and_constitutional_interpreters: Institutional/generational + analytical — interpret and operationalize the constraint through jurisprudence
 *   - international_indigenous_rights_advocates: Moderate/global — amplify the constraint's legitimacy through UN forums and transnational networks
 *   - excluded_provincial_immigrants: Powerless/biographical — no formal standing in treaty negotiations but jurisdictionally bound by outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.72).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy Constraint on Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/indigenous_rights").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '001f3b1b-baee-44a1-8b74-48cac49ea21b').
narrative_ontology:cs_kernel_codification('001f3b1b-baee-44a1-8b74-48cac49ea21b', fixed_text).
narrative_ontology:cs_authority_grounding('001f3b1b-baee-44a1-8b74-48cac49ea21b', lineage).
narrative_ontology:cs_interpretation_layer_present('001f3b1b-baee-44a1-8b74-48cac49ea21b').
narrative_ontology:cs_reading_relation('001f3b1b-baee-44a1-8b74-48cac49ea21b', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('001f3b1b-baee-44a1-8b74-48cac49ea21b', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('001f3b1b-baee-44a1-8b74-48cac49ea21b', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('001f3b1b-baee-44a1-8b74-48cac49ea21b', foundational, treaty_primacy_over_constitutional_hierarchy).
narrative_ontology:cs_axiom_status(treaty_primacy_over_constitutional_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('001f3b1b-baee-44a1-8b74-48cac49ea21b', treaty_primacy_over_constitutional_hierarchy, deontological).
narrative_ontology:cs_axiom('001f3b1b-baee-44a1-8b74-48cac49ea21b', foundational, indigenous_sovereignty_precedes_modern_statehood).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_precedes_modern_statehood, holdable).
narrative_ontology:cs_axiom_grounding('001f3b1b-baee-44a1-8b74-48cac49ea21b', indigenous_sovereignty_precedes_modern_statehood, deontological).
narrative_ontology:cs_reference_frame('001f3b1b-baee-44a1-8b74-48cac49ea21b', pre_confederation_treaty_sovereignty).
narrative_ontology:cs_drift_state('001f3b1b-baee-44a1-8b74-48cac49ea21b', contemporary_constitutional_modernization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('001f3b1b-baee-44a1-8b74-48cac49ea21b', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, treaty_holding_indigenous_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secessionists).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, non_treaty_provincial_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, non_treaty_provincial_residents).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, other_provincial_governments).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, international_indigenous_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold pre-Confederation sovereignty claims grounded in treaties signed before Confederation and international legal doctrines of Aboriginal rights. Their territorial claims and governance authority are explicitly invoked as veto points in any separatist project. They set the terms of secession legitimacy through the framework that treaties predate and supersede federal/provincial authority. Their ability to enforce the veto depends on federal recognition of treaty supremacy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_holding_indigenous_nations, agenda_setter,
    organized, civilizational, trapped, national).

% Benefits from treaty primacy doctrine because it keeps secession decisions anchored to negotiation with the federal center rather than provincial unilateralism. The doctrine reinforces federal jurisdiction over Indian affairs and treaty interpretation. Federal government collects legitimacy authority from being the putative treaty custodian and can use Indigenous consent requirements as a negotiation tool.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the constraint because they cannot unilaterally claim secession legitimacy: any separatist project must now negotiate with Indigenous nations whose territorial claims may overlap or encompass secessionist ambitions. This dramatically raises transaction costs and introduces a veto player outside provincial control. Secessionists see the treaty requirement as an external imposition that subordinates provincial democratic will to pre-Confederation Indigenous claims.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secessionists, payer,
    powerful, biographical, constrained, regional).

% Pay through delayed or blocked secession: if a referendum passes but Indigenous nations withhold consent, provincial exit is delegitimized regardless of provincial majority preference. They also indirectly benefit if the constraint preserves territorial integrity and prevents fragmentation that could destabilize economic arrangements. Their choice set is bounded by the intersection of provincial boundaries and treaty territories.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, non_treaty_provincial_residents, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, non_treaty_provincial_residents, beneficiary).

% Benefit from a constitutional doctrine that raises the bar for secession (applies to all provinces equally). Each province becomes more stable because unilateral exit is foreclosed. Provinces without significant treaty territories benefit disproportionately because they face less active constraint, while secessionist provinces face higher barriers.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, other_provincial_governments, beneficiary,
    institutional, generational, mobile, national).

% Interpret and enforce the treaty supremacy doctrine through jurisprudence. They hold the canonical authority to read treaties, declare their scope, and rule on whether Indigenous consent is procedurally required for secession. Their decisions operationalize the constraint or limit its reach. They serve both as agenda-setter (declaring the rule) and observer (analyzing how it applies).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, courts_and_constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, courts_and_constitutional_interpreters, observer).

% Benefit from a constraint that embeds international norms of Indigenous sovereignty and treaty primacy into domestic constitutional logic. The constraint vindicates their advocacy position that Indigenous peoples hold pre-state rights that supersede modern state boundaries. They amplify the constraint's legitimacy through UN forums and global legal networks.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_indigenous_rights_advocates, beneficiary,
    moderate, generational, mobile, global).

% Have no formal standing in the treaty framework (they arrived post-treaty, post-secession debate) but are jurisdictionally bound by the outcome. If a province cannot secede without Indigenous consent and Indigenous nations deny consent, these populations remain in the federal union against their residence-based preference. Their voice is absent from the treaty negotiation even though their future is determined by it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, excluded_provincial_immigrants, excluded,
    powerless, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, treaty_holding_indigenous_nations).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes Indigenous nations as structural veto holders in any secession project, ensuring territorial and governance claims are recognized in negotiation rather than overridden by provincial majority vote. Coordinates federal, Indigenous, and provincial interests by making all three seats necessary to legitimate exit.
% TRANSFER_FUNCTION: Moves legitimacy authority from provincial electorates to a three-party negotiation (federal + Indigenous nations + provincial majority). Transfers procedural power from unilateral provincial declaration to consensual interstate/inter-nation agreement. Transfers veto power from federal government alone to a structure that includes Indigenous nations as independent actors.
% ABSENT_VOICES: Non-treaty residents within secessionist provinces (those without standing in treaty frameworks) have no formal voice in determining whether they exit the federation; they are excluded by the constraint's focus on treaty-holding nations as the veto player. Recent immigrants and non-Indigenous provincial residents would argue for residence-based democratic legitimacy but are kept structurally outside the treaty negotiation. Also absent: rival secessionist movements in other provinces that might benefit from a precedent of unilateral exit.
% DISAPPEARANCE_RATIONALE: If treaty primacy and the Indigenous veto on secession disappeared overnight, provincial secession could proceed by provincial referendum alone. The constitutional landscape would shift from a three-party negotiation (federal + Indigenous + provincial) back to a two-party (federal + provincial) model. Indigenous territorial and governance claims would lose the structural leverage the treaty framework provides, and the constitutional hierarchy would revert to federal-provincial federalism without Indigenous sovereignty embedded in the legitimacy test. The reorganization of state boundaries would accelerate dramatically.
% FOUNDING_PROBLEM: Pre-Confederation treaties created binding relationships between Indigenous nations and the Crown that predated the 1867 constitutional structure. Modern secession doctrine must account for the fact that provincial territories are already claimed under treaty; unilateral provincial exit ignores these prior commitments and endangers Indigenous governance and resource rights. The founding problem is that Indigenous sovereignty predates the constitutional federation and cannot be overridden by constitutional amendments that Indigenous nations have not consented to.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous nations and their legal advocates attest the treaties remain binding and predate Confederation. Federal governments have variably acknowledged treaty obligations (explicit in the Constitution Act, 1982, Section 35, but inconsistently implemented). Provincial secessionists dispute that 150+ year-old treaties constrain modern democratic will. Appellate courts have ruled (R. v. Sparrow, Delgamuukw, Haida Nation v. British Columbia) that Aboriginal rights and treaties receive constitutional recognition, lending corroboration from outside the Indigenous advocacy community, though judicial consensus on the secession veto specifically remains incomplete. International human rights bodies (UN Permanent Forum on Indigenous Issues, Inter-American Commission on Human Rights) have endorsed the principle that Indigenous nations hold veto rights over state reorganization, providing transnational corroboration beyond Canadian institutional actors.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68 final) is high because the constraint blocks secessionist exit and raises transaction costs through mandatory three-party negotiation. The extraction is procedural (blocked exit, delayed legitimacy) rather than monetary, but it is real. Suppression (0.72) is high because the constraint persists only through active exclusion of unilateral secession as a legitimate path; without suppressive institutional work (court rulings, federal invocation of treaty authority, Indigenous mobilization), the constraint would collapse and secession would become a binary federal-provincial negotiation. Theater ratio (0.41, growing from 0.28) indicates moderate defensive elaboration: the real coordination function (three-party negotiation instead of unilateral action) is genuine, and enforcement through courts interpreting treaties is real, but as the constraint faces modernization pressure from secessionists and constitutional scholars, institutional actors invest increasing effort in defending treaty scope and veto legitimacy through framing, interpretation, and discourse rather than through direct institutional action. The plateau at t=30 suggests the constraint has reached equilibrium in its defensive elaboration. Accessibility collapse (0.79) is high because once treaty primacy is accepted, the alternative (unilateral provincial secession) becomes constitutionally unthinkable for courts and federal actors; the logic of the constraint narrows the policy space sharply. Resistance (0.58) is moderate because provincial secessionists actively resist the veto through counter-sovereignty arguments while other stakeholders have less direct interest in contesting it. The measurement series use one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and payer seats compute radically different types from the same structural facts. From Indigenous nations and courts: the constraint is coordination protecting pre-existing sovereignty claims — legitimate, foundational, defensive. From secessionist payers: the constraint is an externally imposed veto that subordinates modern democratic will to historical claims. The engine computes per-seat classification: Indigenous nations get low directionality (0.05–0.15, strong beneficiary) and should compute as rope or tangled_rope depending on whether they are viewed as coordinating or extracting. Secessionist payers get high directionality (0.80–0.90, strong target) and should compute as snare or tangled_rope depending on whether they believe the coordination story. The federal government gets moderate-low directionality (0.20–0.30, collector of legitimacy authority) and sits near symmetric or weak-beneficiary. This divergence IS the measurement the system exists to capture: the claimed type (tangled_rope) and the metrics describe a constraint with both real coordination (three-party negotiation) and real asymmetric extraction (blocked secessionist exit). The disagreement about whether the extraction is legitimate (treaty protection) or illegitimate (democratic suppression) is the kernel contest, routed through separate constraint stories for the sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are treaty-holding Indigenous nations (primary) and federal government (secondary). Victims are provincial secessionists and non-treaty provincial residents. Indigenous nations benefit from the veto power and constitutional recognition of treaty supremacy; their directionality is near the beneficiary end (d~0.10). Federal government benefits from custodianship of treaty interpretation and from having Indigenous veto as a negotiation tool; directionality is low-to-moderate (d~0.25). Provincial secessionists are victimized by the veto and raised transaction costs; directionality is near target end (d~0.85). Non-treaty residents are structurally ambiguous (secondary_role: payer + beneficiary) because they pay through blocked secession but benefit from territorial continuity and federal constitutional order; directionality is near symmetric (d~0.50). The constraint's power atoms differentiate institutional (federal, courts) from organized (Indigenous nations, secessionist coalitions) from powerless (non-treaty residents excluded from negotiation). Time horizons differentiate civilizational (Indigenous nations) from generational (federal, courts, other provinces) from biographical (secessionists, residents). Exit options differentiate trapped (Indigenous nations locked into territories and treaty relationships) from identity_locked (secessionist populations whose identity is fused with provincial sovereignty claims) from constrained (non-treaty residents and federal government) from arbitrage (international advocates who can shift forums).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-Confederation treaties remain binding and constrain modern state reorganization) has contested status — Indigenous nations affirm it as live; secessionists deny it or minimize it; courts have partially validated it through Section 35 of the Constitution Act, 1982, but have not yet ruled directly on the secession veto. The disappearance verdict is world_rearranges because secession legitimacy is fundamentally altered if treaty veto disappears. This mismatch (contested founding problem + world_rearranges verdict) does not trigger mandatrophy because the problem's contestation is a live political dispute between real coalitions (Indigenous nations vs. secessionists), not a sign that the problem is obsolete. The constraint persists because Indigenous nations actively maintain it through legal action, mobilization, and assertion of treaty rights. Theater ratio growth (0.28→0.41) does reflect increased defensive elaboration — institutional actors (courts, federal government, Indigenous advocates) invest more in framing and interpretation to defend treaty scope against modernization pressure and secessionist counter-arguments — but this is the normal trajectory of a contested constraint whose legitimacy is under political pressure. The constraint's primary function (veto on unilateral secession) remains operationally intact; the theater growth indicates the veto is defended rather than automated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_scope_contestation,
    'Do historical treaties between Indigenous nations and the Crown extend to include a veto over modern secession, or do they address only land use, trade, and historic governance relationships?',
    'Appellate court interpretation of treaty texts in the context of a secession reference; expert historical and linguistic analysis of original treaty intentions; consultation with Indigenous nations on their own interpretation of treaty scope.',
    'If treaties are interpreted narrowly (historic land/trade only), the veto dissolves and secession becomes a federal-provincial negotiation. If interpreted broadly (ongoing sovereignty relationships), the veto holds and transforms the secession legitimacy test into a three-party gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_scope_contestation, conceptual, 'Whether treaty relationships extend to veto rights over modern state reorganization.').

omega_variable(
    consent_aggregation_ambiguity,
    'Does secession require consent from all treaty-holding nations or only from those territorially affected by the secession boundary?',
    'Constitutional court ruling on the scope of the consent requirement; negotiation precedent from any actual secession reference.',
    'Universal consent requirement (all nations) vastly increases extraction and veto power; affected-only requirement significantly reduces it. This determines whether the constraint operates as a true veto (small number of nations can block) or a negotiation (affected parties must agree but others need not).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_aggregation_ambiguity, conceptual, 'The scope of the Indigenous consent requirement determines veto concentration.').

omega_variable(
    legitimacy_source_reading_contest,
    'This reading claims treaty relationships are the foundational source of secession legitimacy. The sibling reading treaty_primacy_reading claims constitutional procedure (federal supremacy over constitutional amendment) is foundational; popular_sovereignty_reading claims democratic will is foundational; grievance_threshold_reading claims suffered injustice is foundational. Which source is actually authoritative?',
    'No empirical resolution — this is a foundational normative disagreement between readings. Operationally resolved by observing which reading courts, Indigenous nations, federal governments, and provincial governments treat as dispositive in practice; that convergence settles the dispute institutionally even if philosophical disagreement persists.',
    'This reading treats treaty relationships as foundational and pre-constitutional. A sibling reading that treats the Constitution as foundational would subordinate treaties to constitutional procedure. The two readings cannot both be fully true within a single unified framework — they offer different constitutional hierarchies. This is the committer-level uncertainty routed through the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_reading_contest, conceptual, 'Kernel contest: which normative source is foundational for secession legitimacy?').

omega_variable(
    enforcement_capacity_federal_indigenous,
    'Can the federal government unilaterally enforce the treaty veto against provincial secession, or does enforcement depend on Indigenous nations actively exercising the veto through courts and international forums?',
    'Test case: a secession reference where the federal government invokes treaty primacy but Indigenous nations are inactive or divided; observation of whether the constraint persists or collapses without active Indigenous participation.',
    'If federal enforcement suffices, the constraint is a stable federal-Indigenous coordination mechanism. If Indigenous nations must actively defend the veto, the constraint is more fragile and depends on sustained Indigenous political will rather than institutional automation — this would lower effective suppression and increase resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_federal_indigenous, empirical, 'Whether treaty veto enforcement is institutionally automated or depends on active Indigenous mobilization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(sece_tr_t5, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(sece_tr_t15, observed).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(sece_tr_t20, observed).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(sece_tr_t25, observed).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sece_tr_t30, observed).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sece_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(sece_be_t5, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(sece_be_t15, observed).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(sece_be_t20, observed).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(sece_be_t25, observed).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(sece_be_t30, observed).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sece_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(sece_su_t5, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(sece_su_t15, observed).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(sece_su_t20, observed).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(sece_su_t25, observed).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(sece_su_t30, observed).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(sece_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__treaty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the secession_legitimacy_boundary kernel. The kernel contest addresses what legitimates secession: prior treaty relationships (treaty_primacy_reading, this constraint), constitutional procedure (constitutional_impossibility_reading), democratic will (popular_sovereignty_reading), or suffered injustice (grievance_threshold_reading). Each reading instantiates a different constraint with distinct ε values, victim sets, and stakeholder structures. The readings coexist in contemporary Canadian constitutional debate, held by different coalitions (Indigenous nations defend treaty primacy; secessionists champion popular sovereignty; courts prioritize constitutional procedure; human rights advocates invoke grievance thresholds). The network links document how each reading's institutional consolidation affects the others' viability. A court decision clarifying treaty scope would directly influence the ε and victim set of this constraint and indirectly pressure the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
