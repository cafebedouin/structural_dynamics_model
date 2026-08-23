% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Treaty Primacy Barrier to Unilateral Secession
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the treaty_primacy_reading of the
 *   secession_legitimacy_boundary kernel. The reading asserts that Indigenous
 *   treaty rights — predating Confederation and affirmed in Section 35 of the
 *   Constitution Act, 1982 — structurally supersede both federal and
 *   provincial sovereignty claims. No secession of a province (or region)
 *   from Canada is legitimate without the free, prior, and informed consent
 *   of affected Indigenous treaty holders. The constraint operates through
 *   Supreme Court jurisprudence (Sparrow, Haida, Tsilhqot'in, Mikisew) and
 *   the Crown's duty to consult and accommodate. It coordinates the
 *   federation by requiring Indigenous consent for constitutional rupture,
 *   but extracts from separatist movements (blocking unilateral exit) and
 *   from Indigenous nations themselves (bearing the cost of federal stability
 *   without full self-determination). The claimed type is tangled_rope:
 *   genuine coordination (nation-to-nation treaty federalism) plus asymmetric
 *   extraction (secessionist movements blocked, Indigenous consent
 *   instrumentalized for Crown sovereignty).
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: Primary beneficiaries and partial payers (moderate/identity_locked) — hold treaty rights, constrained by Crown interpretation
 *   - federal_government: Agenda setter (institutional/arbitrage) — administers constitutional framework, benefits from territorial integrity
 *   - provincial_governments: Payers (powerful/constrained) — barred from unilateral secession, resource authority conditioned on treaty compliance
 *   - separatist_movements: Payers (organized/trapped) — Quebec sovereignty, Western separatism blocked by treaty consent requirement
 *   - supreme_court: Observer (analytical/analytical) — interprets Section 35, defines consent vs. consultation boundary
 *   - resource_corporations: Payers (powerful/mobile) — project approvals gated by treaty consent, capital can exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.78).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy Barrier to Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'b1de5b45-29d1-431c-b39c-991273161084').
narrative_ontology:cs_kernel_codification('b1de5b45-29d1-431c-b39c-991273161084', formalized).
narrative_ontology:cs_authority_grounding('b1de5b45-29d1-431c-b39c-991273161084', lineage).
narrative_ontology:cs_interpretation_layer_present('b1de5b45-29d1-431c-b39c-991273161084').
narrative_ontology:cs_reading_relation('b1de5b45-29d1-431c-b39c-991273161084', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1de5b45-29d1-431c-b39c-991273161084', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('b1de5b45-29d1-431c-b39c-991273161084', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('b1de5b45-29d1-431c-b39c-991273161084', foundational, treaty_rights_anterior_to_crown_sovereignty).
narrative_ontology:cs_axiom_status(treaty_rights_anterior_to_crown_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b1de5b45-29d1-431c-b39c-991273161084', treaty_rights_anterior_to_crown_sovereignty, deontological).
narrative_ontology:cs_axiom('b1de5b45-29d1-431c-b39c-991273161084', foundational, indigenous_consent_required_for_constitutional_rupture).
narrative_ontology:cs_axiom_status(indigenous_consent_required_for_constitutional_rupture, holdable).
narrative_ontology:cs_axiom_grounding('b1de5b45-29d1-431c-b39c-991273161084', indigenous_consent_required_for_constitutional_rupture, deontological).
narrative_ontology:cs_axiom('b1de5b45-29d1-431c-b39c-991273161084', secondary, crown_honour_binds_interpretation_to_indigenous_understanding).
narrative_ontology:cs_axiom_status(crown_honour_binds_interpretation_to_indigenous_understanding, holdable).
narrative_ontology:cs_axiom_grounding('b1de5b45-29d1-431c-b39c-991273161084', crown_honour_binds_interpretation_to_indigenous_understanding, conventional).
narrative_ontology:cs_reference_frame('b1de5b45-29d1-431c-b39c-991273161084', treaty_nation_to_nation_framework).
narrative_ontology:cs_drift_state('b1de5b45-29d1-431c-b39c-991273161084', contemporary_reconciliation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1de5b45-29d1-431c-b39c-991273161084', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, resource_corporations).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, treaty_primacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, nation_to_nation_relationship).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, crown_honour_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty rights predating Confederation affirmed in Section 35. The constraint protects their right to consent to constitutional rupture affecting their territories. However, the Crown's duty-to-consult framework rarely translates into veto power; consent is proceduralized. They cannot exit the treaty relationship without abandoning nationhood and territorial rights. They bear costs when treaty infringement is justified for 'national unity' (resource projects, infrastructure). Resource revenues from their lands flow largely to federal/provincial coffers.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations, payer).

% Administers the constitutional framework including Section 35 interpretation. Benefits from territorial integrity — treaty consent requirement blocks provincial secession that would fragment the federation. Controls the interpretation machinery (Supreme Court appointments, legislative frameworks like Clarity Act). Can negotiate modern treaties but retains ultimate authority over infringement justification. Exit is arbitrage-grade: the federal state itself is the constraint's administrator.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Exercise jurisdiction over natural resources and land management but are constrained by treaty obligations and duty to consult. Cannot unilaterally secede or authorize projects affecting treaty rights without federal/Indigenous alignment. Bear political and economic costs of treaty compliance and consultation processes. Exit is constrained: leaving Confederation requires negotiation with federal government AND Indigenous consent under this reading.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, payer,
    powerful, biographical, constrained, regional).

% Political movements seeking provincial/regional secession (Quebec sovereignty, Western separatism). Blocked by the treaty consent requirement — Indigenous nations in their claimed territory hold veto or near-veto. Their democratic mandate (referendum majority) is structurally insufficient. No legal exit path exists without Indigenous consent. Some movements attempt to negotiate with Indigenous nations; others reject treaty applicability. Trapped by the constraint's legal architecture.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, separatist_movements, payer,
    organized, biographical, trapped, regional).

% Interprets Section 35 and defines the consent/consultation boundary through jurisprudence (Sparrow justification test, Haida duty to consult, Tsilhqot'in title, Mikisew Crown honour). Does not collect extraction nor bear costs directly. Its rulings determine the constraint's operational extractiveness. Analytical seat: sees full structure but is also part of the enforcement machinery.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, supreme_court, observer,
    analytical, generational, analytical, national).

% Extractive industries (mining, oil/gas, forestry, hydro) requiring project approvals on treaty lands. Treaty consent requirement adds regulatory uncertainty and cost. Can exit by moving capital to jurisdictions with weaker Indigenous rights regimes (mobile exit). Some negotiate benefit agreements; others lobby for legislative overrides. Bear costs of delay, consultation, and revenue sharing but have exit options Indigenous nations lack.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, resource_corporations, payer,
    powerful, biographical, mobile, global).

% Indigenous peoples without historic treaties (e.g., many BC nations, urban Indigenous populations). The treaty_primacy_reading centers treaty rights, potentially marginalizing inherent rights claims outside treaty framework. Would object to treaty-centric framing that privileges treaty-holders. Structurally excluded from the constraint's beneficiary set but bear its suppression (provincial/federal authority still constrains them).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, non_treaty_indigenous_peoples, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the federation by requiring Indigenous consent for constitutional rupture, translating nation-to-nation treaty relationships into a legal barrier against unilateral secession. Solves the problem of competing sovereignty claims (federal, provincial, Indigenous) by establishing treaty rights as the anterior authority.
% TRANSFER_FUNCTION: Moves secession legitimacy from provincial democratic majorities and federal constitutional authority to Indigenous treaty holders. Transfers veto power over constitutional dissolution to treaty nations. Transfers regulatory control over resource development on treaty lands from provinces to shared Crown-Indigenous decision-making. Transfers political risk from separatist movements to the constraint's enforcement machinery (courts, negotiation tables).
% ABSENT_VOICES: Non-treaty Indigenous peoples (inherent rights holders outside treaty framework) are excluded — the treaty-centric reading renders their claims invisible. Métis communities with unresolved land claims. Future generations of Indigenous nations whose consent is binding today. International law observers (UNDRIP Committee) who would frame consent as free, prior, informed consent — not the Crown's duty-to-consult.
% DISAPPEARANCE_RATIONALE: If the treaty consent requirement vanished overnight, Quebec could secede via referendum majority alone (popular_sovereignty_reading), Western separatism would face only Clarity Act thresholds, resource projects on treaty lands would proceed via provincial permitting only, and the Crown's Section 35 obligations would revert to pre-1982 common law (sui generis fiduciary duty, no constitutional status). The federation's territorial integrity would rest solely on federal/provincial negotiation — Indigenous nations would lose their strongest constitutional lever.
% FOUNDING_PROBLEM: The 1982 constitutional patriation excluded Indigenous nations and Quebec. Section 35 was inserted as a 'promise' to recognize existing treaty and Aboriginal rights, but its scope was undefined. The founding problem was: how to legitimize a Canadian constitutional order that claims sovereignty over Indigenous territories without Indigenous consent? The treaty_primacy_reading answers: by making treaty rights the supreme constraint on any further constitutional rupture.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous nations (Assembly of First Nations, Inuit Tapiriit Kanatami, Métis National Council) attest the founding problem persists — Section 35 recognized rights but did not resolve colonial sovereignty. Federal government (successive administrations) attests Section 35 solved the founding problem by constitutionalizing rights. Supreme Court (Reference Re Secession, Tsilhqot'in) attests the problem is partially solved: treaty rights are constitutional but operate within Crown sovereignty. Independent legal scholars (Borrows, Macklem, Webber) corroborate the contested status: the constitutional order's legitimacy remains incomplete without Indigenous consent to the constitutional order itself.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the constraint's dual character: it blocks secessionist claims (extraction from separatists) while also constraining Indigenous self-determination within Crown sovereignty (extraction from treaty nations). Suppression (0.78) is high because the Clarity Act and Reference Re Secession of Quebec legally foreclose unilateral exit — alternatives collapse once the constraint is understood. Theater ratio (0.22) is low-moderate: courts genuinely enforce treaty rights, but the duty-to-consult framework often stops short of veto, creating performative consultation. Accessibility collapse (0.72) is high for separatists (no legal path without Indigenous consent) but lower for Indigenous nations (some negotiation space). Resistance (0.68) is substantial: Quebec nationalist resistance to 'veto' framing, Western alienation, and Indigenous rejection of consultation-as-substitute-for-consent. The measurement series shows rising extractiveness post-1982 as Section 35 litigation expanded treaty scope, stabilizing after Tsilhqot'in (2014).
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is genuine coordination: it translates nation-to-nation treaties into a workable federalism. From the Indigenous seat, it is extraction wearing coordination's clothes: consent is required but rarely determinative; the Crown's 'honour' is the enforcement mechanism. From the separatist seat, it is pure suppression: a foreign legal order (treaties they never signed) blocks their democratic choice. The engine computes these divergences from the structural data — the declared claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations: structurally beneficiaries (treaty rights protected) but identity_locked exit (cannot exit treaty relationship without abandoning nationhood) pulls d toward target end — net directionality ambiguous (omega). Federal government: agenda_setter with arbitrage exit (controls interpretation machinery) — d near 0.15 (beneficiary). Provincial governments: powerful but constrained exit (confederation membership) — d ~0.65 (payer). Separatist movements: organized but trapped (no legal exit path) — d ~0.95 (full target). Resource corporations: powerful with mobile exit (capital flight) — d ~0.4 (moderate payer). Supreme Court: analytical — d = 0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1982: constitutional patriation without Indigenous consent, Quebec's refusal to sign) was to legitimize the Canadian constitutional order by recognizing pre-existing treaty rights. That problem is contested: Indigenous nations say the founding problem (colonial imposition) persists; federal government says Section 35 solved it. The constraint persists because it serves federal territorial integrity — if the founding problem is dead but the arrangement remains, mandatrophy is unresolved. The treaty_primacy_reading itself resists mandatrophy resolution by asserting the founding problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_supremacy_vs_federal_interpretation,
    'Do Section 35 treaty rights structurally supersede federal/provincial sovereignty claims, or does the federal interpretation framework (Sparrow, Haida, Mikisew) domesticate them into a duty-to-consult that preserves ultimate Crown authority?',
    'Supreme Court jurisprudence trajectory: if future decisions treat treaty consent as a veto (not merely consultation), supremacy is structural; if consultation remains the ceiling, federal paramountcy persists.',
    'If supremacy is structural, the constraint is a genuine Mountain/Tangled Rope coordination function; if domesticated, the treaty frame is cover for federal control — extraction reclassifies toward Snare for Indigenous nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_supremacy_vs_federal_interpretation, conceptual, 'Whether treaty rights operate as hard constitutional veto or soft procedural duty').

omega_variable(
    indigenous_nations_beneficiary_or_payer,
    'Are Indigenous treaty nations net beneficiaries of this constraint (protection from unilateral secession) or net payers (bearing the cost of Canadian constitutional stability without reciprocal self-determination)?',
    'Compare outcomes: (a) cases where treaty consent blocked secession vs. (b) cases where treaty rights were overridden for ''national unity'' (e.g., Clarity Act framework). Track resource revenue flows with and without consent.',
    'If net payers, the constraint extracts from Indigenous nations to stabilize the federation — reclassifies toward Snare from Indigenous seat. If net beneficiaries, Tangled Rope coordination holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_nations_beneficiary_or_payer, empirical, 'Directionality of extraction for Indigenous treaty nations under the constraint').

omega_variable(
    kernel_framing_ambiguity_treaty_primacy,
    'Is the ''secession legitimacy boundary'' a single kernel with multiple readings, or are the declared readings (treaty_primacy, constitutional_impossibility, popular_sovereignty, grievance_threshold) structurally distinct constraints with different ε referents?',
    'Apply ε-invariance test: measure extractiveness from each reading''s structural perspective. If ε differs materially across readings for the same observable (e.g., Quebec secession), they are distinct constraints.',
    'If distinct constraints, each reading gets its own story and classification; the kernel frame is a linguistic artifact. If single kernel, cross-reading contamination analysis applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity_treaty_primacy, conceptual, 'Whether the kernel decomposition reflects structural reality or linguistic conflation').

omega_variable(
    resource_revenue_extraction_pathway,
    'Does the treaty consent requirement function as a revenue-sharing mechanism where provinces/federal government extract resource rents from Indigenous lands while using treaty process as legitimation?',
    'Trace resource project approvals: compare revenue flows where Indigenous consent was obtained vs. where projects proceeded via Crown justification (infringement test). Measure net transfer to/from Indigenous nations.',
    'If revenue extraction dominates, the constraint operates as Snare from Indigenous seat regardless of treaty rhetoric. If consent genuinely gates projects, coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_revenue_extraction_pathway, empirical, 'Whether treaty process enables or constrains resource extraction from Indigenous territories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_treaty_primacy_tr_t1982, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(secession_treaty_primacy_tr_t1990, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(secession_treaty_primacy_tr_t1995, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2000, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2010, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2020, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(secession_treaty_primacy_tr_t2024, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(secession_treaty_primacy_be_t1982, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(secession_treaty_primacy_be_t1990, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(secession_treaty_primacy_be_t1995, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(secession_treaty_primacy_be_t2000, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(secession_treaty_primacy_be_t2010, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(secession_treaty_primacy_be_t2020, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(secession_treaty_primacy_be_t2024, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(secession_treaty_primacy_su_t1982, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(secession_treaty_primacy_su_t1990, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(secession_treaty_primacy_su_t1995, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(secession_treaty_primacy_su_t2000, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(secession_treaty_primacy_su_t2010, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(secession_treaty_primacy_su_t2020, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(secession_treaty_primacy_su_t2024, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__treaty_primacy_reading, 0.08).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, crown_duty_consult).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, resource_development_consent).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, clarity_act_framework).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, quebec_secession_reference).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, tsilhqotin_title_decision).

% DUAL FORMULATION NOTE:
% This constraint is one member of the secession_legitimacy_boundary constraint family. The constitutional_impossibility_reading centers federal constitutional text; the popular_sovereignty_reading centers provincial democratic will; the grievance_threshold_reading centers structural injustice metrics. This reading centers Indigenous treaty rights as the anterior, superseding authority. The ε values differ materially: constitutional_impossibility has low ε for federal seat, high for separatists; popular_sovereignty has low ε for provincial majority, high for minorities; grievance_threshold has variable ε depending on grievance measure; treaty_primacy has high ε for separatists and ambiguous ε for Indigenous nations (omega). They are linked because each is cited as legitimating or delegitimating the others in judicial and political discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__treaty_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(secession_legitimacy_boundary__treaty_primacy_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
