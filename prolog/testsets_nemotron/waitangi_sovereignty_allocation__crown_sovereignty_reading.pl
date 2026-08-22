% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of Treaty of Waitangi Article I
 *   domain: constitutional/indigenous/post-colonial
 *
 * SUMMARY:
 *   The Crown sovereignty reading of Treaty of Waitangi Article I treats the
 *   English text ('ceded complete sovereignty to the Crown') as establishing
 *   Westminster-style parliamentary supremacy over New Zealand. This reading
 *   empowers the Crown executive and Parliament to exercise plenary
 *   legislative authority without a requirement for Māori consent, enabling
 *   unilateral resource allocation and subordination of Māori interests to
 *   parliamentary will. The constraint operates as a tangled rope: it
 *   provides genuine coordination for settler governance (law, courts,
 *   infrastructure, market regulation) while simultaneously extracting from
 *   Māori through legislative override of tino rangatiratanga claims, land
 *   alienation, and resource decisions made without consent. Active
 *   enforcement is required through courts, police, and legislative machinery
 *   to maintain the sovereignty claim against ongoing Māori resistance and
 *   international indigenous rights norms.
 *
 * KEY AGENTS:
 *   - crown_executive: Primary beneficiary (institutional/arbitrage) — exercises plenary power, controls resource allocation
 *   - parliamentary_majority: Primary beneficiary (institutional/arbitrage) — legislates without Māori consent requirement
 *   - settler_institutions: Beneficiary (organized/constrained) — gain stable governance framework on Māori land
 *   - maori_iwi_hapu: Primary victim (powerless/identity_locked) — bear legislative override, land loss, cultural suppression
 *   - maori_land_owners: Victim (powerless/constrained) — subject to unilateral Crown acquisition and alienation
 *   - taonga_guardians: Victim (powerless/identity_locked) — cultural treasures subordinated to Crown authority
 *   - waitangi_tribunal: Observer (institutional/analytical) — investigates breaches but lacks binding remedial power
 *   - international_indigenous_rights_bodies: Excluded (organized/analytical) — would object but lack domestic enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.72).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of Treaty of Waitangi Article I").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/indigenous/post-colonial").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e9cfcafc-16a8-46f7-bc42-fac7495b37d3').
narrative_ontology:cs_kernel_codification('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', fixed_text).
narrative_ontology:cs_authority_grounding('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', lineage).
narrative_ontology:cs_interpretation_layer_present('e9cfcafc-16a8-46f7-bc42-fac7495b37d3').
narrative_ontology:cs_reading_relation('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', waitangi_sovereignty_allocation__partnership_reading, forecloses).
narrative_ontology:cs_reading_relation('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', foundational, crown_plenary_sovereignty_unqualified).
narrative_ontology:cs_axiom_status(crown_plenary_sovereignty_unqualified, holdable).
narrative_ontology:cs_axiom_grounding('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', crown_plenary_sovereignty_unqualified, conventional).
narrative_ontology:cs_axiom('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', foundational, english_text_authoritative_over_maori_text).
narrative_ontology:cs_axiom_status(english_text_authoritative_over_maori_text, holdable).
narrative_ontology:cs_axiom_grounding('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', english_text_authoritative_over_maori_text, conventional).
narrative_ontology:cs_axiom('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', secondary, parliamentary_supremacy_entrenched).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_entrenched, holdable).
narrative_ontology:cs_axiom_grounding('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', parliamentary_supremacy_entrenched, conventional).
narrative_ontology:cs_reference_frame('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', id_1840_crown_sovereignty_acquisition).
narrative_ontology:cs_drift_state('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', contemporary_treaty_settlements_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e9cfcafc-16a8-46f7-bc42-fac7495b37d3', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_institutions).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_owners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, taonga_guardians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises executive power under the sovereignty claim: controls legislative agenda, resource allocation, Treaty settlements process, and Crown-Māori relations. Collects the benefits of plenary authority (decision control, resource access, international legal personality). Exit is arbitrage-grade: the Crown could concede authority without institutional collapse, as seen in other post-colonial transitions.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislates without Māori consent requirement. Gains legislative supremacy and resource allocation authority. The sovereignty claim protects parliamentary privilege and electoral mandate from Māori constitutional challenge. Exit is arbitrage-grade: Parliament could adopt a partnership framework without losing its core lawmaking function.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliamentary_majority, beneficiary,
    institutional, biographical, arbitrage, national).

% Courts, local government, regulatory bodies, and commercial sectors gain a stable, predictable governance framework. Their operations depend on the certainty of Crown title and parliamentary law. Exit is constrained: they benefit from the framework but would face legal and commercial disruption if the sovereignty basis shifted.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_institutions, beneficiary,
    organized, biographical, constrained, national).

% Bear the extraction: legislative override of rangatiratanga, historical land alienation (raupatu, native land court), resource decisions without consent, cultural suppression. Tino rangatiratanga is constitutive of iwi/hapū identity — exit means cultural dissolution, not merely institutional change. They resist through Waitangi Tribunal claims, political advocacy, and cultural revitalization, but the constraint's enforcement machinery (courts, police, legislation) maintains the override.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    powerless, generational, identity_locked, national).

% Subject to Crown pre-emption, compulsory acquisition, and planning regimes that prioritize national development over Māori land retention. The sovereignty claim underpins the legal framework that makes Māori land alienable and developable by others. Exit is constrained: they can use legal channels (Māori Land Court, Treaty settlements) but the framework itself is Crown-defined.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_owners, payer,
    powerless, biographical, constrained, regional).

% Guardians of cultural treasures (language, ancestral remains, sacred sites, mātauranga) subordinated to Crown authority. The sovereignty claim enables Crown control over taonga through legislation (e.g., Protected Objects Act, resource management). Exit is identity_locked: the guardianship relationship is constitutive of identity and cannot be relinquished without cultural loss.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, taonga_guardians, payer,
    powerless, generational, identity_locked, national).

% Investigates Treaty breaches and makes recommendations but lacks binding remedial power. Its findings create moral and political pressure but the Crown sovereignty reading treats its recommendations as advisory only. Sits at analytical distance: observes the constraint's operation without collecting or paying.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% UN Permanent Forum, Special Rapporteur, EMRIP, and treaty bodies consistently criticize the Crown sovereignty reading as inconsistent with UNDRIP and self-determination. Their objections are structurally excluded from domestic legal effect — the constraint's enforcement machinery (domestic courts, parliamentary privilege) blocks international norms from overriding the sovereignty claim.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_indigenous_rights_bodies, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, recognized sovereign authority for the territory: settler governance, law courts, property system, infrastructure, and international legal personality. Solves the coordination problem of governing a diverse population with a unified legal framework.
% TRANSFER_FUNCTION: Moves legislative supremacy, resource allocation authority, and decision-making control from Māori (who would exercise tino rangatiratanga under sibling readings) to Crown executive and Parliament. The transfer is unilateral — no consent mechanism exists under this reading.
% ABSENT_VOICES: Māori signatories of 1840 who understood they were ceding kāwanatanga (governorship over settlers) not sovereignty; contemporary iwi/hapū leadership excluded from constitutional decision-making; international indigenous rights bodies whose findings are advisory only. They are absent because the sovereignty claim structurally blocks their participation in the constitutional framework.
% DISAPPEARANCE_RATIONALE: If the Crown sovereignty reading vanished overnight, the constitutional basis for plenary parliamentary authority over Māori would collapse. Resource allocation would require Māori consent. Legislation affecting taonga would need partnership agreement. The Treaty settlements process would transform from Crown-determined to negotiated constitutional reform. The entire legal framework of NZ would reorganize around a partnership or rangatiratanga model.
% FOUNDING_PROBLEM: Governance of a British settler colony in 1840: establishing law, order, and administration over a geographically dispersed population with competing land claims, in a context of inter-iwi conflict and European lawlessness.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal (Te Roroa Report 1992, Te Urewera Report 2012) and constitutional scholars (Sir Geoffrey Palmer, Dr. Moana Jackson) attest the 1840 governance problem is solved — NZ has stable institutions. The Crown concedes the founding problem is historical but argues the sovereignty solution remains necessary. No independent corroborator outside Crown/Parliament supports the claim that the 1840 problem requires today's plenary sovereignty.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint enables systematic resource transfer from Māori to Crown/settlers without consent or fair compensation. Suppression (0.72) is substantial because maintaining the sovereignty claim requires active legal and political enforcement against Māori assertions of tino rangatiratanga. Theater ratio (0.28) is moderate — the coordination function (settler governance) is real but a growing share of enforcement activity defends extractive overrides. The measurement series shows extractiveness rising from 0.45 (1840s, early coordination-dominant phase) to 0.78 (present, extraction-dominant) as the coordination function matured and extraction layered on. Theater ratio rises in parallel as coordination infrastructure becomes theater for extraction. Suppression requirement rises as Māori resistance intensifies and international norms challenge the claim.
 *
 * PERSPECTIVAL GAP:
 *   From Crown/Parliament seats: the constraint is genuine coordination (rope-like) — it built the legal and institutional framework of NZ. From Māori seats: the same structure is extractive override (snare-like) — it dispossesses and subordinates. The engine computes this divergence from the structural data: same constraint, different χ per seat. The claimed type (tangled_rope) acknowledges both functions are structurally present and inseparable in current operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown executive and parliamentary majority are structural beneficiaries (d near 0.0) — they collect legislative power and resource control. Settler institutions are moderate beneficiaries (d ~ 0.2) — they gain stable governance but face some constraint from Māori resistance. Māori iwi/hapū are structural targets (d near 1.0) — identity_locked exit (tino rangatiratanga is constitutive of identity), bear the full extraction. Land owners and taonga guardians similarly targeted. Waitangi Tribunal sits at analytical distance (d = 0.5) — observes but cannot compel. International bodies are excluded — their objection is structurally blocked from domestic effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governance of a settler colony) was live in 1840 but is now dead — the coordination function has matured into stable institutions that no longer require the sovereignty claim's extractive edge. The arrangement persists because the extraction (resource control, legislative supremacy) benefits the agenda-setters, not because the founding problem requires it. Mandatrophy is unresolved: the constraint's mandate has outlived its coordination function but the extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading (crown_sovereignty_reading) of the contested kernel waitangi_sovereignty_allocation. What structural elements would change if a sibling reading (partnership_reading or rangatiratanga_reading) were instantiated instead?',
    'Comparative constraint analysis across the three readings: map beneficiary/victim sets, extraction vectors, and enforcement requirements for each reading of the same kernel.',
    'Sibling readings instantiate different constraints with different ε, different stakeholder structures, and potentially different DR types. The kernel is the contested commitment; each reading is a distinct constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Commitment-system kernel with multiple readings: crown_sovereignty_reading, partnership_reading, rangatiratanga_reading').

omega_variable(
    english_vs_maori_text_divergence,
    'Does the English text Article I (''ceded complete sovereignty'') and Māori text Article II (''tino rangatiratanga'') represent a genuine translation ambiguity or a structural incompatibility between the readings?',
    'Linguistic and historical analysis of the 1840 texts; Waitangi Tribunal findings on translation and understanding at signing.',
    'If genuine ambiguity, multiple readings can coexist as defensible interpretations. If structural incompatibility, one reading must foreclose the others within any single legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(english_vs_maori_text_divergence, empirical, 'Translation ambiguity vs. structural incompatibility between English and Māori texts').

omega_variable(
    extraction_legitimacy_boundary,
    'Where does the coordination function of Crown governance (law, order, infrastructure) end and the extractive function (unilateral resource allocation, legislative override of Māori interests) begin?',
    'Historical analysis of Crown actions 1840-present: measure proportion of legislative/regulatory activity that coordinates settler society vs. activity that dispossesses or subordinates Māori interests without consent.',
    'If coordination and extraction are inseparable, the tangled_rope classification holds. If separable, the extraction component could be removed while preserving coordination, shifting toward rope for the coordination residue.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_legitimacy_boundary, conceptual, 'Boundary between genuine governance coordination and extractive override of Māori interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(wait_tr_t30, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(wait_tr_t60, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(wait_tr_t90, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 90, 0.25).
narrative_ontology:measurement(wait_tr_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 120, 0.27).
narrative_ontology:measurement(wait_tr_t150, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 150, 0.28).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(wait_be_t30, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(wait_be_t60, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(wait_be_t90, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 90, 0.76).
narrative_ontology:measurement(wait_be_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 120, 0.78).
narrative_ontology:measurement(wait_be_t150, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 150, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(wait_su_t30, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(wait_su_t60, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(wait_su_t90, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(wait_su_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 120, 0.71).
narrative_ontology:measurement(wait_su_t150, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 150, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, nz_resource_management_act).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_alienation_constraints).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Treaty of Waitangi into three readings of the same kernel. The crown_sovereignty_reading (this story) claims English Article I establishes plenary Crown sovereignty. The partnership_reading claims an ongoing partnership requiring consultation. The rangatiratanga_reading claims Māori retained tino rangatiratanga. Their ε values differ substantially: this reading shows high extraction (0.78) because it subordinates Māori interests; the rangatiratanga_reading would show low extraction from Māori but high coordination cost for Crown; the partnership_reading sits between. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, institutional, 0.05).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
