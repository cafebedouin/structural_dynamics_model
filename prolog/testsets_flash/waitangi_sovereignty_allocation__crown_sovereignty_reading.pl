% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Waitangi Sovereignty Allocation (Crown Sovereignty Reading)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'Crown Sovereignty' reading of the Treaty
 *   of Waitangi, where English Article I is interpreted as a full cession of
 *   Māori sovereignty to the British Crown. This reading establishes
 *   Westminster parliamentary supremacy in New Zealand, granting the Crown
 *   plenary legislative power without requiring Māori consent and allowing
 *   unilateral resource allocation. Māori interests are subordinated to the
 *   will of Parliament. This interpretation has historically served as the
 *   legal basis for land confiscation and the marginalization of Māori
 *   self-governance.
 *
 * KEY AGENTS:
 *   - new_zealand_crown: Agenda setter (institutional/generational) — exercises ultimate authority.
 *   - parliament_of_new_zealand: Agenda setter (institutional/generational) — enacts laws based on plenary power.
 *   - maori_iwi_hapu: Payer (organized/generational) — bears the costs of lost sovereignty and resources.
 *   - maori_rangatira: Payer (powerful/generational) — traditional leaders whose authority is undermined.
 *   - non_maori_settlers: Beneficiary (moderate/generational) — benefit from land acquisition and resource access.
 *   - international_law_scholars: Observer (analytical/civilizational) — analyze the legality and ethics of the interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.85).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.75).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Waitangi Sovereignty Allocation (Crown Sovereignty Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '96270c4c-d7dd-4db1-b562-34b29ca339b4').
narrative_ontology:cs_kernel_codification('96270c4c-d7dd-4db1-b562-34b29ca339b4', fixed_text).
narrative_ontology:cs_authority_grounding('96270c4c-d7dd-4db1-b562-34b29ca339b4', lineage).
narrative_ontology:cs_interpretation_layer_present('96270c4c-d7dd-4db1-b562-34b29ca339b4').
narrative_ontology:cs_reading_relation('96270c4c-d7dd-4db1-b562-34b29ca339b4', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('96270c4c-d7dd-4db1-b562-34b29ca339b4', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('96270c4c-d7dd-4db1-b562-34b29ca339b4', foundational, parliamentary_supremacy_absolute).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('96270c4c-d7dd-4db1-b562-34b29ca339b4', parliamentary_supremacy_absolute, conventional).
narrative_ontology:cs_axiom('96270c4c-d7dd-4db1-b562-34b29ca339b4', foundational, full_sovereignty_cession_by_treaty).
narrative_ontology:cs_axiom_status(full_sovereignty_cession_by_treaty, holdable).
narrative_ontology:cs_axiom_grounding('96270c4c-d7dd-4db1-b562-34b29ca339b4', full_sovereignty_cession_by_treaty, conventional).
narrative_ontology:cs_reference_frame('96270c4c-d7dd-4db1-b562-34b29ca339b4', westminster_parliamentary_supremacy).
narrative_ontology:cs_drift_state('96270c4c-d7dd-4db1-b562-34b29ca339b4', contemporary_indigenous_rights_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('96270c4c-d7dd-4db1-b562-34b29ca339b4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliament_of_new_zealand).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_maori_settlers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_rangatira).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate authority that claims and exercises plenary sovereignty over New Zealand, deriving its power from the English text of the Treaty of Waitangi. Benefits from unfettered legislative and resource control.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% The legislative body that enacts laws based on the asserted plenary sovereignty of the Crown. Its authority is directly enabled by this reading of the Treaty, allowing it to make decisions without requiring Māori consent.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, parliament_of_new_zealand, agenda_setter,
    institutional, generational, constrained, national).

% The collective Māori tribes and sub-tribes who bear the costs of lost sovereignty, land, resources, and cultural autonomy. Their identity is deeply tied to their ancestral lands and self-governance, making 'exit' from the relationship with the Crown a form of cultural dissolution.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, national).

% Traditional Māori chiefs and leaders whose authority (tino rangatiratanga) over their people and lands is undermined by the Crown sovereignty reading. They are compelled to operate within a system that denies their inherent rights, making their position one of constrained resistance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_rangatira, payer,
    powerful, generational, identity_locked, national).

% Individuals and groups of European descent who benefited from the Crown's asserted sovereignty, gaining access to land and resources for settlement and economic development. They generally support the Crown sovereignty reading as it underpins their historical claims and current societal structure.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_maori_settlers, beneficiary,
    moderate, generational, mobile, national).

% Academics and legal experts who analyze the Treaty of Waitangi and its interpretations from a global perspective, often highlighting discrepancies between the English and Māori texts and the implications for indigenous rights under international law. They do not directly participate in the constraint but provide critical analysis.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_crown).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a single, unified system of governance and law over all inhabitants of New Zealand, facilitating British settlement and economic development.
% TRANSFER_FUNCTION: Transfers ultimate legislative and resource control from Māori iwi and hapu to the British Crown and its subsequent parliamentary institutions, enabling unilateral land acquisition and resource exploitation.
% ABSENT_VOICES: The voices of Māori who understood the Treaty as retaining their full authority (tino rangatiratanga) were effectively silenced or dismissed in the early colonial period, and their descendants continue to challenge this interpretation. Their perspective, if fully integrated, would fundamentally alter the constraint's structure.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished overnight, the entire constitutional and legal framework of New Zealand would collapse. Land titles, resource management, and parliamentary authority would be immediately contested, leading to a fundamental re-negotiation of power between Māori and the Crown.
% FOUNDING_PROBLEM: The problem of establishing British colonial authority and a stable legal framework for settlement in New Zealand, while managing relations with the indigenous Māori population.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and its institutions maintain that the need for a unified, stable governance system is still live. However, Māori and many legal scholars outside the benefiting parties argue that the 'problem' of establishing unilateral Crown authority was solved at the expense of Māori rights, and the current 'live' status is a perpetuation of an extractive arrangement, not a genuine coordination need.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).

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
 *   The constraint is classified as a Snare due to its high extractiveness (0.85) and suppression (0.75) from Māori, coupled with active enforcement of Crown authority. The coordination story (establishing a unified legal system) serves as cover for the unilateral transfer of power and resources. Theater ratio (0.4) reflects the performative aspects of 'consultation' or 'recognition' that often mask the underlying power imbalance. The rising extractiveness and suppression over time reflect the increasing consolidation of Crown power and the systematic marginalization of Māori authority post-Treaty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and non-Māori settlers, this reading establishes a legitimate, stable governance structure (a claimed Rope or even Mountain). From the perspective of Māori iwi and rangatira, it is a coercive mechanism that dispossessed them of their inherent rights and resources (a clear Snare). The engine's classification will reflect the latter due to the declared victims and high extraction/suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown and Parliament are full beneficiaries (d=0.0) as they gain plenary power and control. Non-Māori settlers are also beneficiaries (d=0.1-0.2) through access to land and resources. Māori iwi and rangatira are full targets (d=1.0) as they lose sovereignty, land, and self-determination. The constraint directly subsidizes the Crown and settlers by enabling resource acquisition and governance without consent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling a coercive power transfer as legitimate coordination. The 'founding problem' of establishing a unified legal system is used to justify an arrangement that actively dispossesses one party. The persistence of this reading, despite ongoing Māori resistance and alternative interpretations, indicates that its primary function has shifted from initial coordination (if any) to pure extraction, maintained by active suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Treaty of Waitangi a cession of full sovereignty to the Crown, or a grant of governorship with retained Māori authority?',
    'Judicial re-interpretation by a supreme court or constitutional amendment recognizing the Māori text''s primacy, or a political settlement acknowledging co-sovereignty.',
    'If resolved towards the rangatiratanga reading, this constraint would be reclassified from Snare to a contested Tangled Rope or even a Mountain (if co-sovereignty is recognized as an inherent right), with significantly reduced extraction and suppression from Māori. If resolved towards partnership, it would become a Tangled Rope with mandated consultation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the Waitangi sovereignty kernel; other readings (partnership, rangatiratanga) would yield different classifications.').

omega_variable(
    legitimacy_of_unilateral_power,
    'Is the Crown''s exercise of plenary legislative power without Māori consent legitimate, given the contested interpretations of the Treaty?',
    'A national referendum on constitutional principles, or a sustained period of successful co-governance models demonstrating viability and equity.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness would be seen as unjust, increasing resistance and potentially leading to international condemnation or internal political instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_power, preference, 'The legitimacy of unilateral Crown power is a core point of contention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wait_tr_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(wait_tr_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(wait_be_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(wait_be_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(wait_su_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(wait_su_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Waitangi sovereignty allocation' kernel. This 'Crown Sovereignty' reading asserts plenary Crown power, while the 'Partnership' reading posits a co-governance model, and the 'Rangatiratanga' reading asserts retained Māori authority. Each reading constitutes a distinct constraint with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
