% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi: Rangatiratanga Reading (Māori Authority)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rangatiratanga reading' of the
 *   Treaty of Waitangi, which asserts that the Māori text of Article II
 *   retained full Māori authority (tino rangatiratanga) over lands,
 *   resources, and taonga, with the Crown gaining only kāwanatanga
 *   (governorship) over its own settlers. From this reading's perspective,
 *   the current state of governance in Aotearoa New Zealand represents a
 *   severe deviation from the Treaty's original intent, leading to high
 *   extraction from Māori. The claimed type is 'mountain' because
 *   rangatiratanga is understood as an inherent, foundational authority, but
 *   the high extractiveness reflects the historical and ongoing failure to
 *   realize this inherent right in practice.
 *
 * KEY AGENTS:
 *   - maori_iwi_hapu: Primary beneficiary (if rangatiratanga were realized), identity_locked
 *   - maori_citizens: Beneficiary, identity_locked
 *   - crown_government: Payer (would bear costs of implementation), institutional
 *   - pakeha_settlers: Payer, powerful
 *   - treaty_tribunal: Observer, institutional
 *   - international_human_rights_bodies: Observer, institutional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.85).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.75).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, mountain).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi: Rangatiratanga Reading (Māori Authority)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).
domain_priors:emerges_naturally(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, 'dced2fc2-fb79-4a34-947c-b181f14136f8').
narrative_ontology:cs_kernel_codification('dced2fc2-fb79-4a34-947c-b181f14136f8', fixed_text).
narrative_ontology:cs_authority_grounding('dced2fc2-fb79-4a34-947c-b181f14136f8', lineage).
narrative_ontology:cs_interpretation_layer_present('dced2fc2-fb79-4a34-947c-b181f14136f8').
narrative_ontology:cs_reading_relation('dced2fc2-fb79-4a34-947c-b181f14136f8', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('dced2fc2-fb79-4a34-947c-b181f14136f8', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('dced2fc2-fb79-4a34-947c-b181f14136f8', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('dced2fc2-fb79-4a34-947c-b181f14136f8', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_axiom('dced2fc2-fb79-4a34-947c-b181f14136f8', foundational, kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('dced2fc2-fb79-4a34-947c-b181f14136f8', kawanatanga_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('dced2fc2-fb79-4a34-947c-b181f14136f8', maori_inherent_sovereignty).
narrative_ontology:cs_drift_state('dced2fc2-fb79-4a34-947c-b181f14136f8', contemporary_post_colonial_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('dced2fc2-fb79-4a34-947c-b181f14136f8', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, pakeha_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the collective entities of Māori society, they are the primary holders of tino rangatiratanga, asserting inherent authority over their lands, resources, and taonga. Their identity and future are inextricably linked to the recognition and exercise of this authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, beneficiary,
    organized, generational, identity_locked, national).

% Individual Māori citizens benefit from the assertion of rangatiratanga through cultural revitalization, self-determination, and the protection of their collective rights. Their ability to 'exit' from this identity is not an option, as it is foundational to their being.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_citizens, beneficiary,
    moderate, biographical, identity_locked, national).

% The Crown, as the governing body, would bear the significant costs of fully implementing rangatiratanga, including ceding control over resources, land, and legislative authority. Its current power structure is built on a different interpretation of sovereignty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, payer,
    institutional, civilizational, constrained, national).

% Non-Māori citizens, particularly those whose economic and social structures have benefited from the historical assertion of Crown sovereignty, would face significant adjustments and potential costs (e.g., land restitution, resource sharing) if rangatiratanga were fully realized.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, pakeha_settlers, payer,
    powerful, biographical, mobile, national).

% An independent commission of inquiry established to make recommendations on claims relating to the Treaty of Waitangi. It investigates historical grievances and provides findings that often support the rangatiratanga reading, but its recommendations are not binding on the Crown.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_tribunal, observer,
    institutional, biographical, analytical, national).

% These bodies monitor New Zealand's compliance with international indigenous rights declarations and treaties, often providing critical assessments that align with the rangatiratanga reading and pressure the Crown for greater recognition of Māori self-determination.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for co-existence and governance in Aotearoa, where Māori retain full authority over their affairs and resources, and the Crown exercises governorship over its own people.
% TRANSFER_FUNCTION: The current (contested) arrangement effectively transfers control and benefit from Māori lands, resources, and taonga to the Crown and Pākehā settlers, despite the rangatiratanga reading's assertion of retained Māori authority.
% ABSENT_VOICES: The full, uncompromised voice of Māori asserting tino rangatiratanga was largely absent from the subsequent legal and political interpretations that established Crown sovereignty, leading to a systemic marginalization of this reading.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga reading of the Treaty were fully recognized and implemented overnight, it would necessitate a fundamental restructuring of New Zealand's constitutional, legal, and economic systems, leading to significant shifts in land ownership, resource management, and the distribution of political power.
% FOUNDING_PROBLEM: To enable British settlement in Aotearoa while securing Māori sovereignty, property rights, and cultural integrity, establishing a basis for peaceful co-existence and shared governance.
% FOUNDING_PROBLEM_CORROBORATION: Māori leaders, historians, and legal scholars consistently corroborate this understanding of the founding problem and the Treaty's intent. While the Crown's historical actions often contradict it, the Treaty Tribunal and international indigenous rights bodies provide external corroboration for the validity of the rangatiratanga perspective.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, ExtMetricName, E),
    domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(waitangi_sovereignty_allocation__rangatiratanga_reading),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the severe gap between the inherent authority claimed by rangatiratanga and the actual historical and contemporary denial of that authority, leading to significant loss of Māori land, resources, and self-determination. Suppression (0.75) is high due to historical legislative and military actions by the Crown to assert its sovereignty and suppress Māori self-governance. Theater ratio (0.40) indicates that while there is some performative recognition of Māori rights and culture, the fundamental assertion of rangatiratanga is often not genuinely implemented. Resistance (0.80) is consistently high, reflecting ongoing Māori activism, legal challenges, and cultural revitalization efforts. The claimed type 'mountain' reflects the inherent, foundational nature of tino rangatiratanga as understood by this reading, while the metrics describe the extractive reality of its non-recognition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Māori (beneficiaries of rangatiratanga), the constraint is a fundamental, inherent right that has been severely undermined and extracted from. From the perspective of the Crown and Pākehā settlers (payers of rangatiratanga's implementation), the full realization of rangatiratanga would represent a significant loss of established power and resources, making it a 'prohibitive' cost. The engine will compute divergent classifications based on these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi and citizens are declared beneficiaries because the rangatiratanga reading asserts their inherent authority and rights. The Crown government and Pākehā settlers are declared payers because they would bear the costs of ceding control and making restitution if rangatiratanga were fully implemented. The high extractiveness and suppression metrics reflect the current reality where the Crown's interpretation of sovereignty dominates, effectively extracting from Māori and suppressing their self-determination.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'mountain' with high extractiveness and beneficiaries (Māori) is crucial for detecting a 'false summit mountain'. This prevents mislabeling the assertion of rangatiratanga as a 'snare' (pure extraction) or 'tangled rope' (hybrid) from the Crown's perspective, which would obscure the foundational claim of inherent authority. Instead, it highlights that a fundamental, inherent right is being actively denied and extracted from, rather than being a constructed extractive mechanism itself. The high resistance further supports that this is not a 'piton' (inertial decay), but a live, contested claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_interpretation_ambiguity,
    'To what extent is the difference between ''tino rangatiratanga'' and ''kāwanatanga'' an irreducible textual ambiguity, versus a deliberate misinterpretation by the Crown?',
    'Linguistic and historical analysis of 19th-century Māori and English legal terminology, focusing on the understanding of the terms by both parties at the time of signing.',
    'If irreducible ambiguity, the contest is conceptual; if deliberate misinterpretation, the constraint''s suppression and extractiveness are more clearly attributable to active, bad-faith enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_interpretation_ambiguity, empirical, 'Ambiguity of Treaty terms and intent.').

omega_variable(
    sovereignty_compatibility,
    'Can ''tino rangatiratanga'' (full authority) and ''kāwanatanga'' (governorship) coexist within a single, unified constitutional framework, or do they represent fundamentally incompatible claims to sovereignty?',
    'Comparative constitutional analysis of other post-colonial states with indigenous self-determination agreements, and legal/political theory on shared or nested sovereignty.',
    'If incompatible, the contest is zero-sum, requiring a fundamental re-ordering of the state; if compatible, pathways for co-governance or shared authority are structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_compatibility, conceptual, 'Compatibility of Māori and Crown sovereignty claims.').

omega_variable(
    false_summit_rangatiratanga,
    'Is the claim of ''tino rangatiratanga'' as an inherent, natural authority a genuine ''mountain'' of indigenous right, or a constructed claim that, in practice, functions as a ''snare'' or ''tangled_rope'' for other parties?',
    'Analysis of the historical and contemporary exercise of rangatiratanga: does its assertion genuinely benefit Māori without disproportionately extracting from others, or does it create new forms of extraction?',
    'If it functions as a snare, the classification would shift, highlighting the potential for indigenous claims to become extractive if not carefully balanced. If it remains a genuine mountain, the current high extractiveness is solely due to its denial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_rangatiratanga, conceptual, 'Whether rangatiratanga is a genuine inherent right or a constructed claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(wait_tr_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1870, 0.1).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(wait_tr_t1940, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1940, 0.25).
narrative_ontology:measurement(wait_tr_t1980, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(wait_be_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1870, 0.4).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(wait_be_t1940, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1940, 0.75).
narrative_ontology:measurement(wait_be_t1980, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.2).
narrative_ontology:measurement(wait_su_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1870, 0.5).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(wait_su_t1940, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(wait_su_t1980, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
