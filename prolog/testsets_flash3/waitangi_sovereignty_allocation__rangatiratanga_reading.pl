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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi: Māori Tino Rangatiratanga (Full Authority) Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint represents the 'tino rangatiratanga' reading of Article
 *   II of the Māori text of the Treaty of Waitangi. Under this reading, Māori
 *   retained full authority over their lands, resources, and taonga
 *   (treasures), while the Crown gained only kāwanatanga (governorship) over
 *   its own settlers. This interpretation asserts inherent Māori sovereignty
 *   and limits Crown jurisdiction, forming the basis for Māori claims to
 *   self-determination and co-governance. The constraint is classified as a
 *   Tangled Rope because it genuinely seeks to coordinate two distinct
 *   authorities but has been historically undermined by asymmetric extraction
 *   and active suppression of Māori authority by the Crown.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.7).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.6).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi: Māori Tino Rangatiratanga (Full Authority) Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '2ee478d7-bbd0-407e-ab6c-7d00242a2124').
narrative_ontology:cs_kernel_codification('2ee478d7-bbd0-407e-ab6c-7d00242a2124', fixed_text).
narrative_ontology:cs_authority_grounding('2ee478d7-bbd0-407e-ab6c-7d00242a2124', lineage).
narrative_ontology:cs_interpretation_layer_present('2ee478d7-bbd0-407e-ab6c-7d00242a2124').
narrative_ontology:cs_reading_relation('2ee478d7-bbd0-407e-ab6c-7d00242a2124', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('2ee478d7-bbd0-407e-ab6c-7d00242a2124', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('2ee478d7-bbd0-407e-ab6c-7d00242a2124', foundational, maori_inherent_sovereignty).
narrative_ontology:cs_axiom_status(maori_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2ee478d7-bbd0-407e-ab6c-7d00242a2124', maori_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('2ee478d7-bbd0-407e-ab6c-7d00242a2124', foundational, crown_limited_governorship).
narrative_ontology:cs_axiom_status(crown_limited_governorship, holdable).
narrative_ontology:cs_axiom_grounding('2ee478d7-bbd0-407e-ab6c-7d00242a2124', crown_limited_governorship, conventional).
narrative_ontology:cs_reference_frame('2ee478d7-bbd0-407e-ab6c-7d00242a2124', maori_text_original_intent).
narrative_ontology:cs_drift_state('2ee478d7-bbd0-407e-ab6c-7d00242a2124', contemporary_new_zealand_law, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2ee478d7-bbd0-407e-ab6c-7d00242a2124', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_cultural_institutions).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government_agencies).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_landowners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_extraction_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original signatories and inheritors of tino rangatiratanga, they assert full authority over their lands, resources, and cultural treasures. They benefit from the recognition of this authority but face ongoing challenges to its implementation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu, beneficiary,
    organized, generational, identity_locked, national).

% Under this reading, the Crown's authority is limited to kāwanatanga (governorship) over settlers, not over Māori or their resources. This implies a significant reduction in their claimed sovereign power and requires ceding control over substantial assets and policy areas.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government_agencies, payer,
    institutional, generational, constrained, national).

% Their land titles and resource access, often acquired under Crown authority, would be subject to Māori rangatiratanga. This could lead to renegotiation, compensation, or changes in land use, imposing costs and uncertainty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_landowners, payer,
    moderate, biographical, constrained, local).

% Their licenses and permits for forestry, mining, and fishing, granted by the Crown, would be subject to Māori authority. This would require new agreements, potentially higher royalties, or even cessation of operations, impacting their profitability.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_extraction_companies, payer,
    powerful, biographical, constrained, regional).

% Benefit from the explicit recognition and protection of taonga (treasures), including language, arts, and sacred sites, under Māori authority. This empowers them to preserve and promote Māori culture without Crown interference.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_cultural_institutions, beneficiary,
    organized, generational, identity_locked, national).

% Interprets the Treaty and its implications for modern law. While not directly a beneficiary or victim, their rulings significantly shape the implementation and enforcement of this reading, acting as a gatekeeper for its legal recognition.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for two distinct authorities (Māori and Crown) to coexist and govern their respective spheres, preventing conflict over jurisdiction and resource control.
% TRANSFER_FUNCTION: Transfers ultimate authority over Māori lands, resources, and taonga from the Crown's asserted sovereignty back to Māori iwi and hapū, while limiting Crown governance to non-Māori populations.
% ABSENT_VOICES: The full range of Māori iwi and hapū who were signatories or affected by the Treaty, particularly those whose rangatiratanga was most directly undermined by Crown actions, have historically been excluded from the dominant interpretive discourse. Their voices would emphasize the inherent and continuing nature of Māori authority.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished, the legal and political landscape of Aotearoa New Zealand would fundamentally shift. Māori claims to self-determination and resource control would lose their primary legal grounding, leading to renewed Crown assertion of absolute sovereignty and widespread Māori resistance. The entire constitutional order would be destabilized.
% FOUNDING_PROBLEM: The Treaty was intended to establish a relationship between Māori and the British Crown, allowing for British settlement while protecting Māori authority and property, and preventing inter-tribal warfare.
% FOUNDING_PROBLEM_CORROBORATION: Māori scholars and elders corroborate the intent to retain rangatiratanga. Crown historians and legal scholars, particularly those aligned with the 'Crown sovereignty' reading, contest this, arguing for a full cession of sovereignty. Independent constitutional experts often highlight the textual ambiguities and differing understandings at the time of signing.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because the Crown's historical actions have consistently overridden Māori rangatiratanga, appropriating lands and resources. Suppression (0.6) is significant due to legislative and military actions used to enforce Crown sovereignty over Māori authority. Theater ratio (0.2) is moderate; while there are genuine efforts to uphold the Treaty, a substantial portion of Crown engagement has been performative, masking continued assertion of ultimate Crown authority. Resistance (0.8) is high, reflecting ongoing Māori activism and legal challenges to assert rangatiratanga. Accessibility collapse (0.4) is moderate, as Māori have consistently maintained their claims and cultural identity despite suppression, indicating alternatives have not fully collapsed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Māori iwi and hapū, this reading represents the true intent of the Treaty, establishing their inherent authority. From the perspective of Crown government agencies and settler interests, this reading is highly extractive, demanding a cession of power and resources they currently control. The New Zealand judiciary, as an agenda-setter, navigates these competing interpretations, with its rulings shaping the effective classification for all parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi and hapū are beneficiaries of this reading, as it affirms their inherent authority (low d). Crown government agencies, settler landowners, and resource extraction companies are victims, as their current claims to absolute authority and resource access would be curtailed (high d). The New Zealand judiciary, while an agenda-setter, experiences a more moderate directionality as it must balance competing claims within the legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to establish a relationship between two authorities is still live, but its implementation has been distorted. The classification as a Tangled Rope prevents mislabeling it as a pure Snare, acknowledging the genuine coordination function intended by the Māori text, while highlighting the asymmetric extraction and suppression that have historically characterized its operation. It also prevents mislabeling it as a pure Rope, which would ignore the ongoing power imbalances and resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_interpretation_ambiguity,
    'Is the difference between ''kāwanatanga'' (governorship) and ''tino rangatiratanga'' (full authority) a fundamental, irreconcilable textual difference, or a matter of evolving legal interpretation?',
    'Comparative analysis of 19th-century Māori and English legal terminology, and historical records of discussions at the time of signing. Further, examination of how similar concepts have been interpreted in other indigenous treaties globally.',
    'If irreconcilable, it strengthens the argument for two distinct sovereignties. If evolving, it opens pathways for a unified, but re-interpreted, constitutional framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_interpretation_ambiguity, conceptual, 'Ambiguity in the core terms of the Treaty''s Māori text.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Māori rangatiratanga primarily structural (e.g., legislative acts, land confiscations) or internalized (e.g., historical trauma, cultural assimilation)?',
    'Post-settlement outcomes: if Māori authority and well-being persist after structural barriers are removed, it suggests the internalized component is less dominant. Conversely, if challenges persist, internalized suppression may be more significant.',
    'If primarily structural, legal and policy reforms are sufficient. If significantly internalized, deeper cultural and social interventions are required for true rangatiratanga to flourish, potentially increasing the effective suppression beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Māori rangatiratanga.').

omega_variable(
    rangatiratanga_scope_ambiguity,
    'What is the precise scope of ''tino rangatiratanga'' in contemporary governance? Does it imply full independent Māori governance, co-governance with the Crown, or a form of self-management within the existing state structure?',
    'Ongoing legal precedent, political negotiations, and the development of Māori-led governance models. The outcomes of specific co-governance arrangements and Treaty settlements will provide empirical data.',
    'A broader interpretation (independent governance) would significantly increase the perceived extraction from the Crown and settler seats. A narrower interpretation (self-management) would reduce it, potentially shifting the constraint towards a more balanced Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_scope_ambiguity, preference, 'The contemporary scope and form of Māori tino rangatiratanga.').


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
narrative_ontology:measurement(wait_tr_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(wait_tr_t1980, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(wait_be_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1870, 0.3).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(wait_be_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(wait_be_t1980, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.2).
narrative_ontology:measurement(wait_su_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1870, 0.4).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(wait_su_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(wait_su_t1980, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_rights_legislation).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_management_act_interpretations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'waitangi_sovereignty_allocation' kernel. It asserts Māori tino rangatiratanga over lands, resources, and taonga, with Crown kāwanatanga limited to settlers. This contrasts with the 'crown_sovereignty_reading' (English text, full cession) and the 'partnership_reading' (ambiguous text, good faith consultation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
