% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Treaty Restrictive Reading of Minority Protections
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the 'restrictive_reading' of the Lausanne
 *   Treaty's minority protections, specifically as applied in Turkey. Under
 *   this interpretation, the treaty's guarantees are limited to individual
 *   worship rights, while the institutional aspects of minority religious
 *   life—such as property ownership, legal personality of foundations, and
 *   the right to establish and operate theological schools—are considered
 *   purely domestic matters subject to general Turkish law. This reading has
 *   historically enabled the Turkish state to exert significant control over
 *   non-Muslim minority institutions, leading to property confiscations,
 *   closure of educational facilities, and denial of legal autonomy.
 *
 * KEY AGENTS:
 *   - turkish_state_apparatus: Agenda setter (institutional/arbitrage) — benefits from control
 *   - non_muslim_minority_foundations: Payer (organized/trapped) — bears property loss, legal challenges
 *   - minority_religious_communities: Payer (organized/identity_locked) — bears loss of institutional capacity, cultural erosion
 *   - minority_educational_institutions: Payer (organized/trapped) — bears closure, inability to train clergy
 *   - european_court_of_human_rights: Observer (institutional/analytical) — potential arbiter, but limited by state sovereignty claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.85).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.9).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Treaty Restrictive Reading of Minority Protections").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '72b91f44-9c06-4ee0-862c-fc52ce5f4df8').
narrative_ontology:cs_kernel_codification('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', fixed_text).
narrative_ontology:cs_authority_grounding('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', extraction).
narrative_ontology:cs_interpretation_layer_present('72b91f44-9c06-4ee0-862c-fc52ce5f4df8').
narrative_ontology:cs_reading_relation('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', foundational, individual_worship_only_protection).
narrative_ontology:cs_axiom_status(individual_worship_only_protection, holdable).
narrative_ontology:cs_axiom_grounding('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', individual_worship_only_protection, conventional).
narrative_ontology:cs_axiom('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', foundational, institutional_matters_domestic_sovereignty).
narrative_ontology:cs_axiom_status(institutional_matters_domestic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', institutional_matters_domestic_sovereignty, conventional).
narrative_ontology:cs_reference_frame('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', national_sovereignty_post_ottoman).
narrative_ontology:cs_drift_state('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', contemporary_human_rights_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('72b91f44-9c06-4ee0-862c-fc52ce5f4df8', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_communities).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_educational_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Lausanne Treaty's minority provisions restrictively, asserting full domestic sovereignty over minority institutional matters. Benefits from consolidating control and potentially assets.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Legally recognized entities representing minority communities (e.g., Greek, Armenian, Jewish foundations). They face property confiscation, legal challenges to their autonomy, and bureaucratic hurdles under this restrictive interpretation. Their existence is tied to historical presence and community identity.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_foundations, payer,
    organized, generational, trapped, national).

% The collective bodies of non-Muslim minorities whose cultural and religious continuity depends on the institutional capacity denied by this reading. They bear the costs of cultural erosion, loss of heritage, and inability to self-govern their religious affairs. Exit means abandoning their identity and historical ties.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_communities, payer,
    organized, civilizational, identity_locked, national).

% Schools and seminaries serving minority communities, particularly for training clergy. They face closure, denial of legal personality, and restrictions on curriculum, making it difficult to sustain religious leadership and cultural transmission. Their function is directly targeted by the restrictive reading.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_educational_institutions, payer,
    moderate, generational, trapped, national).

% An international judicial body that hears cases related to human rights violations, including those affecting minorities. While it can issue rulings, their enforcement depends on state compliance, and the Turkish state often asserts its domestic interpretation of Lausanne against such rulings.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% Signatories to the Lausanne Treaty (e.g., UK, France, Italy) who could theoretically intervene diplomatically to ensure its full implementation. However, their political will to challenge Turkey's interpretation is often constrained by broader geopolitical interests, making them effectively excluded from active enforcement of a broader reading.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the state's perspective, it coordinates national sovereignty and legal uniformity by asserting domestic jurisdiction over all institutional matters, preventing external interference in internal affairs.
% TRANSFER_FUNCTION: Transfers institutional autonomy, property rights, and educational control from non-Muslim minority communities to the Turkish state apparatus, consolidating state power and national identity.
% ABSENT_VOICES: The 'expansive_reading' and 'guarantor_reading' proponents (minority advocates, international legal scholars, human rights organizations, and potentially other Lausanne signatory states) are effectively absent from the domestic interpretive process. They would argue for a broader interpretation of treaty obligations and international oversight.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished, minority institutions would immediately seek to reclaim confiscated properties, re-establish theological schools, and assert legal personality, leading to significant legal and administrative reorganization. The state's control over these domains would diminish, and international legal challenges would likely intensify, fundamentally altering the landscape of minority rights in Turkey.
% FOUNDING_PROBLEM: The problem of establishing a new Turkish Republic with clear national sovereignty and a unified legal system after the collapse of the Ottoman Empire, while also addressing the status of non-Muslim minorities inherited from the imperial era.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state maintains that the founding problem of national sovereignty and legal unity remains live, justifying the restrictive reading. Minority communities and international human rights bodies argue that the original problem has evolved, and the current interpretation serves to suppress minority rights rather than genuinely solve a national security or legal unity issue. Historical records and international legal opinions from outside the benefiting parties corroborate the shift in function.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading allows for the systematic transfer of assets (property) and capabilities (education, autonomy) from minority institutions to the state or its proxies. Suppression (0.90) is also very high, as the state actively enforces this interpretation through legal and administrative means, leaving minorities with severely constrained or no exit options for their institutional life. The theater ratio (0.10) is low, indicating that the constraint is genuinely functional in achieving its extractive and suppressive goals, with little performative maintenance. Resistance (0.70) is high, reflecting ongoing legal challenges and international advocacy by minority groups, despite the severe suppression.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state apparatus views this as a legitimate exercise of national sovereignty and domestic legal authority, consistent with its interpretation of the Lausanne Treaty. From the perspective of non-Muslim minority communities and their institutions, it is a highly extractive and suppressive regime that undermines their collective existence and cultural continuity, despite nominal individual worship rights. The engine will compute a Snare classification for the victims and a Beneficiary classification for the state.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the primary beneficiary (d=0.0-0.1) as it consolidates control and potentially assets, with high power and arbitrage-grade exit (it can unilaterally alter its interpretation within its domestic sphere). Non-Muslim minority foundations, religious communities, and educational institutions are the primary targets (d=0.9-1.0), facing property confiscation, denial of legal personality, and educational foreclosure. Their exit options are 'trapped' or 'identity_locked' due to their historical ties to the land and their communities, and the lack of alternative legal frameworks within the state.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare. The coordination story (national sovereignty, domestic legal order) is a cover for the systematic extraction of institutional autonomy and assets from minority groups. The persistence of this constraint is due to active enforcement and suppression of alternatives, not a genuine collective action problem it solves for all parties. The classification prevents mislabeling this as a 'rope' or 'tangled_rope' by highlighting the clear victims and the coercive nature of its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine interpretation of the Lausanne Treaty, or a strategic misreading to enable extraction?',
    'Comparative legal analysis of treaty drafting history, subsequent state practice, and international legal scholarship; rulings by international courts on similar treaty language.',
    'If a genuine interpretation, the constraint is a ''tangled_rope'' reflecting a harsh but legally defensible coordination. If a strategic misreading, it is a ''snare'' where the coordination story is cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''restrictive_reading'' of the ''lausanne_minority_protections'' kernel. Sibling readings (''expansive_reading'', ''guarantor_reading'') would lead to different classifications.').

omega_variable(
    institutional_autonomy_status,
    'Are the institutional autonomy, property ownership, and theological education of non-Muslim minorities genuinely ''domestic matters'' under general Turkish law, or are they implicitly protected by the spirit of the Lausanne Treaty''s minority provisions?',
    'Judicial review by the European Court of Human Rights on specific cases of property confiscation or denial of legal personality, assessing the ''effective protection'' of minority rights.',
    'If implicitly protected, the current application of general law becomes a violation, shifting the constraint towards a ''snare'' with higher suppression and extractiveness due to international illegitimacy. If genuinely domestic, the ''snare'' classification remains, but with less international legal leverage for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_autonomy_status, empirical, 'Ambiguity regarding the scope of ''domestic matters'' versus international treaty obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(laus_tr_t10, lausanne_minority_protections__restrictive_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(laus_be_t10, lausanne_minority_protections__restrictive_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(laus_su_t10, lausanne_minority_protections__restrictive_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'lausanne_minority_protections' kernel. Other readings ('expansive_reading', 'guarantor_reading') exist as separate constraints, each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
