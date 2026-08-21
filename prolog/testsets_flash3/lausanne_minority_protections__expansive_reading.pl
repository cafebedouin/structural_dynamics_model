% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Protections: Expansive Reading of Religious Governance
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents an 'expansive reading' of the Treaty of
 *   Lausanne's protections for non-Muslim minorities in Turkey. It asserts
 *   that the treaty guarantees the functional continuity of pre-1923
 *   religious governance, including institutional self-administration,
 *   property rights, and the ability to train clergy via theological schools.
 *   This reading positions the constraint as a coordination mechanism (a
 *   Rope) that enables minority communities to thrive, with minimal
 *   extraction, but relies on active enforcement by the Turkish state and
 *   international oversight. The core contest is over the scope of these
 *   protections.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.2).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.4).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Protections: Expansive Reading of Religious Governance").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'edd2f007-676f-4cc5-8e7c-c840f628e340').
narrative_ontology:cs_kernel_codification('edd2f007-676f-4cc5-8e7c-c840f628e340', fixed_text).
narrative_ontology:cs_authority_grounding('edd2f007-676f-4cc5-8e7c-c840f628e340', lineage).
narrative_ontology:cs_interpretation_layer_present('edd2f007-676f-4cc5-8e7c-c840f628e340').
narrative_ontology:cs_reading_relation('edd2f007-676f-4cc5-8e7c-c840f628e340', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('edd2f007-676f-4cc5-8e7c-c840f628e340', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('edd2f007-676f-4cc5-8e7c-c840f628e340', foundational, institutional_autonomy_is_guaranteed).
narrative_ontology:cs_axiom_status(institutional_autonomy_is_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('edd2f007-676f-4cc5-8e7c-c840f628e340', institutional_autonomy_is_guaranteed, conventional).
narrative_ontology:cs_axiom('edd2f007-676f-4cc5-8e7c-c840f628e340', foundational, theological_education_is_protected).
narrative_ontology:cs_axiom_status(theological_education_is_protected, holdable).
narrative_ontology:cs_axiom_grounding('edd2f007-676f-4cc5-8e7c-c840f628e340', theological_education_is_protected, conventional).
narrative_ontology:cs_reference_frame('edd2f007-676f-4cc5-8e7c-c840f628e340', post_ottoman_minority_protection_framework).
narrative_ontology:cs_drift_state('edd2f007-676f-4cc5-8e7c-c840f628e340', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('edd2f007-676f-4cc5-8e7c-c840f628e340', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, non_muslim_minority_foundations).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These foundations (e.g., Greek, Armenian, Jewish) directly benefit from the treaty's guarantees of their property rights and institutional autonomy, allowing them to maintain schools, hospitals, and places of worship. Their existence depends on the treaty's enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, non_muslim_minority_foundations, beneficiary,
    moderate, generational, constrained, national).

% The communities themselves, as collective entities, benefit from the ability to self-administer their religious affairs, educate their clergy, and preserve their cultural heritage, all protected by the expansive reading of Lausanne.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_communities, beneficiary,
    moderate, generational, constrained, national).

% As a signatory to the Treaty of Lausanne, the Turkish state is obligated to uphold its provisions. Under the expansive reading, this means actively ensuring the functional continuity of minority religious institutions, even if domestic policies might otherwise restrict them. The state's interpretation and enforcement are critical.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, civilizational, constrained, national).

% Observes and occasionally comments on Turkey's compliance with the Treaty of Lausanne, particularly concerning minority rights. While not directly enforcing, its diplomatic pressure and human rights mechanisms can influence the Turkish state's actions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of a majority state with its non-Muslim religious minorities by establishing a framework for the protection of minority religious institutions, ensuring their functional continuity and self-administration.
% TRANSFER_FUNCTION: Transfers the right to self-administer and own property to minority religious foundations and communities, limiting the state's ability to interfere with these institutions, in exchange for minority loyalty and stability post-Ottoman Empire.
% ABSENT_VOICES: Ultra-nationalist factions within the Turkish state who advocate for a more homogenous national identity and would prefer to see minority institutions fully integrated or dissolved into general domestic law. They are present in political discourse but are formally constrained by the treaty.
% DISAPPEARANCE_RATIONALE: If these protections vanished, minority religious institutions would lose their legal basis for self-administration and property rights, likely leading to their dissolution or absorption into state control, fundamentally altering the social and religious landscape for these communities.
% FOUNDING_PROBLEM: The need to establish a stable framework for the treatment of non-Muslim minorities in the newly formed Republic of Turkey, following the collapse of the Ottoman Empire and population exchanges, to prevent further conflict and ensure regional stability.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and the minority communities themselves attest that the problem of ensuring minority rights and preventing assimilation remains live, requiring ongoing vigilance and enforcement of the treaty provisions. This is corroborated by reports from the European Court of Human Rights and various NGOs.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because this reading primarily grants rights and autonomy, rather than extracting resources. Suppression is moderate (0.4) as the state must actively enforce these protections against potential domestic pressures that might seek to restrict minority rights. Theater ratio is low (0.1) because the protections, when enforced, have tangible effects on the ground. Accessibility collapse is low (0.3) as alternatives (e.g., full assimilation, emigration) are not entirely foreclosed but are made less desirable by the protections. Resistance is low (0.2) from the minority communities, who are beneficiaries, but higher from nationalist elements within the state (not explicitly modeled as a victim here, but acknowledged in 'absent_voices'). The temporal measurements reflect a period of some erosion and then stabilization/partial recovery of these protections over the century.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state, particularly its more nationalist factions, might perceive this expansive reading as an imposition on its sovereignty, leading to a higher perceived extractiveness from their seat. Conversely, minority communities would see it as essential for their survival. The international community, as an observer, would largely align with this expansive reading as consistent with human rights norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Muslim minority foundations and religious communities are clear beneficiaries, as the constraint directly enables their existence and autonomy. The Turkish state, while the primary enforcer, is also structurally constrained by the treaty, making its directionality more complex but leaning towards a symmetric position in this reading, as it gains international legitimacy and stability from compliance. There are no direct 'victims' in this expansive reading, as the constraint is seen as a net positive for all parties involved in its coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in this reading, as the founding problem (ensuring minority rights and stability) is considered 'live'. The coordination function remains vital for the continued existence and self-governance of minority religious institutions. The classification as a Rope prevents mislabeling it as a Snare, which would imply pure extraction, or a Piton, which would suggest its function has atrophied. The ongoing contest over its interpretation, however, means its status as a Rope is perpetually under pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_institutional_autonomy,
    'Does ''functional continuity of pre-1923 religious governance'' imply full institutional autonomy, or is it subject to general domestic law where not explicitly specified?',
    'Further rulings by international courts (e.g., ECHR) or explicit amendments/protocols to the Treaty of Lausanne clarifying the scope of institutional rights versus state sovereignty.',
    'If autonomy is limited by domestic law, the constraint''s effective protection for minorities would decrease, potentially shifting it towards a Tangled Rope or Snare from the minority perspective. If full autonomy is affirmed, its Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_institutional_autonomy, conceptual, 'Ambiguity regarding the extent of institutional self-administration guaranteed by the treaty.').

omega_variable(
    theological_schools_status,
    'Are theological schools explicitly protected under ''functional continuity of religious governance'', or are they considered educational institutions subject to general state education policies?',
    'Specific legal challenges regarding the reopening or operation of theological schools, leading to domestic or international court decisions.',
    'If theological schools are not explicitly protected, a key aspect of clergy formation would be lost, weakening the overall functional continuity and increasing the vulnerability of minority communities, potentially raising extractiveness from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_schools_status, empirical, 'Contested status of theological schools under Lausanne protections.').

omega_variable(
    reading_contest_impact,
    'How would the ''restrictive_reading'' or ''guarantor_reading'' alter the effective extractiveness and suppression experienced by minority communities?',
    'Comparative analysis of legal outcomes and lived experiences under different interpretive regimes, or a shift in the dominant legal interpretation.',
    'A shift to the ''restrictive_reading'' would drastically increase extractiveness and suppression for minorities, likely reclassifying the constraint as a Snare. The ''guarantor_reading'' would likely strengthen the Rope classification by providing external enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_impact, conceptual, 'Impact of alternative readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(laus_tr_t1950, lausanne_minority_protections__expansive_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(laus_tr_t1975, lausanne_minority_protections__expansive_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(laus_tr_t2000, lausanne_minority_protections__expansive_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__expansive_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.2).
narrative_ontology:measurement(laus_be_t1950, lausanne_minority_protections__expansive_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(laus_be_t1975, lausanne_minority_protections__expansive_reading, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(laus_be_t2000, lausanne_minority_protections__expansive_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 2023, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.4).
narrative_ontology:measurement(laus_su_t1950, lausanne_minority_protections__expansive_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(laus_su_t1975, lausanne_minority_protections__expansive_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(laus_su_t2000, lausanne_minority_protections__expansive_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__expansive_reading, suppression_requirement, 2023, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'lausanne_minority_protections' kernel. Other readings include 'restrictive_reading' and 'guarantor_reading', each representing a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
