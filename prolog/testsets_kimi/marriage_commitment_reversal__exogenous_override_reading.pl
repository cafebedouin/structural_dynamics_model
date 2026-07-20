% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercive Override of LDS Plural Marriage Practice (Exogenous Override Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   In 1890, the LDS Church issued a Manifesto suspending plural marriage
 *   under sustained federal coercion, including the Edmunds-Tucker Act's
 *   threat of property confiscation and disincorporation. Critically, the
 *   Church never removed Section 132 from its canon, preserving the
 *   marriage-commitment principle as divine revelation while reversing public
 *   practice. This constraint story models the exogenous override reading of
 *   that reversal: the practice changed not because doctrine was revised
 *   through internal revelatory authority, but because federal territorial
 *   power extracted institutional autonomy from the Church. The beneficiary
 *   is federal territorial control; the victim is LDS institutional
 *   sovereignty. The doctrine-practice gap persists as a structural wound.
 *
 * KEY AGENTS:
 *   - Federal Government (agenda_setter/beneficiary): Institutional power with analytical exit; extracts territorial integration by criminalizing plural marriage and threatening institutional dissolution.
 *   - LDS Church Hierarchy (payer): Institutional power with identity-locked exit; forced to publicly suspend practice while internally preserving Section 132, bearing the cost of lost autonomy and doctrinal dissonance.
 *   - LDS Membership (payer): Organized power with identity-locked exit; bears the family-level and spiritual costs of a practice forbidden but still canonized.
 *   - Political Theologian (observer): Analytical seat tracking the coercion-to-compliance pipeline and its long-term institutional effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercive Override of LDS Plural Marriage Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'ef74dd87-3361-4466-adab-29798b8cad1e').
narrative_ontology:cs_kernel_codification('ef74dd87-3361-4466-adab-29798b8cad1e', fixed_text).
narrative_ontology:cs_authority_grounding('ef74dd87-3361-4466-adab-29798b8cad1e', lineage).
narrative_ontology:cs_interpretation_layer_present('ef74dd87-3361-4466-adab-29798b8cad1e').
narrative_ontology:cs_reading_relation('ef74dd87-3361-4466-adab-29798b8cad1e', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef74dd87-3361-4466-adab-29798b8cad1e', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('ef74dd87-3361-4466-adab-29798b8cad1e', foundational, coerced_manifesto_preserves_doctrine_intact).
narrative_ontology:cs_axiom_status(coerced_manifesto_preserves_doctrine_intact, holdable).
narrative_ontology:cs_axiom_grounding('ef74dd87-3361-4466-adab-29798b8cad1e', coerced_manifesto_preserves_doctrine_intact, conventional).
narrative_ontology:cs_axiom('ef74dd87-3361-4466-adab-29798b8cad1e', foundational, federal_sovereignty_supersedes_territorial_religious_practice).
narrative_ontology:cs_axiom_status(federal_sovereignty_supersedes_territorial_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('ef74dd87-3361-4466-adab-29798b8cad1e', federal_sovereignty_supersedes_territorial_religious_practice, conventional).
narrative_ontology:cs_reference_frame('ef74dd87-3361-4466-adab-29798b8cad1e', lds_doctrine_practice_integrity).
narrative_ontology:cs_drift_state('ef74dd87-3361-4466-adab-29798b8cad1e', post_1890_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ef74dd87-3361-4466-adab-29798b8cad1e', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_church_hierarchy).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the Edmunds-Tucker Act, federal courts, territorial administration, and the statehood threat to compel the LDS Church to publicly abandon plural marriage. Collects territorial political integration, monopoly over marriage law, and the elimination of competing sovereign authority in the Utah Territory.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, federal_government, beneficiary).

% Issues the 1890 Manifesto suspending public plural marriage under explicit federal duress while preserving Section 132 as canonized divine principle. Loses institutional autonomy to define valid marriage, is forced to publicly misalign practice with doctrine, and faces property confiscation and disincorporation if it resists.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_church_hierarchy, payer,
    institutional, generational, identity_locked, national).

% Expected to abandon plural marriage practice while Section 132 remains canonized scripture. Bears the doctrinal dissonance and family-structure disruption caused by the gap between preserved principle and coerced practice; has no seat at the federal negotiation and cannot exit without abandoning religious identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_membership, payer,
    organized, generational, identity_locked, national).

% Analyzes the structural relationship between federal coercion and the resulting doctrine-practice gap. Evaluates whether the reversal was driven by external threat or internal revelation and tracks the long-term institutional consequences of compliance without doctrinal revision.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, political_theologian, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Federal authorities coordinated the political integration of the Utah Territory under uniform United States marriage law by eliminating plural marriage as a visible political and social structure.
% TRANSFER_FUNCTION: Moves the authority to legitimate and regulate marriage from the LDS institutional hierarchy to the federal territorial and statehood apparatus; moves public compliance from voluntary doctrinal practice to coerced behavioral suspension.
% ABSENT_VOICES: Practicing LDS families and local plural marriage communities were excluded from the federal negotiation; dissenting federal voices opposing heavy-handed territorial coercion were marginalized; future LDS theological voices who would have to reconcile the preserved canon with suspended practice were not yet in the room.
% DISAPPEARANCE_RATIONALE: If the federal threat vanished overnight in 1890, the LDS hierarchy had not renounced Section 132 and would likely have resumed public plural marriage; the Manifesto would not have been necessary, and territorial marriage governance would have remained contested, rearranging both religious practice and political status.
% FOUNDING_PROBLEM: Federal framers asserted the problem was theocratic rebellion and the violation of federal marriage law in US territories; the LDS institution asserted the problem was federal violation of religious liberty and territorial self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Federal sources including the Edmunds-Tucker Act legislative record and Supreme Court opinions (Reynolds v. US) corroborate the federal framing from the agenda-setter seat. LDS sources including the 1890 Manifesto wording, Wilford Woodruff's private correspondence, and post-Manifesto continuation of plural marriages corroborate the coerced-compliance framing from the payer seat. No contemporaneous neutral third party attested exclusively for one side.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the federal government effectively captured the authority to define and legitimate marriage in the territory, extracting a core sovereign function from the LDS institutional structure. Suppression is higher (0.88) because the constraint's persistence depended entirely on active federal enforcementâlegislation, courts, and the statehood vetoârather than on doctrinal consensus or voluntary coordination. Theater ratio is moderate (0.30) and rising: the 1890 Manifesto was carefully worded to appear as religious counsel while functioning as coerced compliance, and the growing doctrine-practice gap increased performative maintenance over time. Accessibility collapse is high (0.78) because once the federal legal framework was established, open plural marriage became structurally impossible; resistance is substantial (0.68) because the Church resisted for years before 1890, but compliance was ultimately forced.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is legitimate territorial law enforcement integrating a rebellious territory under national marriage norms; from the LDS payer seats, the identical structure is an extraction of religious institutional autonomy by sovereign threat. The engine computes this divergence from the structural data: the federal agent is the declared beneficiary with analytical exit, while the LDS agents are declared victims with identity-locked exit, yielding diametrically opposed directionality values.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the structural beneficiary and agenda-setter (d near 0.0): it collects territorial control and legal monopoly. The LDS hierarchy and membership are structural targets (d near 1.0): they bear the extraction of sovereignty, are locked in by religious identity, and have no viable exit that preserves their institutional or spiritual continuity. The identity-locked exit amplifies effective extraction for the LDS seats because the cost of leaving the constraint is fused to self-concept and communal belonging.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a snare prevents mislabeling the federal constraint as mere coordination (rope) or the LDS compliance as willing doctrinal evolution (which would align with the endogenous reinterpretation reading). The active enforcement requirement, the explicit victim set, and the preserved-but-unpracticed doctrine together certify that the mandate was not internally resolved but externally overridden. The persistence of Section 132 as canon proves the institutional mandate was not revised; it was captured. This blocks the false conclusion that the constraint naturally atrophied or was consensually reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_driver,
    'Was the 1890 reversal driven primarily by federal coercive threat or by internal prophetic revelation?',
    'Comparative analysis of federal enforcement timing versus claimed revelatory dates; review of internal LDS correspondence and diary records from the 1887-1890 interval.',
    'If revelation was primary, this reading''s classification as snare weakens toward tangled_rope or rope; if coercion was primary, snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_driver, empirical, 'Empirical ambiguity over the primary causal driver of the 1890 practice reversal.').

omega_variable(
    doctrine_practice_gap_stability,
    'Can Section 132 remain canonized indefinitely while practice remains suspended, or does the gap generate recursive institutional pressure?',
    'Longitudinal study of LDS doctrinal development post-1904; analysis of official curriculum, temple theology, and institutional rhetoric regarding Section 132.',
    'If the gap eventually forces doctrinal revision, the exogenous override reading shifts toward the practice_doctrine_gap sibling; if the principle is permanently preserved as inert text, the exogenous override remains stable as snare with potential piton characteristics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_gap_stability, conceptual, 'Conceptual ambiguity over the long-term stability of the doctrine-practice gap.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression purely structural (federal law and military threat) or did it become internalized within LDS institutional identity (self-policing to maintain legitimacy)?',
    'Analysis of post-1904 enforcement: whether federal coercion was still required to prevent public practice, or whether the Church enforced the suspension internally through disciplinary mechanisms.',
    'If internalized, effective extraction persists even after the federal threat recedes, altering the temporal trajectory and potentially shifting terminal classification toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Ambiguity over whether suppression remained external or became internalized within LDS institutional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_exog_tr_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(mcr_exog_tr_t0, observed).
narrative_ontology:measurement(mcr_exog_tr_t5, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(mcr_exog_tr_t5, observed).
narrative_ontology:measurement(mcr_exog_tr_t10, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(mcr_exog_tr_t10, observed).
narrative_ontology:measurement(mcr_exog_tr_t15, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(mcr_exog_tr_t15, observed).
narrative_ontology:measurement(mcr_exog_tr_t20, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(mcr_exog_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(mcr_exog_be_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(mcr_exog_be_t0, observed).
narrative_ontology:measurement(mcr_exog_be_t5, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement_basis(mcr_exog_be_t5, observed).
narrative_ontology:measurement(mcr_exog_be_t10, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement_basis(mcr_exog_be_t10, observed).
narrative_ontology:measurement(mcr_exog_be_t15, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement_basis(mcr_exog_be_t15, observed).
narrative_ontology:measurement(mcr_exog_be_t20, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement_basis(mcr_exog_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(mcr_exog_su_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(mcr_exog_su_t0, observed).
narrative_ontology:measurement(mcr_exog_su_t5, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement_basis(mcr_exog_su_t5, observed).
narrative_ontology:measurement(mcr_exog_su_t10, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement_basis(mcr_exog_su_t10, observed).
narrative_ontology:measurement(mcr_exog_su_t15, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement_basis(mcr_exog_su_t15, observed).
narrative_ontology:measurement(mcr_exog_su_t20, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 20, 0.92).
narrative_ontology:measurement_basis(mcr_exog_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the marriage_commitment_reversal kernel, decomposed per the Îµ-invariance principle because the exogenous coercion reading carries a substantially different Îµ (high extraction, active suppression) than the endogenous reinterpretation reading (lower extraction, revelatory authority) or the gap reading (structural ambiguity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
