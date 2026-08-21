% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: Plural Marriage Mandate (Institutional Pragmatism Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto through an
 *   'institutional pragmatism' lens, where the church's doctrinal claims (the
 *   revelation narrative) served to legitimate a survival-driven capitulation
 *   to superior federal coercive power. The constraint is a Tangled Rope: it
 *   provided a coordination function for institutional survival but extracted
 *   significant costs from members, enforced through a performative doctrinal
 *   shift. The M-set gap (doctrine unchanged, practice suspended, secret
 *   continuations) is a primary observable. The beneficiary set is primarily
 *   the church leadership (institutional survival, restored political
 *   rights), while victims include both coerced polygamists and deceived
 *   monogamists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.7).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.8).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "Plural Marriage Mandate (Institutional Pragmatism Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'e4e91b46-3f7a-42b8-8cf7-ba57658e02d6').
narrative_ontology:cs_kernel_codification('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', formalized).
narrative_ontology:cs_authority_grounding('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', lineage).
narrative_ontology:cs_interpretation_layer_present('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6').
narrative_ontology:cs_reading_relation('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', foundational, doctrinal_flexibility_for_survival).
narrative_ontology:cs_axiom_status(doctrinal_flexibility_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', doctrinal_flexibility_for_survival, instrumental).
narrative_ontology:cs_axiom('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', secondary, revelation_as_legitimation_tool).
narrative_ontology:cs_axiom_status(revelation_as_legitimation_tool, holdable).
narrative_ontology:cs_axiom_grounding('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', revelation_as_legitimation_tool, conventional).
narrative_ontology:cs_reference_frame('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', institutional_survival_imperative).
narrative_ontology:cs_drift_state('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e4e91b46-3f7a-42b8-8cf7-ba57658e02d6', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, loyal_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orchestrated the suspension of plural marriage through the 1890 Manifesto, framing it as divine revelation. This secured institutional survival, prevented federal confiscation of assets, and restored political rights, benefiting the church's long-term viability and their own authority.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Were compelled to abandon a practice they believed was a divine commandment, facing profound personal and spiritual upheaval. Many experienced the loss of family structures and social standing, with some continuing the practice secretly at great risk.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, identity_locked, regional).

% Accepted the official narrative of divine reinterpretation, reconciling their faith with the new policy. However, the continued secret practice of plural marriage by some leaders, if discovered, would lead to a crisis of trust and a sense of deception regarding the institutional claims.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    powerless, biographical, identity_locked, regional).

% Applied sustained coercive pressure (confiscation of property, disenfranchisement, imprisonment) to force the church to abandon plural marriage. Its policy goal was achieved through the Manifesto, leading to the church's eventual political and social reintegration.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the church's survival and its reintegration into mainstream American society, avoiding further persecution. They had to reconcile the doctrinal shift with their faith, often accepting the official narrative of divine guidance.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, loyal_members, beneficiary,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the church's institutional adaptation to overwhelming federal coercive power, allowing it to avoid dissolution, regain legal standing, and continue its mission within the United States.
% TRANSFER_FUNCTION: Transferred the burden of doctrinal reconciliation and the personal costs of abandoning plural marriage onto individual members, while transferring institutional legitimacy, property, and political acceptance back to the church leadership.
% ABSENT_VOICES: Those who viewed plural marriage as an eternal, unchangeable divine command and refused to comply with the Manifesto. Many formed splinter groups or continued the practice in secret, their dissent suppressed by the institutional authority and the threat of excommunication.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its subsequent enforcement had not occurred, the church would have faced continued and escalating federal persecution, potentially leading to its complete dissolution, confiscation of all assets, and the permanent disenfranchisement of its members. The entire trajectory of the institution and its community would have been fundamentally altered.
% FOUNDING_PROBLEM: The existential threat posed by the federal government's anti-polygamy legislation, which included property confiscation, imprisonment of leaders, and the potential disincorporation of the church, jeopardizing its very existence.
% FOUNDING_PROBLEM_CORROBORATION: Extensive historical records, federal court decisions, and independent scholarly analyses from outside the church leadership corroborate the severe existential threat faced by the institution. While the church leadership frames the resolution as divine intervention, external sources highlight the pragmatic adaptation to superior coercive force.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the institutional benefit (survival) came at a significant cost to members who had to abandon a divinely sanctioned practice or live with deception. Suppression is high due to the active enforcement by church leadership to ensure compliance and manage dissent, alongside the federal government's continued pressure. Theater ratio is moderate-high, reflecting the performative aspect of presenting a pragmatic institutional decision as a new divine revelation, especially given the documented secret continuations of plural marriage by some leaders post-Manifesto. Accessibility collapse is moderate; while official practice ceased, the underlying doctrine remained, and exiting the church meant losing community and identity. Resistance was moderate, manifesting as internal dissent and the formation of splinter groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the church leadership, the Manifesto was a divinely guided act of institutional preservation. From the perspective of coerced polygamists, it was a betrayal of divine command enforced by institutional power. The engine's classification will highlight this divergence, showing a Tangled Rope for those bearing the costs, while the leadership's seat might compute closer to a Rope due to their perceived benefits and control.
 *
 * DIRECTIONALITY LOGIC:
 *   The church leadership is the primary beneficiary (low d) as they secured the institution's survival and their own authority. Coerced polygamists and deceived monogamists are clear targets (high d), bearing the direct costs of the policy shift and the associated spiritual/social upheaval. The federal government acted as an external agenda-setter, applying pressure that shaped the constraint. Loyal members are beneficiaries (low d) of the institutional survival, but also bear indirect costs of doctrinal reconciliation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_pragmatism_ambiguity,
    'Was the 1890 Manifesto a genuine divine revelation, or a pragmatic institutional adaptation presented as revelation to legitimate capitulation to superior coercive power?',
    'Analysis of internal church records, private correspondence of leaders, and comparison with external historical accounts of federal pressure. Evidence of strategic decision-making preceding the ''revelation'' would support the pragmatic reading.',
    'If primarily pragmatic, the constraint''s theater_ratio and extractiveness are higher, reinforcing its Tangled Rope classification. If genuinely revelatory, the constraint leans more towards a Rope, with lower extraction and theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revelation_vs_pragmatism_ambiguity, empirical, 'Ambiguity between divine revelation and institutional pragmatism.').

omega_variable(
    exogenous_override_reading_delta,
    'How would the classification change if the `exogenous_override_reading` (federal coercion forced abandonment) were adopted?',
    'This reading would emphasize the federal government as the primary agenda_setter and the church leadership as a victim of external force, shifting the beneficiary/victim structure and potentially lowering the church leadership''s directionality.',
    'The constraint might be classified as a Snare from the federal government''s perspective (pure extraction of compliance), or a Rope for the church leadership (coordination to survive an external threat), depending on the emphasis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_override_reading_delta, conceptual, 'Impact of adopting the exogenous override reading.').

omega_variable(
    endogenous_reinterpretation_reading_delta,
    'How would the classification change if the `endogenous_reinterpretation_reading` (legitimate prophetic reinterpretation) were adopted?',
    'This reading would emphasize the internal theological justification for the change, framing it as a legitimate evolution of doctrine. It would likely minimize extraction and theater, presenting the constraint as a Rope or even a Mountain (divine law).',
    'The constraint would likely compute as a Rope or even a Mountain from the perspective of loyal members, with significantly lower extractiveness and theater_ratio, as the costs would be framed as necessary for spiritual progression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_reinterpretation_reading_delta, conceptual, 'Impact of adopting the endogenous reinterpretation reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (federal pressure, institutional authority) or internalized (faith, identity fusion)?',
    'Post-exit suppression trajectory: if individuals who leave the church continue to self-regulate their behavior or experience internal conflict regarding plural marriage, it suggests internalized suppression. If compliance immediately ceases upon exit, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1905).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.7).
narrative_ontology:measurement(plur_tr_t1893, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1893, 0.65).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.6).
narrative_ontology:measurement(plur_tr_t1899, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1899, 0.55).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.52).
narrative_ontology:measurement(plur_tr_t1905, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1905, 0.5).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(plur_be_t1893, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1893, 0.68).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.7).
narrative_ontology:measurement(plur_be_t1899, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1899, 0.72).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.74).
narrative_ontology:measurement(plur_be_t1905, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1905, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(plur_su_t1893, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1893, 0.78).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.8).
narrative_ontology:measurement(plur_su_t1899, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1899, 0.82).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.83).
narrative_ontology:measurement(plur_su_t1905, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1905, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
