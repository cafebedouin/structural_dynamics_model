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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: 1890 Manifesto: Institutional Pragmatism Reading
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto from an 'institutional
 *   pragmatism' reading, where the church's doctrinal claims (revelation
 *   narrative) served to legitimate a survival-driven capitulation to
 *   superior coercive power (the U.S. federal government). This reading
 *   highlights the strategic adaptation of the institution to external
 *   pressure, resulting in a public suspension of plural marriage while
 *   allowing for secret continuations until 1904. The beneficiary set
 *   includes church leadership and institutional survival, while victims
 *   include coerced polygamists and deceived monogamists. The constraint is
 *   classified as a tangled_rope due to its dual function of institutional
 *   coordination and asymmetric extraction, maintained by active enforcement
 *   and doctrinal legitimation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.78).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.85).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto: Institutional Pragmatism Reading").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '5d2e3454-94b0-4a48-8c89-a765d31ee924').
narrative_ontology:cs_kernel_codification('5d2e3454-94b0-4a48-8c89-a765d31ee924', fixed_text).
narrative_ontology:cs_authority_grounding('5d2e3454-94b0-4a48-8c89-a765d31ee924', extraction).
narrative_ontology:cs_interpretation_layer_present('5d2e3454-94b0-4a48-8c89-a765d31ee924').
narrative_ontology:cs_reading_relation('5d2e3454-94b0-4a48-8c89-a765d31ee924', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d2e3454-94b0-4a48-8c89-a765d31ee924', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('5d2e3454-94b0-4a48-8c89-a765d31ee924', foundational, institutional_survival_trumps_doctrinal_practice).
narrative_ontology:cs_axiom_status(institutional_survival_trumps_doctrinal_practice, holdable).
narrative_ontology:cs_axiom_grounding('5d2e3454-94b0-4a48-8c89-a765d31ee924', institutional_survival_trumps_doctrinal_practice, instrumental).
narrative_ontology:cs_axiom('5d2e3454-94b0-4a48-8c89-a765d31ee924', foundational, revelation_narrative_legitimates_pragmatic_shifts).
narrative_ontology:cs_axiom_status(revelation_narrative_legitimates_pragmatic_shifts, holdable).
narrative_ontology:cs_axiom_grounding('5d2e3454-94b0-4a48-8c89-a765d31ee924', revelation_narrative_legitimates_pragmatic_shifts, conventional).
narrative_ontology:cs_reference_frame('5d2e3454-94b0-4a48-8c89-a765d31ee924', institutional_pragmatism_as_divine_guidance).
narrative_ontology:cs_drift_state('5d2e3454-94b0-4a48-8c89-a765d31ee924', post_1904_official_cessation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5d2e3454-94b0-4a48-8c89-a765d31ee924', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival).
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

% Issued the 1890 Manifesto, publicly suspending plural marriage while privately allowing its continuation for a period. Benefited from restored political rights and institutional survival, but bore the cost of internal dissent and managing the public/private contradiction. Their legitimacy was tied to the revelation narrative.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Were compelled by federal law and church pressure to abandon or conceal plural marriages, often leading to family dissolution, legal persecution, or internal exile. Their identity was deeply tied to the practice, making exit from the church unthinkable despite the personal cost.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, identity_locked, local).

% Were led to believe plural marriage had ceased, while some church leaders and members continued the practice in secret. They bore the cost of institutional deception and the erosion of trust, but their commitment to the church often constrained their exit options.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, biographical, constrained, local).

% The abstract entity representing the continued existence and flourishing of the church as an organization. It benefited from the Manifesto by avoiding federal confiscation of assets and the disenfranchisement of its members, ensuring its long-term viability.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival).

% Exerted coercive pressure through anti-polygamy legislation, asset confiscation, and imprisonment of church leaders. Its goal was to enforce federal law and assimilate the church into mainstream American society, ultimately forcing the Manifesto.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the church's continued existence and the restoration of political rights, allowing them to participate in civic life without persecution. Their loyalty to the church and its leadership made them accept the Manifesto as a necessary adaptation, despite potential internal conflicts.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, loyal_members, beneficiary,
    moderate, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the church's public stance with federal law to ensure institutional survival and the restoration of political rights, while attempting to manage internal doctrinal consistency and member loyalty.
% TRANSFER_FUNCTION: Transferred the burden of compliance and doctrinal ambiguity onto individual members, particularly those in plural marriages, while transferring political legitimacy and institutional stability to the church leadership.
% ABSENT_VOICES: Those who felt betrayed by the perceived abandonment of a divine commandment, or those who continued plural marriage in secret, were largely silenced or marginalized within the official discourse, their dissent managed through internal disciplinary actions.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its pragmatic interpretation vanished, the church's historical narrative of divine guidance through adaptation would be fundamentally challenged, potentially leading to a crisis of legitimacy and a re-evaluation of its relationship with secular authority. The institutional structure would need to re-legitimate its past actions.
% FOUNDING_PROBLEM: The church faced existential threat from the U.S. federal government due to its practice of plural marriage, risking confiscation of property, disenfranchisement of members, and the imprisonment of its leaders.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal legislation, and contemporary accounts from both church members and external observers corroborate the severe existential threat posed by the federal government. The problem of direct federal persecution over plural marriage is now dead, but the institutional adaptation persists.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the Manifesto imposed significant personal costs on members, particularly those in plural marriages, while benefiting the institution. Suppression is very high (0.85) due to the combined coercive power of the federal government and the church's internal disciplinary mechanisms, which enforced compliance and managed dissent. The theater ratio is substantial (0.65) because the public declaration of suspending plural marriage was largely performative, masking a period of continued, albeit secret, practice. Accessibility collapse is high (0.70) as alternatives to compliance were severely limited for members, and resistance is moderate (0.40) due to the strong internal loyalty and external pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, the Manifesto was a necessary, divinely guided adaptation for institutional survival. From the perspective of coerced polygamists, it was a devastating betrayal enforced by both secular and religious authority. This reading emphasizes the structural asymmetry of costs and benefits, where the institution's survival was prioritized over individual well-being, legitimated by a flexible interpretation of revelation.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and institutional survival are primary beneficiaries (low d) as the Manifesto secured the church's future and political standing. Coerced polygamists and deceived monogamists are clear targets (high d) as they bore the direct costs of legal persecution, family disruption, and institutional deception. Loyal members are beneficiaries (low d) as they gained from the church's continued existence and restored civic participation, accepting the pragmatic adaptation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival in the face of federal persecution) was resolved by the early 20th century. However, the 'revelation narrative' used to legitimate the pragmatic shift continued to function, creating a theatrical element where the original justification for the constraint (divine command for plural marriage) was publicly suspended but privately maintained, then later reinterpreted. This prevents mislabeling it as pure extraction by acknowledging the initial coordination function for institutional survival, but highlights the subsequent drift into a more extractive and theatrical mode as the original problem faded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''institutional pragmatism'' reading of the plural marriage mandate kernel?',
    'Comparative analysis with other readings (exogenous_override_reading, endogenous_reinterpretation_reading) to assess the distinctness of the structural delta and beneficiary/victim sets.',
    'If misidentified, the classification of this constraint would shift to align with the structural properties of the correct reading, altering its extractiveness, suppression, and claimed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the plural marriage mandate kernel.').

omega_variable(
    duration_of_secret_practice,
    'What was the true extent and duration of secret plural marriage practice after the 1890 Manifesto, and how widely was it known within the church?',
    'Further historical research, including analysis of private diaries, letters, and disciplinary records, to quantify the scale and awareness of post-Manifesto plural marriages.',
    'A longer or more widespread secret practice would increase the measured theater_ratio and extractiveness, as the public declaration would be even more performative and the deception more pervasive. It would also amplify the victim status of deceived monogamists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_of_secret_practice, empirical, 'Quantifies the gap between public declaration and private practice.').

omega_variable(
    internalized_suppression_of_dissent,
    'To what extent did the suppression of dissent against the Manifesto become internalized by loyal members, beyond overt institutional coercion?',
    'Sociological studies of intergenerational memory, oral histories, and analysis of personal narratives from descendants of those affected, focusing on self-censorship and identity-based compliance.',
    'If internalized suppression was significant, the effective suppression for loyal members and coerced polygamists would be higher than structural measures suggest, as they carried the suppression within their identity and community norms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_dissent, empirical, 'Assesses the role of internalized vs. structural suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.58).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.62).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.65).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.65).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.7).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.75).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.78).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.75).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.8).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.83).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'plural_marriage_mandate' kernel. This 'institutional pragmatism' reading focuses on the strategic adaptation of the church to external coercive power, using doctrinal claims to legitimate survival-driven capitulation. It differs from the 'exogenous override' reading (which emphasizes federal coercion as the sole driver) and the 'endogenous reinterpretation' reading (which frames it as legitimate prophetic reinterpretation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
