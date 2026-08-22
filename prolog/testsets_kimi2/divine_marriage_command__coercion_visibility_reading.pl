% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command â Coercion Visibility Reading
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   The LDS divine marriage command (plural marriage) was publicly suspended
 *   by the 1890 Manifesto under explicit federal coercion. This
 *   readingâcoercion_visibilityâacknowledges that the Manifesto was a
 *   pragmatic response to state pressure and that the post-Manifesto church's
 *   theological legitimacy derives from institutional survival necessity
 *   rather than from revelatory supersession. The constraint coordinates the
 *   survival of the institution while extracting theological coherence from
 *   members who must accept a coerced doctrinal shift. The arrangement is
 *   actively enforced against continuationist dissent and is claimed as
 *   necessary coordination, but the metrics are authored independently: high
 *   extraction (theological destabilization acknowledged), high suppression
 *   (of fundamentalist practice), and moderate theater (survival-necessity
 *   framing is partially performative, partially genuine crisis response).
 *
 * KEY AGENTS:
 *   - Church hierarchy (agenda_setter/beneficiary): institutional power, identity-locked exit â administers the shift and collects institutional survival
 *   - Monogamist laity (beneficiary): organized power, constrained exit â receives mainstream acceptance and institutional continuity
 *   - Continuationist believers (payer): powerless, trapped exit â bear theological betrayal and suppression for maintaining the original command
 *   - Federal state (excluded): institutional, analytical exit â historical coercer outside current theological legitimation
 *   - External historians (observer): analytical power â document the coercion from outside the commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.72).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.85).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command â Coercion Visibility Reading").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '93023617-9f5b-4a5c-8bb3-14410f06ed6b').
narrative_ontology:cs_kernel_codification('93023617-9f5b-4a5c-8bb3-14410f06ed6b', fixed_text).
narrative_ontology:cs_authority_grounding('93023617-9f5b-4a5c-8bb3-14410f06ed6b', lineage).
narrative_ontology:cs_interpretation_layer_present('93023617-9f5b-4a5c-8bb3-14410f06ed6b').
narrative_ontology:cs_reading_relation('93023617-9f5b-4a5c-8bb3-14410f06ed6b', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('93023617-9f5b-4a5c-8bb3-14410f06ed6b', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_axiom('93023617-9f5b-4a5c-8bb3-14410f06ed6b', foundational, coerced_doctrinal_accommodation_legitimate).
narrative_ontology:cs_axiom_status(coerced_doctrinal_accommodation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('93023617-9f5b-4a5c-8bb3-14410f06ed6b', coerced_doctrinal_accommodation_legitimate, conventional).
narrative_ontology:cs_axiom('93023617-9f5b-4a5c-8bb3-14410f06ed6b', foundational, divine_providence_through_institutional_survival).
narrative_ontology:cs_axiom_status(divine_providence_through_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding('93023617-9f5b-4a5c-8bb3-14410f06ed6b', divine_providence_through_institutional_survival, theological).
narrative_ontology:cs_reference_frame('93023617-9f5b-4a5c-8bb3-14410f06ed6b', divine_command_eternal).
narrative_ontology:cs_drift_state('93023617-9f5b-4a5c-8bb3-14410f06ed6b', contemporary_acknowledged_coercion, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('93023617-9f5b-4a5c-8bb3-14410f06ed6b', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, monogamist_laity).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, continuationist_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrinal shift from plural marriage to public monogamy, acknowledging the 1890 Manifesto as a response to federal coercion. Derives continued legitimacy from institutional survival rather than from revelatory certainty about the shift. Cannot exit the constraint without dissolving the authority structure they inhabit.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, church_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, church_hierarchy, beneficiary).

% Receive normalized status with the federal government and mainstream society through the church's continued existence. Their religious identity is tied to the institution that survived because of the Manifesto. They do not practice plural marriage and largely accept the institutional survival narrative, though some experience theological dissonance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, monogamist_laity, beneficiary,
    organized, biographical, constrained, global).

% Bear the theological and social costs of the acknowledged coerced shift. They hold that the original divine marriage command remains in force and view the Manifesto as illegitimate capitulation. They face excommunication, criminal prosecution, and social ostracism if they practice plural marriage, and experience doctrinal betrayal by the acknowledged non-revelatory origin of the change.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, continuationist_believers, payer,
    powerless, biographical, trapped, regional).

% Applied coercive pressure through the Edmunds-Tucker Act and anti-polygamy statutes that forced the doctrinal shift. No longer participates in the theological legitimation of the post-Manifesto church, but its historical coercion is the acknowledged proximate cause of the constraint's current form.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_state, excluded,
    institutional, generational, analytical, national).

% Document the federal coercion and institutional response from outside the theological framework. They corroborate the historical duress but do not participate in the legitimacy claims of any reading.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, external_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate institutional survival under federal persecution by shifting public practice from plural to monogamous marriage, preserving corporate property, leadership continuity, and organizational existence.
% TRANSFER_FUNCTION: Moves theological authority from the eternal divine-command frame to the institutional-survival frame; transfers the compliance burden from the institution to continuationist believers who must either abandon their theology or face expulsion and prosecution.
% ABSENT_VOICES: Fundamentalist and polygamous practitioners who view the Manifesto as apostate compromise and await restoration; federal prosecutors who engineered the coercion but do not participate in theological meaning-making.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the church would either revert to plural marriage (triggering renewed federal conflict and schism) or fracture into competing mainstream and fundamentalist factions, destroying current institutional unity and the post-1890 authority structure.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation, disincorporation, and property seizure threatening the church's institutional extinction in the late 19th century.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records (Edmunds-Tucker Act), Supreme Court rulings, and non-Mormon historians corroborate the existential threat. Continuationist scholars outside the benefiting hierarchy also attest the coercion, though they dispute the legitimacy of the response.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the acknowledged coerced origin extracts theological certainty from believers who must reconcile eternal commands with survival-driven suspension. Suppression (0.85) is high because federal and ecclesiastical enforcement jointly suppress polygamous practice and continuationist organizing. Theater ratio (0.45) is moderate: the survival narrative is partially genuine (the federal threat was real) and partially performed to cover the revelatory gap. Accessibility collapse (0.78) is high because alternatives (fundamentalist exit or apostasy) are structurally and socially closed. Resistance (0.55) reflects persistent but contained continuationist dissent. The measurement series share a single time grid to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (church hierarchy) experiences the constraint as genuine coordination preserving the institution against impossible odds; the payer seat (continuationist believers) experiences it as betrayal and extraction that destabilizes their theological world. The monogamist laity experiences mixed benefits and costs. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy is the structural beneficiary (d near 0.0) because the constraint subsidizes institutional survival and authority. Continuationist believers are the structural targets (d near 1.0) because they bear the theological and social costs of the coerced shift with trapped exit. Monogamist laity sits near symmetric (0.5): they benefit from institutional continuity and social acceptance but pay through theological thinning and cognitive dissonance about the coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal extinction threat) is dead, yet the constraint persists and has deepened. Classification as tangled_rope captures that the original coordination (survival under coercion) was genuine, but ongoing extraction (theological compliance despite acknowledged non-revelatory origins) requires active enforcement. If the constraint were purely a scaffold, it would have sunset when the federal threat receded; instead it hardened into permanent orthodoxy backed by suppression of the original command, indicating extraction layered onto coordination. The theater-ratio peak around 1940 reflects maximal performative maintenance when the institutional survival narrative was most strained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_acknowledgement_locus,
    'Is the coerced origin of the Manifesto acknowledged by the authority structure itself in its official discourse, or only inferred by external analytical observers and dissenting insiders?',
    'Systematic review of official curriculum, First Presidency statements, and correlated instructional materials for explicit versus implicit acknowledgement of federal coercion as the proximate cause.',
    'If acknowledgement is only external, the authority structure operates as extraction-with-drift-denial (snare-like); if internal and explicit, the constraint is a genuine paradigm shift toward survival-legitimacy (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_acknowledgement_locus, empirical, 'Whether coercion acknowledgement is internal or external to authority discourse').

omega_variable(
    survival_legitimacy_stability,
    'Can theological legitimacy be grounded in institutional survival necessity without collapsing the distinction between revelation and political accommodation?',
    'Comparative analysis of other religious traditions facing state persecution and their doctrinal adaptations; longitudinal tracking of member retention and schism rates.',
    'If survival-necessity legitimacy collapses into pure pragmatism, the constraint degrades toward piton or snare; if it stabilizes as a coherent theological category, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_legitimacy_stability, conceptual, 'Whether survival-based legitimacy is theologically stable or collapses into pure pragmatism').

omega_variable(
    m_set_legitimacy_crisis,
    'Does acknowledging federal coercion as a valid input for doctrinal change irrevocably destabilize the authority structure''s revelatory claims, or can the M-set gap be closed without mandatrophy?',
    'Longitudinal analysis of institutional authority markers (prophetic credibility attribution, member retention, schism rates) in traditions with acknowledged politically-coerced doctrinal shifts.',
    'If destabilizing, the constraint is a piton degrading toward institutional collapse; if stable, the reading successfully renegotiated legitimacy without dissolving the commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_legitimacy_crisis, conceptual, 'Whether acknowledged coercion destroys revelatory legitimacy or permits stable renegotiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_marriage_cvr_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(divine_marriage_cvr_tr_t20, divine_marriage_command__coercion_visibility_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(divine_marriage_cvr_tr_t40, divine_marriage_command__coercion_visibility_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(divine_marriage_cvr_tr_t70, divine_marriage_command__coercion_visibility_reading, theater_ratio, 70, 0.55).
narrative_ontology:measurement(divine_marriage_cvr_tr_t100, divine_marriage_command__coercion_visibility_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(divine_marriage_cvr_tr_t130, divine_marriage_command__coercion_visibility_reading, theater_ratio, 130, 0.45).

% Extraction over time
narrative_ontology:measurement(divine_marriage_cvr_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divine_marriage_cvr_be_t20, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(divine_marriage_cvr_be_t40, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(divine_marriage_cvr_be_t70, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement(divine_marriage_cvr_be_t100, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement(divine_marriage_cvr_be_t130, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 130, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(divine_marriage_cvr_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(divine_marriage_cvr_su_t20, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(divine_marriage_cvr_su_t40, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(divine_marriage_cvr_su_t70, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 70, 0.75).
narrative_ontology:measurement(divine_marriage_cvr_su_t100, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(divine_marriage_cvr_su_t130, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 130, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, substitutionist_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel decomposes into three structurally distinct constraints because the label conflates competing claims about the Manifesto's theological status (coerced survival, prudential suspension, or superseding revelation). Each reading has a different epsilon, beneficiary structure, and directionality profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
