% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy by Inherited Right
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the 'monarchical reading' of sovereign
 *   legitimacy, where authority is inherited and divinely sanctioned. It is a
 *   Snare because it extracts heavily from subjects while suppressing
 *   alternative claims to legitimacy. The system relies on active enforcement
 *   to maintain its structure, despite often presenting itself as a natural
 *   or divinely ordained order. The high theater ratio reflects the elaborate
 *   rituals and ceremonies used to reinforce the legitimacy of inherited
 *   rule, which often mask the underlying coercive mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.85).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.9).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, snare).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy by Inherited Right").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'a88d165c-edf3-43d3-b7d1-75227e502c61').
narrative_ontology:cs_kernel_codification('a88d165c-edf3-43d3-b7d1-75227e502c61', implicit).
narrative_ontology:cs_authority_grounding('a88d165c-edf3-43d3-b7d1-75227e502c61', lineage).
narrative_ontology:cs_interpretation_layer_present('a88d165c-edf3-43d3-b7d1-75227e502c61').
narrative_ontology:cs_reading_relation('a88d165c-edf3-43d3-b7d1-75227e502c61', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('a88d165c-edf3-43d3-b7d1-75227e502c61', sovereign_legitimacy__constitutional_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('a88d165c-edf3-43d3-b7d1-75227e502c61', foundational, authority_descends_from_divine_right).
narrative_ontology:cs_axiom_status(authority_descends_from_divine_right, holdable).
narrative_ontology:cs_axiom_grounding('a88d165c-edf3-43d3-b7d1-75227e502c61', authority_descends_from_divine_right, theological).
narrative_ontology:cs_axiom('a88d165c-edf3-43d3-b7d1-75227e502c61', foundational, legitimacy_inheres_in_bloodline_continuity).
narrative_ontology:cs_axiom_status(legitimacy_inheres_in_bloodline_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a88d165c-edf3-43d3-b7d1-75227e502c61', legitimacy_inheres_in_bloodline_continuity, conventional).
narrative_ontology:cs_reference_frame('a88d165c-edf3-43d3-b7d1-75227e502c61', absolute_monarchy_divine_right).
narrative_ontology:cs_drift_state('a88d165c-edf3-43d3-b7d1-75227e502c61', contemporary_global_political_order, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a88d165c-edf3-43d3-b7d1-75227e502c61', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, alternative_legitimacy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority by birthright, administers the state, and benefits directly from the system's stability and the extraction of resources from subjects. Their identity is fused with the system's perpetuation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, identity_locked, national).

% Receives privileges, land, and positions of influence by virtue of their proximity and loyalty to the sovereign. They are invested in maintaining the system but are ultimately subordinate to the ruling class.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Are governed without consent, bear the costs of state administration through taxes and labor, and have no formal means to participate in or alter the authority structure. Their options are obedience or rebellion.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, subjects, payer,
    powerless, biographical, trapped, national).

% Advocate for different sources of authority (e.g., popular sovereignty, meritocracy) but are actively suppressed by the monarchical system. Their claims are delegitimized and often met with coercion.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, alternative_legitimacy_claimants, excluded,
    moderate, generational, constrained, national).

% Provide divine sanction for the monarch's rule, reinforcing the legitimacy narrative. In return, they receive state support, protection, and a privileged position in society. Their authority is intertwined with the monarch's.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, religious_institutions, beneficiary,
    institutional, civilizational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, stable line of succession and a single, unambiguous source of ultimate authority, preventing internal power struggles and civil war by establishing a fixed hierarchy.
% TRANSFER_FUNCTION: Transfers political power, economic resources, and social status from the general populace to the hereditary ruling class and associated aristocracy, in exchange for perceived order and stability.
% ABSENT_VOICES: Advocates for popular sovereignty, democratic representation, and merit-based leadership are systematically excluded and suppressed. They would argue that authority must derive from the consent of the governed, not inherited status.
% DISAPPEARANCE_RATIONALE: If the principle of monarchical legitimacy vanished, the entire political and social order would collapse. The hereditary ruling class would lose its claim to power, the aristocratic hierarchy would be delegitimized, and a vacuum of authority would emerge, leading to widespread political reorganization or chaos.
% FOUNDING_PROBLEM: The problem of establishing a stable, unquestionable source of authority to prevent constant internal conflict and ensure continuity of governance, particularly in pre-modern states.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside the monarchical system acknowledge that hereditary succession provided a mechanism for stability in certain historical contexts. However, they also corroborate that the 'problem' of instability is often manufactured or exaggerated to justify the extraction inherent in the system, and that the problem is 'dead' in modern contexts where alternative, more equitable forms of stable governance exist.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the transfer of wealth and power to the ruling class without popular consent. Suppression (0.90) is critical, as any challenge to the divine right or bloodline continuity is met with severe penalties. The theater ratio (0.60) indicates that a significant portion of the system's activity is performative, designed to awe and legitimize, rather than to genuinely coordinate or serve the populace. Accessibility collapse is high (0.75) because alternatives are not merely suppressed but often rendered unthinkable within the dominant ideological framework. Resistance is moderate (0.40) but often localized and met with overwhelming force.
 *
 * PERSPECTIVAL GAP:
 *   The hereditary ruling class perceives this as a legitimate, stable, and divinely sanctioned order, a 'Mountain' of natural law. Subjects, however, experience it as a 'Snare' that extracts resources and suppresses their agency. The engine's classification will highlight this divergence, showing a Snare from the subjects' seat and potentially a Tangled Rope or even a False Summit Mountain from the ruling class's seat, depending on the balance of perceived coordination benefits (e.g., stability) versus extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class and aristocratic hierarchy are clear beneficiaries (d near 0.0) as they directly profit from the system. Subjects and alternative legitimacy claimants are targets (d near 1.0) as they bear the costs and are actively suppressed. Religious institutions are beneficiaries (d near 0.0) due to their role in sanctioning the monarch's rule. The system is designed to subsidize the ruling elite at the expense of the governed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing stability through a clear line of authority) has largely atrophied in modern contexts where democratic alternatives exist. However, the constraint persists due to the concentrated benefits for the ruling class and the active suppression of alternatives. The high extractiveness and suppression prevent it from degrading into a Piton; it remains a Snare because identifiable parties actively benefit from its coercive maintenance. Resolving mandatrophy would require dismantling the hereditary power structure and establishing a new basis for legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is the divine sanction claimed by the monarchical reading an empirically verifiable fact or a socially constructed narrative?',
    'Theological and historical analysis of the origins and persistence of the ''divine right'' claim, examining its instrumental role in consolidating power versus its independent theological grounding.',
    'If purely constructed, the ''divine sanction'' acts as a powerful form of ideological suppression, increasing the effective suppression metric and reinforcing the Snare classification. If genuinely believed by a significant portion of the populace, it might slightly reduce the perceived suppression for those adherents, but not alter the structural extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'Ambiguity of divine sanction as a source of legitimacy.').

omega_variable(
    stability_vs_extraction_tradeoff,
    'Does the monarchical system genuinely provide greater stability than alternative forms of governance, or is the claim of stability primarily a justification for extraction?',
    'Comparative historical analysis of states with monarchical versus republican systems, evaluating metrics of internal conflict, economic development, and social welfare over long periods. This would also involve analyzing the frequency and severity of succession crises within monarchical systems.',
    'If stability is demonstrably superior, it might suggest a genuine, albeit highly extractive, coordination function, pushing the classification closer to a Tangled Rope. If stability is not superior, or is achieved at disproportionate cost, it reinforces the Snare classification by exposing the coordination claim as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_extraction_tradeoff, empirical, 'Trade-off between claimed stability and actual extraction.').

omega_variable(
    suppression_internalized_vs_structural,
    'To what extent is the suppression of alternative legitimacy claims internalized by subjects (e.g., through education, cultural norms) versus structurally enforced (e.g., through laws, military force)?',
    'Sociological studies of public opinion and historical analysis of dissent and rebellion. If suppression persists after the removal of overt coercive mechanisms, it indicates a higher degree of internalization.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as subjects carry the suppression with them. This makes the constraint more resilient and the Snare more deeply entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism for alternative legitimacy claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 1000, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1000, sovereign_legitimacy__monarchical_reading, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(sove_tr_t1200, sovereign_legitimacy__monarchical_reading, theater_ratio, 1200, 0.45).
narrative_ontology:measurement(sove_tr_t1400, sovereign_legitimacy__monarchical_reading, theater_ratio, 1400, 0.5).
narrative_ontology:measurement(sove_tr_t1600, sovereign_legitimacy__monarchical_reading, theater_ratio, 1600, 0.55).
narrative_ontology:measurement(sove_tr_t1800, sovereign_legitimacy__monarchical_reading, theater_ratio, 1800, 0.6).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__monarchical_reading, theater_ratio, 2000, 0.6).

% Extraction over time
narrative_ontology:measurement(sove_be_t1000, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement(sove_be_t1200, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1200, 0.8).
narrative_ontology:measurement(sove_be_t1400, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1400, 0.85).
narrative_ontology:measurement(sove_be_t1600, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1600, 0.88).
narrative_ontology:measurement(sove_be_t1800, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1800, 0.87).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__monarchical_reading, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1000, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1000, 0.8).
narrative_ontology:measurement(sove_su_t1200, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1200, 0.85).
narrative_ontology:measurement(sove_su_t1400, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1400, 0.9).
narrative_ontology:measurement(sove_su_t1600, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1600, 0.92).
narrative_ontology:measurement(sove_su_t1800, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1800, 0.91).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__monarchical_reading, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
