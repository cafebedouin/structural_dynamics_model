% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Eternal Marriage Covenant (Prophetic Override Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story analyzes the 'prophetic override' reading of the
 *   eternal marriage covenant kernel. This reading asserts that the doctrine
 *   of continuing revelation allows a living prophet to supersede prior
 *   divine commandments when circumstances (often external pressures like
 *   federal law) require. It functions as a mechanism for institutional
 *   adaptation and survival, but at the cost of extracting compliance and
 *   belief from members who may hold to the immutability of prior
 *   revelations. The claimed type is Tangled Rope because it coordinates the
 *   community around current prophetic guidance while extracting compliance
 *   from those whose prior beliefs are superseded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.75).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Eternal Marriage Covenant (Prophetic Override Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, 'a22aea62-034a-467f-af09-43f7bf0961f6').
narrative_ontology:cs_kernel_codification('a22aea62-034a-467f-af09-43f7bf0961f6', formalized).
narrative_ontology:cs_authority_grounding('a22aea62-034a-467f-af09-43f7bf0961f6', lineage).
narrative_ontology:cs_interpretation_layer_present('a22aea62-034a-467f-af09-43f7bf0961f6').
narrative_ontology:cs_reading_relation('a22aea62-034a-467f-af09-43f7bf0961f6', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('a22aea62-034a-467f-af09-43f7bf0961f6', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('a22aea62-034a-467f-af09-43f7bf0961f6', foundational, living_prophet_authority_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_authority_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('a22aea62-034a-467f-af09-43f7bf0961f6', living_prophet_authority_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('a22aea62-034a-467f-af09-43f7bf0961f6', secondary, divine_will_is_dynamic_and_contextual).
narrative_ontology:cs_axiom_status(divine_will_is_dynamic_and_contextual, holdable).
narrative_ontology:cs_axiom_grounding('a22aea62-034a-467f-af09-43f7bf0961f6', divine_will_is_dynamic_and_contextual, theological).
narrative_ontology:cs_reference_frame('a22aea62-034a-467f-af09-43f7bf0961f6', dynamic_divine_guidance).
narrative_ontology:cs_drift_state('a22aea62-034a-467f-af09-43f7bf0961f6', post_polygamy_manifestos, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a22aea62-034a-467f-af09-43f7bf0961f6', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, active_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, members_clinging_to_prior_revelation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authority to receive new revelation, interpret doctrine, and supersede prior commandments when circumstances require, ensuring the church's adaptation and survival. Benefits from the flexibility this doctrine provides.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, living_prophet_first_presidency, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the stability and unity provided by a clear mechanism for doctrinal adaptation. Must uphold and teach the current prophetic guidance, even if it supersedes prior teachings.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_leadership, beneficiary,
    institutional, biographical, constrained, global).

% Benefit from clear, current divine guidance and the church's continued existence. Bear the cost of adapting their beliefs and practices to new revelations, which can be emotionally and intellectually challenging.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, active_members, beneficiary,
    moderate, biographical, constrained, global).

% Bear the cost of having deeply held beliefs (e.g., in the immutability of specific commandments) superseded. May experience spiritual distress, social pressure, or feel their eternal salvation is jeopardized by changes they cannot accept. Exit means abandoning their community and identity.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, members_clinging_to_prior_revelation, payer,
    powerless, generational, identity_locked, local).

% Actively resist or question the authority of new revelations, often leading to ecclesiastical discipline or excommunication. Their resistance highlights the coercive aspect of the constraint.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, dissenters, payer,
    moderate, biographical, constrained, local).

% Exert external pressure (e.g., legal, social) that can influence the timing and content of new revelations. Their actions can activate the prophetic override mechanism, making them an indirect agenda-setter for doctrinal shifts.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government_secular_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, federal_government_secular_authorities, agenda_setter).

% Left the church due to doctrinal changes or the perceived erosion of prior commandments. They are outside the system but represent the historical cost of the prophetic override doctrine.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, former_members_splinter_groups, excluded,
    powerless, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological mechanism for the church to adapt its doctrine and practices to changing external circumstances (e.g., legal, social, political), thereby maintaining institutional unity, relevance, and survival.
% TRANSFER_FUNCTION: Transfers the ultimate authority to define and interpret divine will from fixed, prior revelations to the living prophet, and transfers the burden of adapting beliefs and practices to the general membership.
% ABSENT_VOICES: Those who believe in the absolute, unchanging nature of specific prior revelations, particularly those who left the church over such changes (e.g., polygamy). They would argue that divine law cannot be superseded by human or temporal concerns.
% DISAPPEARANCE_RATIONALE: If the doctrine of continuing revelation and prophetic override vanished, the church would lose its primary mechanism for adapting to external pressures. It would either fracture into numerous factions clinging to different interpretations of immutable past revelations, or face severe legal and social challenges that could lead to its decline or dissolution.
% FOUNDING_PROBLEM: How to reconcile divine commandments, some of which were in tension with secular law or evolving societal norms (e.g., polygamy), with the practical necessity for the church to survive, grow, and operate within a larger society.
% FOUNDING_PROBLEM_CORROBORATION: Church historians, sociologists of religion, and legal scholars document the historical instances (e.g., the Manifestos on polygamy) where this doctrine was invoked to navigate severe external pressures. Independent academic analyses corroborate the role of external constraints in activating this mechanism, even if the church frames it as purely divine will.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because members are compelled to abandon or reinterpret deeply held beliefs and practices that were previously presented as eternal commandments. Suppression is also high, as dissent or refusal to accept new revelations can lead to severe social and ecclesiastical consequences, including excommunication. Theater ratio is moderate, reflecting that while the process of receiving revelation is genuinely believed, the timing and content of such revelations can be influenced by external, pragmatic concerns, leading to a degree of performative justification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, this doctrine is a divinely ordained mechanism for dynamic guidance and institutional preservation. From the perspective of members whose prior beliefs are superseded, it can feel like an arbitrary imposition or a betrayal of eternal principles, leading to significant personal cost and extraction. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The living prophet and church leadership are beneficiaries, gaining flexibility and institutional survival. Active members are also beneficiaries, receiving clear guidance, but bear costs in adapting their beliefs. Members clinging to prior revelation and dissenters are clear targets, experiencing extraction as their foundational beliefs are challenged or superseded. Federal authorities act as an external force, indirectly shaping the agenda.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_external_pressure,
    'To what extent is a new revelation under this doctrine a direct expression of divine will, versus a pragmatic response to external (e.g., federal) pressure for institutional survival?',
    'Analysis of historical records, prophetic discourses, and external political/legal contexts surrounding specific revelations. Comparison of internal theological justifications with external pressures at the time.',
    'If primarily a pragmatic response, the ''divine will'' framing becomes more theatrical, increasing the constraint''s theater_ratio and potentially shifting its classification towards a Snare or a more extractive Tangled Rope, as the coordination story becomes cover for institutional self-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_external_pressure, empirical, 'Ambiguity between divine inspiration and institutional pragmatism in new revelations.').

omega_variable(
    scope_of_supersedence_limits,
    'What are the theological and practical limits to the prophetic override? Can any prior doctrine be superseded, or only specific practices or interpretations?',
    'Analysis of historical precedents, theological treatises, and future revelations. Examination of whether core theological tenets (e.g., nature of God, atonement) are ever subject to override, versus social/familial practices.',
    'If the override is unlimited, it increases the perceived instability and potential for extraction from members, making the constraint more Snare-like. If clear limits exist, it reduces the perceived arbitrariness and strengthens the coordination function, moving it closer to a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_supersedence_limits, conceptual, 'Uncertainty regarding the boundaries of prophetic authority to supersede prior revelation.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the compliance with new revelations is due to genuine belief and internalized commitment, versus fear of social ostracism or ecclesiastical discipline?',
    'Sociological studies of member attitudes, surveys on belief vs. compliance, and analysis of post-excommunication trajectories. If compliance persists after formal enforcement is removed, it suggests internalization.',
    'If suppression is largely internalized, the effective suppression is higher than structural measures suggest, as members carry the constraint with them. If primarily structural, removing formal enforcement would lead to greater dissent and potential schism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for doctrinal compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(eter_tr_t1980, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1980, 0.23).
narrative_ontology:measurement(eter_tr_t2000, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(eter_tr_t2024, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(eter_be_t1980, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(eter_be_t2000, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(eter_be_t2024, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(eter_su_t1980, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1980, 0.73).
narrative_ontology:measurement(eter_su_t2000, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(eter_su_t2024, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
