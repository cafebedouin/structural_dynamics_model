% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant (Temporal Accommodation Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'temporal accommodation' reading of the
 *   eternal marriage covenant, particularly concerning plural marriage. It
 *   posits that the practice was suspended due to federal pressure (the
 *   Manifesto of 1890), but the underlying doctrine remains eternally valid
 *   and could be restored in a future dispensation. This reading allows the
 *   church to comply with the law of the land while preserving its
 *   theological heritage. The claimed type is 'tangled_rope' because it
 *   coordinates legal compliance with doctrinal preservation, but extracts
 *   from those who believe in the immediate practice of the doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.45).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.6).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant (Temporal Accommodation Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '0aa2c4d3-5963-4900-93ca-ecbf4021a28a').
narrative_ontology:cs_kernel_codification('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', fixed_text).
narrative_ontology:cs_authority_grounding('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', lineage).
narrative_ontology:cs_interpretation_layer_present('0aa2c4d3-5963-4900-93ca-ecbf4021a28a').
narrative_ontology:cs_reading_relation('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', eternal_marriage_covenant__immutable_commandment_reading, influences).
narrative_ontology:cs_reading_relation('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', foundational, obedience_to_law_of_land_is_divine_command).
narrative_ontology:cs_axiom_status(obedience_to_law_of_land_is_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', obedience_to_law_of_land_is_divine_command, theological).
narrative_ontology:cs_axiom('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', foundational, eternal_principles_can_be_temporarily_suspended).
narrative_ontology:cs_axiom_status(eternal_principles_can_be_temporarily_suspended, holdable).
narrative_ontology:cs_axiom_grounding('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', eternal_principles_can_be_temporarily_suspended, theological).
narrative_ontology:cs_reference_frame('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', divine_law_accommodated_to_secular_law).
narrative_ontology:cs_drift_state('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0aa2c4d3-5963-4900-93ca-ecbf4021a28a', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, polygamist_factions).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, disaffected_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrine and its suspension, balancing divine command with legal compliance. Benefits from maintaining institutional legitimacy and avoiding federal intervention, while preserving the doctrine's eternal validity for future restoration.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the church's legal standing and social acceptance, which the accommodation enables. They are not required to practice polygamy and find the current arrangement socially palatable, while still believing in the doctrine's eternal truth.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_members, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of the suspension, as their core practice is forbidden by the church and the law. They are often excommunicated or marginalized, but remain identity-locked to the original doctrine, believing the suspension is temporary or illegitimate.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, polygamist_factions, payer,
    powerless, generational, identity_locked, local).

% Struggle with the cognitive dissonance of a suspended eternal doctrine. They may feel the church is compromising divine truth for worldly acceptance, leading to spiritual distress or eventual departure. They pay in loss of faith or community.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, disaffected_members, payer,
    moderate, biographical, constrained, national).

% Exerted pressure that led to the Manifesto, enforcing anti-polygamy laws. Benefits from the church's compliance, maintaining the rule of law and social order. Its role is to ensure the church adheres to secular legal norms.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's adherence to federal law while preserving the theological integrity of an eternal doctrine, allowing the institution to survive and grow within the legal framework of the United States.
% TRANSFER_FUNCTION: Transfers the practice of plural marriage from active observance to a suspended, dormant status, from polygamous practitioners to the church's institutional legitimacy and legal standing.
% ABSENT_VOICES: Early church leaders who established the doctrine of plural marriage as essential for exaltation would object to its suspension, viewing it as a compromise of divine command. Their voices are present in historical texts but absent from contemporary authoritative interpretation.
% DISAPPEARANCE_RATIONALE: If the temporal accommodation reading vanished, the church would either be forced to fully renounce the doctrine (rearranging its theology) or resume the practice (rearranging its legal status and relationship with the federal government). Neither outcome would leave the world unchanged.
% FOUNDING_PROBLEM: The conflict between the divinely revealed doctrine of plural marriage and the anti-polygamy laws of the United States, threatening the church's existence and property.
% FOUNDING_PROBLEM_CORROBORATION: Church leadership attests the problem is live, as the doctrine remains eternal and the legal context could theoretically shift. Polygamist factions also attest it is live, as they continue to practice it. The federal government's historical actions corroborate the severity of the original conflict.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, as the church as an institution benefits from legal compliance, but a significant portion of its members (polygamist factions, disaffected members) bear the cost of doctrinal suspension. Suppression (0.6) is high, as the church actively enforces the suspension through excommunication and social pressure, alongside federal legal enforcement. Theater ratio (0.2) is low, as the suspension is a genuine, enforced change in practice, not merely performance, though the 'eternal validity' aspect has a performative dimension for future potential. Accessibility collapse is high (0.7) because for mainstream members, the alternative of practicing polygamy is largely foreclosed by church policy and social norms. Resistance (0.3) is moderate, primarily from marginalized polygamous groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, this is a necessary and divinely guided accommodation, a 'rope' that saves the institution. From the perspective of polygamous factions, it is a 'snare' that denies them a core religious practice. The engine's classification as 'tangled_rope' reflects this hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and mainstream members are beneficiaries, gaining legal and social legitimacy. Polygamist factions and disaffected members are payers, bearing the cost of suppressed practice or cognitive dissonance. The federal government acts as an agenda-setter, enforcing the legal framework that necessitates the accommodation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the accommodation as pure extraction by recognizing the genuine coordination problem it solved (institutional survival). However, it also highlights the ongoing extraction from those whose religious identity is tied to the suspended practice, preventing it from being seen as a 'rope' for all parties. The 'live' status of the founding problem, despite the accommodation, suggests the tension is ongoing, not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_restoration_likelihood,
    'What is the actual likelihood of the ''eternal principle'' of plural marriage being restored to practice, and how does this affect the current reading''s legitimacy?',
    'Future prophetic pronouncements or shifts in legal/social landscape. Analysis of historical patterns of doctrinal re-emphasis.',
    'If restoration is deemed highly unlikely, the ''temporal accommodation'' reading might drift towards a de facto renunciation, reducing its ''theater'' component and potentially reclassifying it. If restoration becomes plausible, the current suppression might be seen as more temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_restoration_likelihood, empirical, 'Uncertainty about the future status of the suspended doctrine.').

omega_variable(
    doctrinal_integrity_vs_institutional_survival,
    'To what extent does the ''temporal accommodation'' compromise the core doctrinal integrity of the eternal marriage covenant versus ensuring the institutional survival of the church?',
    'Theological analysis by independent scholars, internal church debates, and the long-term impact on member faith and retention.',
    'If the compromise is seen as severe, the extractiveness from disaffected members might be higher, and the coordination function for the institution might be viewed as more extractive. If seen as a necessary and divinely sanctioned adaptation, extractiveness might be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_institutional_survival, conceptual, 'Ambiguity regarding the balance between doctrinal fidelity and institutional pragmatism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(eter_tr_t1980, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(eter_tr_t2024, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(eter_be_t1980, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(eter_be_t2024, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(eter_su_t1980, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(eter_su_t2024, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. This 'temporal accommodation' reading directly influences the 'immutable commandment' reading by suspending its practice, and coexists with the 'prophetic override' reading as an alternative interpretive framework for doctrinal change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
