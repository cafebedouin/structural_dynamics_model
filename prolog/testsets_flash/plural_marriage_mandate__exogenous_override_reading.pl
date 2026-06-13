% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Plural Marriage Mandate (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto as a direct result of
 *   exogenous federal coercion, forcing the abandonment of a divinely
 *   commanded practice (plural marriage) rather than a legitimate internal
 *   doctrinal reinterpretation. The federal government, acting as the
 *   agenda-setter, used severe legal and economic pressure to compel
 *   conformity, creating a snare for the church leadership and practicing
 *   polygamists. The high extractiveness and suppression reflect the cost
 *   borne by the victims and the coercive nature of the federal intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.85).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Plural Marriage Mandate (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, 'bb531cc7-21ff-4ac4-83cf-2ba0eca426a6').
narrative_ontology:cs_kernel_codification('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', formalized).
narrative_ontology:cs_authority_grounding('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', extraction).
narrative_ontology:cs_interpretation_layer_present('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6').
narrative_ontology:cs_reading_relation('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', foundational, divine_command_is_immutable).
narrative_ontology:cs_axiom_status(divine_command_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', divine_command_is_immutable, theological).
narrative_ontology:cs_axiom('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', foundational, state_coercion_invalidates_religious_change).
narrative_ontology:cs_axiom_status(state_coercion_invalidates_religious_change, holdable).
narrative_ontology:cs_axiom_grounding('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', state_coercion_invalidates_religious_change, deontological).
narrative_ontology:cs_reference_frame('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', divine_command_immutable_by_state).
narrative_ontology:cs_drift_state('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', post_1890_manifesto, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bb531cc7-21ff-4ac4-83cf-2ba0eca426a6', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, united_states_federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, mainstream_american_society).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_leadership_under_duress).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, future_church_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforced anti-polygamy laws through fines, imprisonment, and property confiscation, aiming to compel conformity to national social norms and assert federal authority over territorial practices. Benefited from the perceived 'civilizing' of the territory.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, united_states_federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Faced severe legal penalties, social ostracization, and economic ruin for adhering to what they believed was a divine commandment. Their options were to abandon their families and faith, or endure persecution.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, trapped, local).

% Issued the 1890 Manifesto under extreme federal pressure, including the threat of disincorporation and confiscation of church property. While formally the 'agenda setter' for the church, their action was coerced, making them a victim of the federal constraint.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_leadership_under_duress, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_leadership_under_duress, agenda_setter).

% Benefited from the perceived triumph of 'American values' and the suppression of a practice widely considered immoral and uncivilized, reinforcing their cultural and social norms.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, mainstream_american_society, beneficiary,
    institutional, generational, mobile, national).

% Benefited from the church's survival and eventual integration into mainstream American society, which allowed for its continued growth and missionary work, albeit at the cost of abandoning a core doctrine.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, future_church_members, beneficiary,
    moderate, generational, constrained, global).

% Would argue that the federal government's actions constituted an infringement on religious freedom, even if the practice was unpopular. Their voices were largely marginalized in the dominant discourse of the era.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, religious_freedom_advocates, excluded,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, from the federal perspective, coordinated the social and legal norms of the Utah territory with those of the broader United States, resolving a perceived conflict over marriage practices and federal authority.
% TRANSFER_FUNCTION: Transferred the right to practice plural marriage from the religious community to the federal government's control, effectively extracting a core religious practice in exchange for territorial statehood and the church's institutional survival.
% ABSENT_VOICES: Religious freedom advocates and those who believed in the divine origin of plural marriage were largely silenced or ignored by the dominant federal narrative. Their arguments for religious autonomy were overridden by the state's coercive power.
% DISAPPEARANCE_RATIONALE: If the federal coercion had vanished, the practice of plural marriage would likely have continued within the religious community, and the church's relationship with the U.S. government and mainstream society would have been fundamentally different, potentially leading to a distinct socio-political landscape in the American West.
% FOUNDING_PROBLEM: The federal government perceived plural marriage as a moral affront, a challenge to national sovereignty in the territories, and an impediment to Utah's statehood, creating a conflict between religious practice and secular law.
% FOUNDING_PROBLEM_CORROBORATION: The federal government's historical records and contemporary legal scholars corroborate the problem's existence and its resolution through coercion. The church's own historical accounts, while framing the Manifesto as inspired, also document the intense federal pressure, corroborating the 'dead' status of the original problem as a driver for the current constraint.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because a core religious practice was forcibly abandoned, representing a significant loss for the affected community. Suppression is extremely high (0.92) due to the federal government's systematic campaign of arrests, imprisonment, property confiscation, and threats to the church's institutional existence. The theater ratio is moderate (0.60) because while the Manifesto presented an appearance of voluntary compliance or divine revelation, a substantial portion of its function was to mask the underlying coercion and present a narrative of internal change.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a legitimate exercise of state power to enforce national norms, potentially seen as a 'rope' coordinating social order. From the perspective of practicing polygamists, it was a 'snare' of pure extraction, forcing them to abandon a divine commandment under duress. Church leadership, while issuing the Manifesto, experienced it as a 'tangled rope' – a coerced coordination to save the institution, but at a high cost to its members and doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. federal government and mainstream American society are clear beneficiaries (d=0.0-0.1) as they achieved their policy and social conformity goals. Practicing polygamists are full targets (d=1.0) due to direct persecution and loss of religious freedom. Church leadership, while appearing to set the agenda, was under such duress that their directionality is closer to a target (d=0.7-0.8), as they were forced to act against their prior doctrinal commitments to ensure institutional survival.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies the constraint as a snare, emphasizing the coercive nature of its persistence. It prevents mislabeling the outcome as a 'rope' (legitimate coordination) or 'mountain' (natural evolution of doctrine) by highlighting the active enforcement, identifiable victims, and the suppression of alternatives. The 'dead' status of the founding problem (federal opposition to plural marriage) combined with the 'world_rearranges' disappearance verdict indicates a zombie constraint, where the original coercive mechanism has achieved its goal, but the 'solution' (abandonment of plural marriage) persists, now maintained by internal church doctrine, which itself was initiated under duress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_human_coercion,
    'Was the abandonment of plural marriage a genuine divine reinterpretation, or a direct capitulation to overwhelming federal coercion?',
    'Analysis of primary historical documents, including private communications of church leaders, federal government records, and testimonies of those directly affected, to assess the degree of internal agency versus external pressure.',
    'If primarily divine reinterpretation, the constraint leans towards a ''mountain'' or ''rope'' (endogenous_reinterpretation_reading). If primarily coercion, it remains a ''snare'' (exogenous_override_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_command_vs_human_coercion, empirical, 'Ambiguity between divine will and political force.').

omega_variable(
    institutional_survival_vs_doctrinal_purity,
    'To what extent was the Manifesto a pragmatic decision for institutional survival, distinct from either divine command or pure coercion?',
    'Comparative analysis with other religious institutions facing similar existential threats, examining the role of ''pragmatic'' narratives in legitimizing shifts under pressure.',
    'If institutional pragmatism is the dominant factor, the constraint might be reclassified as a ''tangled_rope'' (institutional_pragmatism_reading), where the coordination function is institutional survival, but extraction is the cost of that survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_survival_vs_doctrinal_purity, conceptual, 'Role of institutional pragmatism in doctrinal change.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (federal laws, imprisonment, property seizure) or internalized (the belief among members that the Manifesto was divine will, leading to self-censorship)?',
    'Post-Manifesto adherence patterns: if adherence to monogamy persisted even after federal enforcement waned, it suggests a degree of internalized suppression. If adherence was primarily maintained by continued external social pressure, it points to structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the community carries the suppression with them. If purely structural, the constraint''s persistence relies solely on external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1894, 0.55).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.8).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1894, 0.83).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1898, 0.85).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1894, 0.92).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1898, 0.92).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, religious_freedom_limits_in_us_law).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'plural_marriage_mandate' kernel, emphasizing federal coercion. Sibling readings explore endogenous reinterpretation and institutional pragmatism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
