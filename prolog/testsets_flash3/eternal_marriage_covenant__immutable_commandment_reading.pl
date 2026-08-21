% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant (Immutable Commandment Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'immutable commandment' reading of
 *   the eternal marriage covenant, specifically D&C 132, which establishes
 *   polygamy as an eternal, immutable divine law required for exaltation.
 *   This reading holds that the commandment cannot be legitimately revised or
 *   superseded, even in the face of external pressure. Federal anti-polygamy
 *   laws (e.g., the Edmunds-Tucker Act) created a direct conflict, forcing
 *   adherents to choose between religious obedience and legal compliance,
 *   often leading to martyrdom or severe legal penalties. This reading sees
 *   no legitimate revision path, only a test of faith.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.9).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.95).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'bd7ac461-9406-4dfa-9aec-e34f6675e66a').
narrative_ontology:cs_kernel_codification('bd7ac461-9406-4dfa-9aec-e34f6675e66a', fixed_text).
narrative_ontology:cs_authority_grounding('bd7ac461-9406-4dfa-9aec-e34f6675e66a', lineage).
narrative_ontology:cs_interpretation_layer_present('bd7ac461-9406-4dfa-9aec-e34f6675e66a').
narrative_ontology:cs_reading_relation('bd7ac461-9406-4dfa-9aec-e34f6675e66a', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('bd7ac461-9406-4dfa-9aec-e34f6675e66a', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('bd7ac461-9406-4dfa-9aec-e34f6675e66a', foundational, divine_commandment_is_immutable).
narrative_ontology:cs_axiom_status(divine_commandment_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('bd7ac461-9406-4dfa-9aec-e34f6675e66a', divine_commandment_is_immutable, theological).
narrative_ontology:cs_axiom('bd7ac461-9406-4dfa-9aec-e34f6675e66a', foundational, polygamy_required_for_exaltation).
narrative_ontology:cs_axiom_status(polygamy_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('bd7ac461-9406-4dfa-9aec-e34f6675e66a', polygamy_required_for_exaltation, theological).
narrative_ontology:cs_reference_frame('bd7ac461-9406-4dfa-9aec-e34f6675e66a', original_divine_mandate).
narrative_ontology:cs_drift_state('bd7ac461-9406-4dfa-9aec-e34f6675e66a', federal_anti_polygamy_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('bd7ac461-9406-4dfa-9aec-e34f6675e66a', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_authority_structure).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamous_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_in_polygamous_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, members_facing_federal_prosecution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces D&C 132 as an eternal, immutable divine law. Benefits from the hierarchical structure and control over family units that polygamy enables. Views any deviation as apostasy and a threat to salvation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_authority_structure, agenda_setter,
    institutional, generational, identity_locked, global).

% Bound by religious conviction and social pressure to accept polygamous marriages. Often face economic dependence, social isolation, and legal vulnerability. Exit means spiritual condemnation and loss of community.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_wives, payer,
    powerless, biographical, identity_locked, local).

% Born into the system, with no agency over their family structure. Subject to the social and legal consequences of polygamy, including potential stigma and legal uncertainty regarding parentage or inheritance. Exit is extremely difficult due to age and dependence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_in_polygamous_families, payer,
    powerless, biographical, trapped, local).

% Adherents who continue to practice polygamy in defiance of federal law, facing arrest, imprisonment, and loss of property. Their commitment to the immutable commandment leads to direct legal and personal costs.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, members_facing_federal_prosecution, payer,
    powerless, immediate, trapped, national).

% Enforces anti-polygamy laws, viewing it as a criminal act and a violation of social norms. Its actions create a direct conflict for adherents of the immutable commandment reading, forcing a choice between religious obedience and legal compliance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who have left polygamous communities, often facing ostracization, loss of family ties, and psychological trauma. Their voices are actively suppressed within the communities that uphold the immutable commandment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, apostates_from_polygamous_groups, excluded,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family structure and social hierarchy within a religious community, ensuring adherence to a specific interpretation of divine law and maintaining a patriarchal order.
% TRANSFER_FUNCTION: Transfers authority, resources, and labor from wives and children to the male head of the household and the broader patriarchal authority structure, in exchange for spiritual salvation and community belonging.
% ABSENT_VOICES: Apostates from polygamous groups, former wives, and children who have left the system are actively silenced or discredited within the communities that adhere to this reading. They would speak of coercion, abuse, and the suppression of individual autonomy.
% DISAPPEARANCE_RATIONALE: If the immutable commandment reading of D&C 132 vanished overnight, the entire social, legal, and spiritual structure of fundamentalist polygamous communities would collapse. Family units would dissolve, patriarchal authority would be challenged, and individuals would seek legal and social redress, leading to a profound reorganization of these societies.
% FOUNDING_PROBLEM: The problem of ensuring eternal salvation and the continuation of family lines in the afterlife, as understood through specific scriptural interpretations requiring plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: Adherents within fundamentalist polygamous groups attest that the problem of eternal salvation and exaltation, as defined by D&C 132, remains live and central to their faith. External observers, including sociologists studying these communities, corroborate that this belief system is actively maintained and shapes their social structures, even if they dispute the premise itself.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the constraint demands absolute obedience to a practice that imposes severe social, economic, and legal costs on its adherents, particularly women and children, with no reciprocal material benefit from the constraint itself. Suppression is also extremely high (0.95) due to the combination of intense religious indoctrination, social ostracization for non-compliance, and the direct legal penalties imposed by the federal government. The theater ratio is very low (0.05) because the commitment to polygamy under this reading is genuine and actively practiced, not merely performative. Resistance is high (0.8) due to the direct conflict with federal law, leading to active defiance and legal battles by adherents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the patriarchal authority structure, this is a divine law that, while challenging, is essential for eternal salvation and the maintenance of a righteous order. From the perspective of polygamous wives and children, it is a system of profound extraction and suppression, enforced by both religious authority and social pressure, with severe consequences for non-compliance. The federal government's perspective is one of legal enforcement against a criminal practice.
 *
 * DIRECTIONALITY LOGIC:
 *   The patriarchal authority structure is the primary beneficiary (d near 0.0) as it gains control and legitimacy from enforcing this divine law. Polygamous wives, children, and members facing federal prosecution are clear targets (d near 1.0), bearing the direct costs and legal risks. The federal government acts as an external enforcer, creating a martyrdom constraint for the adherents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a Snare because its persistence relies heavily on coercion (religious, social, and legal) and the suppression of alternatives, with clear victims. The coordination story (eternal salvation, righteous order) serves as a cover for the severe extraction and control exerted by the patriarchal authority. The high resistance and direct conflict with external law further indicate its extractive nature, rather than a genuine coordination mechanism. Mandatrophy is not resolved; the founding problem (eternal salvation via polygamy) is still considered 'live' by adherents, but the costs and external pressures have intensified the extractive nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_human_interpretation,
    'Is D&C 132 an immutable divine commandment, or a historically contingent interpretation of religious texts that can be superseded by prophetic revelation or societal change?',
    'Theological re-evaluation within the religious tradition, or a shift in the authoritative interpretive framework that allows for re-contextualization or abrogation of the commandment.',
    'If re-interpreted as mutable, the constraint''s extractiveness and suppression would significantly decrease, potentially reclassifying it as a Piton or even dissolving it. If confirmed as immutable, the Snare classification would be reinforced, highlighting the inherent conflict with external legal systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_human_interpretation, conceptual, 'Ambiguity regarding the immutability of the divine commandment.').

omega_variable(
    martyrdom_vs_coercion,
    'To what extent is adherence to polygamy under federal pressure a genuine act of martyrdom, versus a coerced compliance driven by identity-lock and lack of viable exit options?',
    'Longitudinal studies of individuals who successfully exit polygamous communities, assessing the psychological and social costs, and the availability of support structures. Analysis of the ''choice'' presented to adherents under duress.',
    'If primarily martyrdom, the high resistance and suppression reflect a genuine conflict of values. If primarily coercion, the extractiveness is amplified by the lack of true agency, and the ''resistance'' is a symptom of entrapment rather than empowered defiance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martyrdom_vs_coercion, empirical, 'Distinguishing genuine martyrdom from coerced compliance under duress.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (federal laws, economic dependence) or internalized (religious indoctrination, identity fusion)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals leaving the community still experience severe psychological barriers to autonomy), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true freedom more elusive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in polygamous communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1852, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.1).
narrative_ontology:measurement(eter_tr_t1865, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(eter_tr_t1878, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1878, 0.06).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.05).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.7).
narrative_ontology:measurement(eter_be_t1865, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1865, 0.78).
narrative_ontology:measurement(eter_be_t1878, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1878, 0.85).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.75).
narrative_ontology:measurement(eter_su_t1865, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1865, 0.82).
narrative_ontology:measurement(eter_su_t1878, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1878, 0.9).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, federal_anti_polygamy_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eternal_marriage_covenant' kernel. Sibling readings include 'prophetic_override_reading' and 'temporal_accommodation_reading', which offer alternative interpretations of D&C 132's applicability and revisability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
