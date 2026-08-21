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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant (Immutable Commandment Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'immutable commandment' reading of
 *   the 'eternal_marriage_covenant' kernel. From this perspective, D&C 132
 *   establishes polygamy as an eternal, immutable divine law, absolutely
 *   required for the highest degree of exaltation. Compliance is a
 *   non-negotiable spiritual imperative, and any deviation is seen as
 *   apostasy. Federal pressure against polygamy historically created a
 *   martyrdom constraint, where adherence to divine law meant defying secular
 *   law, but did not alter the perceived immutability of the command itself.
 *   The high extractiveness and suppression reflect the profound spiritual
 *   and social costs of non-compliance, and the identity-locked nature of
 *   adherence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.85).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.9).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '1d54208f-959d-46f6-8d69-43ddfd1b88d4').
narrative_ontology:cs_kernel_codification('1d54208f-959d-46f6-8d69-43ddfd1b88d4', fixed_text).
narrative_ontology:cs_authority_grounding('1d54208f-959d-46f6-8d69-43ddfd1b88d4', lineage).
narrative_ontology:cs_interpretation_layer_present('1d54208f-959d-46f6-8d69-43ddfd1b88d4').
narrative_ontology:cs_reading_relation('1d54208f-959d-46f6-8d69-43ddfd1b88d4', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('1d54208f-959d-46f6-8d69-43ddfd1b88d4', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('1d54208f-959d-46f6-8d69-43ddfd1b88d4', foundational, divine_law_is_immutable).
narrative_ontology:cs_axiom_status(divine_law_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('1d54208f-959d-46f6-8d69-43ddfd1b88d4', divine_law_is_immutable, theological).
narrative_ontology:cs_axiom('1d54208f-959d-46f6-8d69-43ddfd1b88d4', foundational, polygamy_required_for_exaltation).
narrative_ontology:cs_axiom_status(polygamy_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('1d54208f-959d-46f6-8d69-43ddfd1b88d4', polygamy_required_for_exaltation, theological).
narrative_ontology:cs_reference_frame('1d54208f-959d-46f6-8d69-43ddfd1b88d4', eternal_divine_commandment).
narrative_ontology:cs_drift_state('1d54208f-959d-46f6-8d69-43ddfd1b88d4', contemporary_secular_society, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1d54208f-959d-46f6-8d69-43ddfd1b88d4', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, faithful_adherents).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, church_leadership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamous_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, disobedient_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_of_polygamous_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those who believe polygamy is an eternal, immutable divine law required for exaltation. They bear the social and personal costs of compliance but anticipate ultimate spiritual rewards. Their identity is deeply intertwined with this commitment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, faithful_adherents, beneficiary,
    powerful, civilizational, identity_locked, global).

% Administers and interprets the divine law, enforcing compliance through ecclesiastical courts and social pressure. Benefits from the authority and loyalty derived from being the custodians of eternal truth. Faced external pressure from the federal government.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Often entered into polygamous marriages due to religious conviction and social pressure. Bear significant personal, social, and emotional costs, including legal vulnerability and subordinate status within the family. Exit means spiritual and social ostracization.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_wives, payer,
    powerless, biographical, trapped, local).

% Members who struggle with or reject the practice of polygamy but remain within the faith. They face spiritual penalties, social stigma, and the threat of losing eternal blessings if they do not comply with what is presented as divine law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, disobedient_members, payer,
    moderate, biographical, identity_locked, local).

% Born into polygamous families, they often face social stigma, legal ambiguities, and complex family dynamics. Their life choices are heavily influenced by the constraint, and their exit options are limited by their upbringing and social context.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_of_polygamous_families, payer,
    powerless, biographical, constrained, local).

% Historically opposed polygamy, enacting laws and exerting pressure that led to the church's official suspension of the practice. From this reading's perspective, the government acts as an external force creating a martyrdom constraint, not as a legitimate authority on divine law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, federal_government, agenda_setter).

% Individuals who left the faith, often due to their inability to reconcile with or practice polygamy. Their voices are excluded from the internal discourse of the faithful, and their experiences are often dismissed as a loss of faith rather than a critique of the constraint.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, apostates, excluded,
    powerless, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family structure, eternal progression, and social order within a specific theological framework, ensuring adherence to a revealed divine pattern for exaltation.
% TRANSFER_FUNCTION: Transfers absolute obedience, loyalty, and often significant personal sacrifice (especially from women) to the church and family unit, in exchange for promised eternal blessings and the highest degree of salvation.
% ABSENT_VOICES: Former members who left the faith over polygamy, and those within who silently dissent but fear eternal consequences, are excluded. They would argue that the 'divine law' is a human construct or an outdated practice, not an immutable requirement for exaltation.
% DISAPPEARANCE_RATIONALE: If the belief in polygamy as an immutable divine law vanished, the foundational theology of exaltation would collapse, leading to a radical reorganization of family structures, church authority, and the very identity of the faithful. The entire commitment system would need to be re-grounded.
% FOUNDING_PROBLEM: To restore ancient biblical practices and provide the only path to the highest degree of exaltation, as revealed by God to Joseph Smith, ensuring eternal increase and family continuity.
% FOUNDING_PROBLEM_CORROBORATION: Within this reading, the founding problem is considered eternally live, as the divine command is immutable. This is attested by current adherents and theological texts. External observers, historians, and former members would dispute the divine origin or current necessity, viewing it as a historical artifact or a means of social control.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) due to the demand for a specific, often difficult, lifestyle choice (polygamy) as a prerequisite for eternal salvation. Suppression is also very high (0.90) because non-compliance carries the ultimate penalty of eternal damnation and social ostracization within the faith. Accessibility collapse is near total (0.95) as alternatives (monogamy, leaving the faith) are presented as leading to eternal loss. Resistance (0.70) reflects both historical external opposition (federal government) and internal struggles of individuals. Theater ratio is low (0.10) because the belief is deeply held and acted upon, not merely performative. The claimed type is 'snare' because the coordination story (eternal progression, family unity) serves as cover for the profound extraction and coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of faithful adherents and church leadership, this is a divine 'rope' or even 'mountain' leading to eternal blessings. From the perspective of polygamous wives or disobedient members, it operates as a 'snare' due to the immense personal cost and lack of viable exit without eternal consequences. The engine's classification as 'snare' reflects the structural reality of extraction and suppression, regardless of the internal theological framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Faithful adherents are beneficiaries in the long term (promised exaltation) but bear significant costs in the short term, making their directionality complex but leaning towards beneficiary due to the ultimate spiritual reward. Church leadership benefits from the authority and loyalty derived from administering this divine law. Polygamous wives, disobedient members, and children of polygamous families are clear targets, bearing the direct costs and suffering the consequences of the constraint. The federal government, while an external agenda-setter, is seen by this reading as an opposing force, not a beneficiary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_human_interpretation,
    'Is the immutability of polygamy truly a direct, eternal divine command, or is it an interpretation of revelation that could be re-evaluated?',
    'Further theological scholarship, new prophetic revelation, or a shift in the interpretive tradition within the religious community.',
    'If re-evaluated as a mutable interpretation, the constraint''s extractiveness and suppression would significantly decrease, potentially reclassifying it from a Snare to a Piton or even a Rope, as the eternal consequences would diminish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_human_interpretation, conceptual, 'Ambiguity regarding the source and fixity of the divine command.').

omega_variable(
    eternal_consequences_vs_temporal_suffering,
    'Does the promised eternal exaltation genuinely outweigh the temporal suffering and costs imposed by the practice of polygamy, as experienced by its targets?',
    'Longitudinal studies of well-being among adherents and former adherents, or a shift in the theological emphasis on the nature of exaltation and its requirements.',
    'If the temporal suffering is found to consistently outweigh the perceived eternal benefits, the justification for the high extraction would be undermined, strengthening the Snare classification and highlighting its coercive nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eternal_consequences_vs_temporal_suffering, empirical, 'Balance between spiritual rewards and lived experience of suffering.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (church discipline, social pressure, legal vulnerability) or internalized (fear of eternal loss, identity fusion with the faith)?',
    'Post-exit suppression trajectory: if fear of eternal loss and identity-based self-censorship persist after formal church disaffiliation, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true freedom from the constraint more difficult.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a religious context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1843, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1843, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1843, 0.05).
narrative_ontology:measurement(eter_tr_t1860, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1860, 0.07).
narrative_ontology:measurement(eter_tr_t1880, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(eter_tr_t1900, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(eter_tr_t1925, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1925, 0.1).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(eter_be_t1843, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1843, 0.8).
narrative_ontology:measurement(eter_be_t1860, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1860, 0.82).
narrative_ontology:measurement(eter_be_t1880, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1880, 0.87).
narrative_ontology:measurement(eter_be_t1900, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement(eter_be_t1925, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1925, 0.84).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1843, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1843, 0.8).
narrative_ontology:measurement(eter_su_t1860, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1860, 0.85).
narrative_ontology:measurement(eter_su_t1880, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1880, 0.92).
narrative_ontology:measurement(eter_su_t1900, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1900, 0.88).
narrative_ontology:measurement(eter_su_t1925, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1925, 0.86).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
