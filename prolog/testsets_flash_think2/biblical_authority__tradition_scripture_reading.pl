% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Interpretation of Scripture and Tradition
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint describes the theological position that Scripture
 *   requires the authoritative interpretation of Tradition, as guarded by the
 *   Magisterium, for a full and correct understanding of Christian faith. It
 *   is a reading of the broader 'biblical_authority' kernel, emphasizing
 *   centralized interpretive control and the role of an institutional
 *   hierarchy in defining doctrine. The constraint is claimed as a Rope by
 *   its proponents (ensuring unity and truth), but its operational metrics
 *   reflect substantial extraction and suppression, leading to a computed
 *   Tangled Rope classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.78).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.85).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Interpretation of Scripture and Tradition").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'd46999e3-c33c-4522-9592-78c65934f6ed').
narrative_ontology:cs_kernel_codification('d46999e3-c33c-4522-9592-78c65934f6ed', formalized).
narrative_ontology:cs_authority_grounding('d46999e3-c33c-4522-9592-78c65934f6ed', lineage).
narrative_ontology:cs_interpretation_layer_present('d46999e3-c33c-4522-9592-78c65934f6ed').
narrative_ontology:cs_reading_relation('d46999e3-c33c-4522-9592-78c65934f6ed', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('d46999e3-c33c-4522-9592-78c65934f6ed', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('d46999e3-c33c-4522-9592-78c65934f6ed', foundational, magisterial_infallibility).
narrative_ontology:cs_axiom_status(magisterial_infallibility, holdable).
narrative_ontology:cs_axiom_grounding('d46999e3-c33c-4522-9592-78c65934f6ed', magisterial_infallibility, theological).
narrative_ontology:cs_axiom('d46999e3-c33c-4522-9592-78c65934f6ed', foundational, tradition_coequal_with_scripture).
narrative_ontology:cs_axiom_status(tradition_coequal_with_scripture, holdable).
narrative_ontology:cs_axiom_grounding('d46999e3-c33c-4522-9592-78c65934f6ed', tradition_coequal_with_scripture, theological).
narrative_ontology:cs_reference_frame('d46999e3-c33c-4522-9592-78c65934f6ed', apostolic_succession_and_magisterial_authority).
narrative_ontology:cs_drift_state('d46999e3-c33c-4522-9592-78c65934f6ed', post_vatican_ii_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('d46999e3-c33c-4522-9592-78c65934f6ed', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, institutional_magisterium).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_believers).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, theologians_outside_magisterium).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Church, which claims to infallibly interpret Scripture and Tradition, guarding the 'deposit of faith'. It sets doctrinal boundaries, adjudicates theological disputes, and enforces adherence through ecclesiastical discipline. Its authority is foundational to the institutional structure.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, institutional_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).

% Bishops, priests, and deacons who derive their authority and sacramental function from the magisterium. They mediate grace through sacraments and teach doctrine as defined by the magisterium, reinforcing their central role in the spiritual lives of believers. Their professional identity is fused with this structure.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clerical_hierarchy, beneficiary,
    institutional, biographical, identity_locked, global).

% Receive doctrine and spiritual guidance through the magisterium and clergy. They are expected to assent to magisterial teachings and participate in sacraments for salvation. Their direct interpretive agency is suppressed, and alternatives are presented as spiritually dangerous. Exit means abandoning their faith community and spiritual framework.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_believers, payer,
    powerless, biographical, identity_locked, global).

% Academic theologians who operate within the broader tradition but may find their interpretive freedom constrained by magisterial pronouncements. They contribute to theological discourse but risk censure or loss of institutional standing if their conclusions diverge too sharply from official teaching. Their careers and intellectual identity are often tied to the institutional church.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, theologians_outside_magisterium, payer,
    moderate, biographical, constrained, global).

% Protestant traditions that assert Scripture alone is the sufficient and ultimate authority for faith and practice, rejecting the necessity of an infallible magisterium or co-equal tradition. They are structurally excluded from the interpretive framework of this constraint, representing a fundamental alternative.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, sola_scriptura_advocates, excluded,
    organized, generational, mobile, global).

% Theologians and movements within Christianity (e.g., some Orthodox traditions, historical conciliarism) who emphasize ecumenical councils and patristic consensus as the primary interpretive lens for Scripture and Tradition, rather than a singular, ongoing magisterial authority. They are excluded from the specific interpretive mechanism of this constraint, though they share a broader commitment to tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, conciliar_theologians, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a centralized, unified, and authoritative interpretation of divine revelation (Scripture and Tradition), preventing doctrinal fragmentation, heresy, and ensuring a consistent understanding of faith and morals across the global Church.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and spiritual mediation from individual believers and local communities to the institutional magisterium and clerical hierarchy, in exchange for doctrinal certainty, sacramental grace, and a unified ecclesial identity.
% ABSENT_VOICES: Sola Scriptura advocates would argue for the sufficiency of Scripture alone, rejecting the need for magisterial mediation. Conciliar theologians would emphasize the authority of ecumenical councils and broader patristic consensus over a singular, ongoing magisterial decree. Both are structurally excluded from the interpretive process of this constraint.
% DISAPPEARANCE_RATIONALE: If the magisterium's authoritative interpretation and enforcement vanished overnight, the institutional church would face immediate and severe doctrinal fragmentation, leading to widespread schism, a collapse of its unified identity, and a complete reorganization of its structure and claims to authority. The global institution as it exists would cease to function.
% FOUNDING_PROBLEM: Early Christian communities faced diverse interpretations of apostolic teachings, the emergence of heresies, and a pressing need for a unified understanding of faith and practice to maintain cohesion and identity across geographically dispersed communities.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of early church councils (e.g., Nicaea, Chalcedon) and patristic writings (e.g., Irenaeus, Augustine) attest to the problem of doctrinal disputes and the early development of authoritative structures. While the *necessity* of a magisterial solution is contested by other Christian traditions, the historical problem of fragmentation is widely acknowledged by historians and theologians outside the benefiting parties.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because interpretive authority and spiritual mediation are concentrated in the magisterium and clergy, requiring lay believers to defer their own interpretive agency. Suppression is very high (0.85) due to active enforcement of doctrinal conformity, censure of dissenting theologians, and the presentation of alternatives as heretical or spiritually dangerous. Theater ratio is moderate (0.25); while there is genuine theological work and pastoral care, a portion of institutional activity is dedicated to maintaining the interpretive monopoly rather than purely serving the faithful. Accessibility collapse is moderate (0.65) as alternatives exist (e.g., other Christian traditions) but are presented as illegitimate or dangerous from within this framework. Resistance is moderate (0.45), reflecting ongoing internal theological debates and external challenges from other Christian traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the magisterium, this constraint is a necessary Rope, ensuring the purity and unity of faith. From the perspective of lay believers or dissenting theologians, it can feel like a Snare or Tangled Rope, where genuine spiritual needs are met, but at the cost of interpretive freedom and agency. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional magisterium and clerical hierarchy are clear beneficiaries, deriving their authority, legitimacy, and social role from this interpretive framework. Lay believers and theologians operating outside the magisterium are the primary targets, as their interpretive agency is curtailed, and they bear the costs of conformity. Sola Scriptura advocates and conciliar theologians are structurally excluded, as their alternative interpretive frameworks are incompatible with or marginalized by this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a faithful instantiation of the ''biblical_authority'' kernel, or does its operational reality diverge from its claimed theological grounding?',
    'Comparative theological analysis across Christian traditions, historical-critical studies of the development of magisterial authority, and sociological studies of lay interpretive practices.',
    'If the operational reality significantly diverges, it would suggest the constraint functions more as an institutional power structure than a theological truth claim, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''biblical_authority'' kernel, specifically the ''tradition_scripture_reading''.').

omega_variable(
    doctrinal_unity_cost_benefit,
    'Is the suppression of lay interpretive agency and theological dissent a necessary cost for achieving doctrinal unity and preventing fragmentation, or is it an overreach that stifles spiritual and intellectual growth?',
    'Empirical studies comparing doctrinal stability and spiritual vitality in traditions with centralized vs. decentralized interpretive authority, and theological arguments regarding the nature of revelation and human freedom.',
    'If the cost is deemed unnecessary or disproportionate, the extractiveness and suppression metrics would be further amplified, reinforcing a Snare classification. If deemed necessary, it would support the coordination function, potentially shifting towards a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_unity_cost_benefit, preference, 'Assessing the trade-off between centralized interpretive authority and individual interpretive freedom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__tradition_scripture_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__tradition_scripture_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(bibl_tr_t60, biblical_authority__tradition_scripture_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(bibl_tr_t80, biblical_authority__tradition_scripture_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__tradition_scripture_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__tradition_scripture_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__tradition_scripture_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(bibl_be_t60, biblical_authority__tradition_scripture_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(bibl_be_t80, biblical_authority__tradition_scripture_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__tradition_scripture_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__tradition_scripture_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__tradition_scripture_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(bibl_su_t60, biblical_authority__tradition_scripture_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(bibl_su_t80, biblical_authority__tradition_scripture_reading, suppression_requirement, 80, 0.83).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__tradition_scripture_reading, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sacramental_discipline).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, clerical_celibacy).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'biblical_authority' kernel. Its structural claims about tradition and magisterial authority directly influence and are influenced by the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
