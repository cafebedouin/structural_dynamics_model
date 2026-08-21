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
 *   human_readable: Magisterial Authority over Biblical Interpretation (Tradition-Scripture Reading)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint describes the 'Tradition-Scripture' reading of biblical
 *   authority, where Scripture is interpreted authoritatively only within the
 *   living Tradition of the Church, guarded by the Magisterium. This reading
 *   asserts that both Scripture and Tradition are divine revelation, with the
 *   Magisterium serving as the infallible interpreter of both. It is a
 *   specific instantiation of the broader 'biblical_authority' kernel,
 *   distinct from 'sola_scriptura' or 'conciliar' readings. The high
 *   extractiveness reflects the transfer of interpretive agency and the
 *   requirement for sacramental mediation, while high suppression indicates
 *   the active enforcement of doctrinal conformity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.78).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.85).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Authority over Biblical Interpretation (Tradition-Scripture Reading)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '58065f37-64ce-497c-86ef-540d74d48059').
narrative_ontology:cs_kernel_codification('58065f37-64ce-497c-86ef-540d74d48059', formalized).
narrative_ontology:cs_authority_grounding('58065f37-64ce-497c-86ef-540d74d48059', lineage).
narrative_ontology:cs_interpretation_layer_present('58065f37-64ce-497c-86ef-540d74d48059').
narrative_ontology:cs_reading_relation('58065f37-64ce-497c-86ef-540d74d48059', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('58065f37-64ce-497c-86ef-540d74d48059', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('58065f37-64ce-497c-86ef-540d74d48059', foundational, magisterium_infallible_interpreter).
narrative_ontology:cs_axiom_status(magisterium_infallible_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('58065f37-64ce-497c-86ef-540d74d48059', magisterium_infallible_interpreter, theological).
narrative_ontology:cs_axiom('58065f37-64ce-497c-86ef-540d74d48059', foundational, tradition_coequal_revelation).
narrative_ontology:cs_axiom_status(tradition_coequal_revelation, holdable).
narrative_ontology:cs_axiom_grounding('58065f37-64ce-497c-86ef-540d74d48059', tradition_coequal_revelation, theological).
narrative_ontology:cs_reference_frame('58065f37-64ce-497c-86ef-540d74d48059', apostolic_succession_deposit_of_faith).
narrative_ontology:cs_drift_state('58065f37-64ce-497c-86ef-540d74d48059', contemporary_pluralistic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('58065f37-64ce-497c-86ef-540d74d48059', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterium).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Church, which claims to infallibly interpret Scripture and Tradition. It defines doctrine, adjudicates theological disputes, and enforces adherence through ecclesiastical discipline. Its authority is foundational to the institutional structure.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The bishops, priests, and other clergy who administer the Church. Their authority and sacramental mediation are directly tied to the magisterium's interpretive claims, ensuring their central role in the spiritual lives of adherents and in the governance of the institution.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, institutional_hierarchy, beneficiary,
    institutional, generational, identity_locked, global).

% Individual adherents who are taught that their personal interpretation of Scripture must conform to magisterial teaching. They bear the cost of intellectual submission and are dependent on the clergy for authoritative understanding and sacramental grace. Exit means leaving the Church and potentially losing access to perceived salvific means.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    powerless, biographical, constrained, local).

% Scholars who dedicate their careers to studying Scripture and Tradition. While they contribute to theological discourse, their work is subject to magisterial review and approval. They risk censure or loss of academic standing if their interpretations deviate from official teaching, limiting their intellectual freedom.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, theologians, payer,
    moderate, biographical, constrained, global).

% Protestant traditions that assert Scripture alone is the sufficient and ultimate authority. Their interpretive methodology and theological conclusions are fundamentally incompatible with the tradition-scripture reading, leading to their exclusion from the magisterial interpretive framework.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, sola_scriptura_proponents, excluded,
    organized, generational, mobile, global).

% Those who emphasize the authority of ecumenical councils and the broader patristic consensus as the primary interpretive lens for Scripture, viewing tradition as a living, evolving stream rather than a static deposit guarded by a singular magisterium. Their approach, while valuing tradition, differs in its locus of authority.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, conciliar_theologians, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures doctrinal unity and prevents fragmentation of belief by providing a single, authoritative interpretive framework for Scripture and Tradition, thereby maintaining a coherent 'deposit of faith' across generations and geographies.
% TRANSFER_FUNCTION: Transfers interpretive authority and spiritual mediation from individual believers and independent scholars to the magisterium and institutional hierarchy, in exchange for doctrinal certainty and access to sacraments.
% ABSENT_VOICES: Proponents of 'sola scriptura' and those who advocate for a more decentralized, conciliar, or individual-led interpretive authority are excluded. They would argue for direct access to Scripture and a less mediated spiritual experience, but their views are deemed heterodox within this framework.
% DISAPPEARANCE_RATIONALE: If the magisterium's claim to authoritative interpretation vanished, the institutional hierarchy would lose its primary source of legitimacy and control. Doctrinal unity would fragment, individual interpretation would proliferate, and the Church's structure would fundamentally reorganize, likely leading to multiple, competing interpretive communities.
% FOUNDING_PROBLEM: The early Church faced diverse interpretations of Christ's teachings and apostolic writings, leading to heresies and schisms, necessitating a mechanism to preserve doctrinal purity and unity.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium itself attests that the problem of doctrinal fragmentation and heresy remains live, requiring its ongoing vigilance. While external observers might contest the necessity of a singular magisterial authority, the historical record of early Christian disputes corroborates the existence of the founding problem of interpretive diversity.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.78) stems from the significant transfer of interpretive agency and spiritual mediation from individuals to the institutional hierarchy. Lay interpretive agency is suppressed, and access to grace is mediated, creating a dependency. Suppression (0.85) is high due to the active enforcement mechanisms (e.g., anathemas, excommunication, censorship of theological works) that ensure doctrinal conformity and prevent alternative interpretations from gaining traction. The theater ratio is low (0.15) because the magisterium's interpretive function is genuinely active and central to the institutional operation, not merely performative. Accessibility collapse is moderate-high (0.70) because while alternative interpretations exist outside the Church, within the framework, they are deemed invalid or heretical, effectively collapsing their accessibility for adherents. Resistance is low (0.30) because internal dissent is actively suppressed, and external resistance is largely ignored as coming from outside the authoritative framework.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, this is a necessary 'Rope' for preserving the integrity of faith and ensuring salvation, a coordination mechanism against chaos. From the perspective of lay interpretive agency and theologians, it functions as a 'Snare' or 'Tangled Rope,' extracting intellectual freedom and agency while providing a perceived benefit of doctrinal certainty. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and the institutional hierarchy are clear beneficiaries (d near 0.0), as their authority and existence are directly predicated on this interpretive framework. Lay interpretive agency and theologians are targets (d near 1.0), as they must submit their understanding and work to the magisterium's authority, bearing the cost of intellectual and spiritual dependency. Sola Scriptura proponents and conciliar theologians are excluded, as their very interpretive premises are incompatible with this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to preserve doctrinal unity and prevent heresy is still considered 'live' by its beneficiaries. However, the high extractiveness and suppression suggest that while a coordination function exists, it is deeply intertwined with asymmetric power and control. The classification as a 'Tangled Rope' (rather than a pure 'Rope') prevents mislabeling the coordination as benign, highlighting the extractive component that benefits the institutional hierarchy at the expense of individual interpretive agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    locus_of_infallibility,
    'Is the Magisterium''s claim to infallible interpretation of Scripture and Tradition empirically verifiable or solely a matter of theological assertion?',
    'Historical-critical analysis of magisterial pronouncements against documented historical and scientific facts, or a theological re-evaluation of the nature of infallibility itself.',
    'If the claim to infallibility is found to be empirically or logically untenable, the constraint''s legitimacy would collapse, significantly reducing its suppression and extractiveness, potentially reclassifying it as a ''Piton'' or ''Snare'' sustained by inertia or pure coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(locus_of_infallibility, empirical, 'The empirical grounding of magisterial infallibility.').

omega_variable(
    necessity_of_mediation,
    'Is sacramental mediation by the institutional hierarchy structurally necessary for salvation, or is it a constructed requirement that enhances clerical power?',
    'Comparative theological study of Christian traditions that do not require such mediation, examining their spiritual outcomes and adherence rates, or a re-interpretation of early Christian soteriology.',
    'If mediation is found to be a constructed requirement, the extractiveness from lay interpretive agency would be re-evaluated as higher, and the constraint''s coordination function would be seen as a cover for power consolidation, pushing it closer to a ''Snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_mediation, conceptual, 'The structural necessity of clerical mediation for spiritual outcomes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of lay interpretive agency structural (ecclesiastical laws, censorship) or internalized (adherents'' belief in magisterial authority, fear of heresy)?',
    'Post-exit interpretive trajectory: if interpretive conformity persists after an adherent leaves the Church, reclassify as partially internalized. Sociological studies of former adherents'' interpretive practices.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making genuine interpretive freedom harder to achieve even outside the formal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive agency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bibl_tr_t10, biblical_authority__tradition_scripture_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__tradition_scripture_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(bibl_tr_t30, biblical_authority__tradition_scripture_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__tradition_scripture_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(bibl_tr_t50, biblical_authority__tradition_scripture_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(bibl_be_t10, biblical_authority__tradition_scripture_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__tradition_scripture_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(bibl_be_t30, biblical_authority__tradition_scripture_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__tradition_scripture_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(bibl_be_t50, biblical_authority__tradition_scripture_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bibl_su_t10, biblical_authority__tradition_scripture_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__tradition_scripture_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(bibl_su_t30, biblical_authority__tradition_scripture_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__tradition_scripture_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(bibl_su_t50, biblical_authority__tradition_scripture_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
