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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Interpretation of Scripture and Tradition
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint describes the theological position that Scripture
 *   requires the living Tradition and the Magisterium (teaching authority of
 *   the Church) for its authoritative interpretation, with the Magisterium
 *   guarding the 'deposit of faith'. It is one reading of the broader
 *   'biblical_authority' kernel. The constraint is claimed as a Tangled Rope,
 *   reflecting its dual function of coordinating faith while also extracting
 *   interpretive agency and consolidating power within the institutional
 *   hierarchy. The metrics reflect high extraction and suppression,
 *   maintained by active enforcement of doctrinal conformity.
 *
 * KEY AGENTS:
 *   - institutional_hierarchy: Agenda-setter/Beneficiary (institutional/arbitrage)
 *   - lay_adherents: Payer/Beneficiary (powerless/identity_locked)
 *   - dissenting_theologians: Payer (moderate/constrained)
 *   - sola_scriptura_advocates: Excluded (organized/mobile)
 *   - conciliar_theologians: Excluded (organized/mobile)
 *   - analytical_historians: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.78).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.85).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Interpretation of Scripture and Tradition").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3').
narrative_ontology:cs_kernel_codification('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', formalized).
narrative_ontology:cs_authority_grounding('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', lineage).
narrative_ontology:cs_interpretation_layer_present('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3').
narrative_ontology:cs_reading_relation('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', foundational, magisterial_infallibility_in_faith_and_morals).
narrative_ontology:cs_axiom_status(magisterial_infallibility_in_faith_and_morals, holdable).
narrative_ontology:cs_axiom_grounding('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', magisterial_infallibility_in_faith_and_morals, theological).
narrative_ontology:cs_axiom('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', foundational, tradition_as_interpretive_key).
narrative_ontology:cs_axiom_status(tradition_as_interpretive_key, holdable).
narrative_ontology:cs_axiom_grounding('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', tradition_as_interpretive_key, conventional).
narrative_ontology:cs_reference_frame('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', apostolic_succession_and_doctrinal_unity).
narrative_ontology:cs_drift_state('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', contemporary_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eff3ea54-ad74-4ba3-a1b8-5a4c52c9c1a3', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, dissenting_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, lay_adherents).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims authority to definitively interpret Scripture and Tradition, ensuring doctrinal unity and guarding the 'deposit of faith'. Benefits from centralized control, spiritual authority, and the mediation of grace through sacraments it administers. Actively enforces interpretive norms.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, institutional_hierarchy, beneficiary).

% Receive doctrinal certainty, spiritual guidance, and sacramental grace mediated by the institutional hierarchy. Pay by ceding individual interpretive agency and adhering to prescribed theological frameworks. Exit is difficult due to deep identity formation within the tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_adherents, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, lay_adherents, beneficiary).

% Seek to engage with Scripture and Tradition critically, often proposing alternative interpretations. They pay through professional marginalization, censure, or excommunication if their views deviate too far from magisterial teaching. Their careers and standing are constrained by institutional approval.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, global).

% Advocate for Scripture alone as the sufficient and self-interpreting authority. They are structurally excluded from the interpretive framework of this constraint, representing a competing theological paradigm that rejects the necessity of tradition and magisterium for authoritative interpretation.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, sola_scriptura_advocates, excluded,
    organized, generational, mobile, global).

% Emphasize the authority of ecumenical councils and patristic consensus as the primary interpretive lens for Scripture and Tradition, often viewing tradition as a living continuity rather than solely magisterial decree. While valuing tradition, their approach differs from the strict magisterial model and they are excluded from its specific interpretive mechanism.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, conciliar_theologians, excluded,
    organized, generational, mobile, global).

% Study the historical development of biblical interpretation, theological doctrines, and institutional authority without being bound by the internal claims of any specific tradition. They analyze the social, political, and intellectual forces shaping interpretive practices.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, institutional_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a centralized, consistent, and authoritative interpretation of Christian doctrine, ensuring unity of faith and practice across generations and preventing fragmentation or heresy.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and the mediation of spiritual benefits (e.g., sacraments) from individual believers and local communities to the institutional hierarchy, in exchange for doctrinal certainty and a guaranteed path to salvation.
% ABSENT_VOICES: Sola Scriptura advocates and conciliar theologians are excluded; they would argue for alternative, less centralized, or more historically grounded interpretive authorities. Lay interpretive agency is also suppressed, as their direct engagement with scripture is subordinated to magisterial teaching.
% DISAPPEARANCE_RATIONALE: If the magisterium's authoritative interpretive role vanished overnight, the institutional church would face immediate and severe doctrinal fragmentation, a crisis of authority regarding sacraments and moral teaching, and a fundamental re-evaluation of its identity and mission, leading to a complete reorganization of its structure and theological landscape.
% FOUNDING_PROBLEM: Early Christian communities faced diverse and often conflicting interpretations of scripture, leading to doctrinal disputes, heresies, and challenges to the unity of the nascent Church. A mechanism was needed to preserve the 'deposit of faith' and ensure consistent teaching.
% FOUNDING_PROBLEM_CORROBORATION: The institutional hierarchy consistently attests to the ongoing problem of doctrinal relativism and the need for a unified interpretive authority. Independent historians and sociologists of religion corroborate the historical problem of fragmentation but often contest the *necessity* or *efficacy* of the specific magisterial solution for contemporary faith, noting its role in maintaining institutional power.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) because the institutional hierarchy centralizes interpretive authority and mediates spiritual benefits, effectively extracting interpretive agency and requiring adherence to its structures. Suppression is very high (0.85) due to active enforcement mechanisms (e.g., anathemas, excommunication, professional censure) that prevent alternative interpretations from gaining traction within the tradition. Theater ratio is moderate (0.45): while genuine spiritual guidance and doctrinal preservation occur, a significant portion of institutional activity is performative maintenance of authority and suppression of dissent, rather than purely functional coordination. Accessibility collapse is moderate (0.60) as alternatives exist outside the tradition but are actively suppressed within it. Resistance is moderate (0.55) reflecting ongoing internal and external challenges to magisterial authority.
 *
 * PERSPECTIVAL GAP:
 *   The institutional hierarchy perceives this constraint as a necessary Rope, ensuring unity and truth. Lay adherents may experience it as a beneficial Rope (providing certainty) or a Snare (limiting personal spiritual exploration). Dissenting theologians clearly experience it as a Snare, as their interpretive freedom is curtailed. The engine's computation of per-seat types will reflect these divergences based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional hierarchy is the primary beneficiary (d near 0.0) as it collects the benefits of centralized authority and doctrinal control. Lay adherents are both beneficiaries (d near 0.5, receiving certainty) and payers (d near 0.5, ceding agency). Dissenting theologians are clear targets (d near 1.0) as they bear the costs of suppression. Sola Scriptura and conciliar advocates are excluded, their alternative frameworks actively suppressed by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Rope (as the institutional hierarchy claims) by highlighting the significant extraction of interpretive agency and the active suppression of alternatives. It also avoids mislabeling as a pure Snare by acknowledging the genuine coordination function of doctrinal unity and spiritual guidance for many adherents. The 'live' status of the founding problem, combined with high extraction, suggests an ongoing tension between coordination and rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tradition_definition_ambiguity,
    'Is ''Tradition'' in this context a living, evolving consensus, or a fixed, unchangeable body of doctrine guarded by the Magisterium?',
    'Historical-theological analysis of magisterial documents and theological discourse over time, examining how the concept of Tradition has been applied and developed in practice.',
    'If Tradition is more fluid, the constraint''s suppression of alternative interpretations might be less justified, potentially lowering its effective suppression. If it is fixed, the suppression is more consistent with the internal logic of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradition_definition_ambiguity, conceptual, 'Ambiguity in the nature and scope of ''Tradition'' itself.').

omega_variable(
    necessity_of_magisterial_authority,
    'Is a centralized, infallible magisterial authority truly necessary to prevent doctrinal fragmentation and preserve the ''deposit of faith'', or are alternative, more decentralized models equally effective?',
    'Comparative theological and sociological studies of religious traditions with different interpretive authorities, assessing their long-term doctrinal coherence and spiritual vitality.',
    'If alternative models prove effective, the justification for the high extraction and suppression of this constraint weakens, potentially reclassifying it closer to a Snare. If it is uniquely effective, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_magisterial_authority, empirical, 'Whether magisterial authority is uniquely necessary for doctrinal unity.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression of lay interpretive agency structural (institutional rules, lack of access to theological education) versus internalized (self-censorship, belief in personal inadequacy for interpretation)?',
    'Qualitative sociological research among lay adherents, examining their perceived freedom and actual practices of biblical interpretation, and the persistence of interpretive deference even when structural barriers are reduced.',
    'If suppression is largely internalized, the effective suppression is higher and more resilient to external reforms, making the constraint more deeply entrenched. If primarily structural, reforms targeting institutional barriers would have a more immediate impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression of lay interpretive agency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__tradition_scripture_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__tradition_scripture_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(bibl_tr_t60, biblical_authority__tradition_scripture_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(bibl_tr_t80, biblical_authority__tradition_scripture_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__tradition_scripture_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__tradition_scripture_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__tradition_scripture_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(bibl_be_t60, biblical_authority__tradition_scripture_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(bibl_be_t80, biblical_authority__tradition_scripture_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__tradition_scripture_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__tradition_scripture_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__tradition_scripture_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(bibl_su_t60, biblical_authority__tradition_scripture_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(bibl_su_t80, biblical_authority__tradition_scripture_reading, suppression_requirement, 80, 0.84).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__tradition_scripture_reading, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sacramental_efficacy_doctrine).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, moral_theology_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'biblical_authority' kernel, alongside 'sola_scriptura_reading' and 'conciliar_reading'. Each reading instantiates a distinct constraint with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
