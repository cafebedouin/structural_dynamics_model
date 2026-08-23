% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Doctrinal Authority — Continuity Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models the 'hermeneutic of continuity' reading of
 *   Vatican II's doctrinal authority: the Council's documents are read as
 *   organic developments within an unchanging deposit of faith, where
 *   apparent novelties (religious liberty, ecumenism, collegiality) are
 *   explications of implicit prior teaching rather than innovations. The
 *   reading functions as a tangled rope — it performs genuine coordination by
 *   giving the post-conciliar Church a unified interpretive framework that
 *   prevents schism and doctrinal chaos, while simultaneously extracting
 *   interpretive labor from progressive and traditionalist communities whose
 *   readings are excluded from legitimate reception. The magisterial teaching
 *   office sets the agenda; conciliar reception communities benefit through
 *   identity alignment; progressive reform advocates and traditionalist
 *   dissenters pay the cost of having their readings structurally
 *   marginalized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.28).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.35).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Doctrinal Authority — Continuity Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '509487ac-360e-4aaf-8340-e1c7a93367b0').
narrative_ontology:cs_kernel_codification('509487ac-360e-4aaf-8340-e1c7a93367b0', formalized).
narrative_ontology:cs_authority_grounding('509487ac-360e-4aaf-8340-e1c7a93367b0', lineage).
narrative_ontology:cs_interpretation_layer_present('509487ac-360e-4aaf-8340-e1c7a93367b0').
narrative_ontology:cs_reading_relation('509487ac-360e-4aaf-8340-e1c7a93367b0', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('509487ac-360e-4aaf-8340-e1c7a93367b0', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('509487ac-360e-4aaf-8340-e1c7a93367b0', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('509487ac-360e-4aaf-8340-e1c7a93367b0', foundational, conciliar_texts_explicate_implicit_tradition).
narrative_ontology:cs_axiom_status(conciliar_texts_explicate_implicit_tradition, holdable).
narrative_ontology:cs_axiom_grounding('509487ac-360e-4aaf-8340-e1c7a93367b0', conciliar_texts_explicate_implicit_tradition, deontological).
narrative_ontology:cs_axiom('509487ac-360e-4aaf-8340-e1c7a93367b0', foundational, magisterium_sole_authentic_interpreter_of_conciliar_reception).
narrative_ontology:cs_axiom_status(magisterium_sole_authentic_interpreter_of_conciliar_reception, holdable).
narrative_ontology:cs_axiom_grounding('509487ac-360e-4aaf-8340-e1c7a93367b0', magisterium_sole_authentic_interpreter_of_conciliar_reception, conventional).
narrative_ontology:cs_reference_frame('509487ac-360e-4aaf-8340-e1c7a93367b0', conciliar_texts_as_organic_development).
narrative_ontology:cs_drift_state('509487ac-360e-4aaf-8340-e1c7a93367b0', contemporary_synodal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('509487ac-360e-4aaf-8340-e1c7a93367b0', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterial_teaching_office).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conciliar_reception_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_advocates).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_dissenters).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, organic_development_of_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, conciliar_authority_subordinate_to_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Vatican II texts through the hermeneutic of continuity. Issues binding interpretations that resolve ambiguities in favor of doctrinal consistency. Maintains that the Council changed discipline and pastoral approach but not defined doctrine. Controls the canonical machinery that adjudicates legitimate reception.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterial_teaching_office, agenda_setter,
    institutional, generational, analytical, universal).

% Dioceses, religious orders, and lay movements that have integrated the continuity reading into their identity and apostolic self-understanding. They receive institutional legitimacy, pedagogical clarity, and protection from doctrinal sanction by aligning with the official hermeneutic. Exit would mean dissolving their formative self-conception as faithful conciliar recipients.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conciliar_reception_communities, beneficiary,
    organized, biographical, identity_locked, global).

% Theologians, pastoral leaders, and reform organizations who read Vatican II as authorizing ongoing doctrinal development. They bear the cost of having their proposals filtered through a continuity lens that treats novelty as suspect. Their institutional access depends on framing innovations as 'explications' rather than developments, which constrains the scope and pace of reform they can legitimately pursue.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_advocates, payer,
    moderate, biographical, constrained, global).

% Communities and clergy who reject the continuity reading as a cover for rupture. They bear the cost of either submitting to a hermeneutic they believe falsifies the Council's actual effect, or separating from canonical structures. Their identity is fused to the claim that Vatican II broke tradition; the continuity reading denies their central conviction while demanding their obedience.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_dissenters, payer,
    organized, biographical, identity_locked, global).

% Scholars who study the conciliar texts, debates, and reception history using historical-critical methods. They document discontinuities between pre- and post-conciliar teaching that the continuity reading must explain away. Their structural position is outside the canonical adjudication process but their work supplies the evidence base all readings must engage.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, historical_theological_academy, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutical framework that allows the Church to claim both fidelity to tradition and legitimate adaptation to modernity — resolving the tension between immutability of doctrine and historical change by reading all conciliar novelty as explication of implicit prior teaching.
% TRANSFER_FUNCTION: Moves interpretive authority from the conciliar texts' plain sense (which contains tensions and ambiguities) to the magisterial teaching office's authoritative resolution of those tensions. The transfer is: textual openness → institutional closure; pastoral flexibility → doctrinal fixity; the cost is borne by those whose readings are excluded from legitimate reception.
% ABSENT_VOICES: The Council Fathers who voted for texts precisely because of their strategic ambiguity — the 'minority' bishops who understood they were authorizing change but accepted ambiguous language for unity. Also absent: the laity in the Global South whose reception was shaped by liberationist readings later disciplined as discontinuities. Both groups would contest the claim that the texts themselves mandate continuity.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the magisterium would lose its primary tool for adjudicating post-conciliar disputes. Progressive reforms currently blocked as 'discontinuous' would gain legitimacy; traditionalist communities currently marginalized as 'rejecting the Council' would lose their structural foil. The entire post-conciliar ecclesiastical order — liturgical, canonical, ecumenical — is organized around this reading's authority.
% FOUNDING_PROBLEM: The Council produced texts with deliberate ambiguities to achieve consensus. The founding problem was: how to present these texts as a coherent, authoritative act of the magisterium without either admitting doctrinal rupture or freezing pastoral adaptation. The continuity reading solves this by making ambiguity a feature — it becomes the space where the teaching office exercises its charism of authoritative interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the conciliar historical record: the 'moderate' majority at Vatican II explicitly sought formulations that could satisfy both reformist and conservative blocs (Alberigo, Komonchak, O'Malley). The continuity reading's claim that ambiguity serves the teaching office's interpretive role is attested by Ratzinger/Benedict XVI's own accounts of conciliar hermeneutics, and by the canonical machinery built to enforce it (CDF notifications, motu proprio Summorum Pontificum, Traditionis Custodes). No source outside the benefiting magisterial structure treats the founding problem as resolved.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.28) on doctrinal change claims — the reading genuinely limits doctrinal mutation. But extraction rises on liturgical/pastoral practice where the continuity lens forces complex adaptations into a 'development' frame. Suppression (0.35) reflects active canonical enforcement against readings deemed discontinuous, but stops short of total exclusion (both progressive and traditionalist communities persist). Theater ratio (0.22) is moderate: the hermeneutic performs real intellectual work resolving textual tensions, but a growing share of its operation defends institutional authority against historical evidence of rupture. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives (rupture readings) remain intellectually viable and organizationally active despite institutional pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterial seat, the constraint is a rope — genuine coordination preventing doctrinal fragmentation. From progressive and traditionalist payer seats, it computes as snare — their readings are suppressed to maintain a unity that benefits the center. The conciliar reception communities experience it as scaffold: transitional support for their identity that would collapse if the reading changed. The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the aggregate structure where coordination and extraction are inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial teaching office is the structural beneficiary (d ~ 0.15): it collects interpretive authority and canonical control. Conciliar reception communities are identity-locked beneficiaries (d ~ 0.25): they gain legitimacy but cannot exit without self-dissolution. Progressive reform advocates are constrained payers (d ~ 0.7): they bear costs but retain some institutional mobility. Traditionalist dissenters are identity-locked payers (d ~ 0.85): their core conviction is structurally denied while their obedience is demanded. The historical-theological academy sits at analytical (d ~ 0.5): it neither collects nor pays but supplies the evidence all sides must engage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing conciliar ambiguity without admitting rupture) remains live — the Church still faces the same tension between immutability claims and historical change. But the continuity reading has accumulated extraction: it now treats as 'implementation errors' what historical evidence suggests were intended developments (religious liberty, ecumenism). The mandate has not atrophied; it has expanded to cover domains the Council Fathers deliberately left open. This is not mandatrophy but mandate creep — the reading's coordination function has become the vehicle for a widening extraction of interpretive freedom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_empirical_underdetermination,
    'Does the historical record of Vatican II''s drafting, debates, and immediate reception support the continuity reading''s claim that all conciliar novelty is implicit in prior tradition, or does it support the rupture readings'' claim of intentional discontinuity?',
    'Comparative analysis of conciliar acta, intervention texts, and the schema evolution of key documents (Dignitatis Humanae, Unitatis Redintegratio, Lumen Gentium, Gaudium et Spes) against pre-conciliar magisterial teaching. The question is whether the continuity reading''s explications are historically plausible or retroactive harmonizations.',
    'If historical evidence favors rupture, the continuity reading''s coordination function rests on a constructed narrative — its extraction of interpretive authority from progressive/traditionalist readings is not the price of genuine coordination but the enforcement of a founding myth. If evidence favors continuity, the reading''s low ε on doctrine is structurally earned.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_vs_rupture_empirical_underdetermination, empirical, 'Whether the continuity reading''s core historical claim is empirically warranted or a retrospective construction.').

omega_variable(
    liturgical_pastoral_extraction_boundary,
    'Is the higher extraction on liturgical/pastoral practice (vs. doctrine) a necessary cost of the coordination function, or does the continuity reading overextend its hermeneutic to domains where the Council intended genuine change?',
    'Analyze the post-conciliar liturgical reform (Missal of 1970), ecumenical dialogue structures, and episcopal conference governance against the conciliar texts'' explicit mandates. Determine where the continuity reading adds constraints not in the texts themselves.',
    'If the reading adds constraints beyond the texts, its coordination function has become a vehicle for institutional control over domains the Council opened to legitimate diversity. This would raise the constraint''s effective extraction and shift its classification toward snare for those domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_pastoral_extraction_boundary, conceptual, 'Whether the continuity reading''s application to practice exceeds its textual mandate.').

omega_variable(
    identity_lock_mechanism_traditionalist,
    'Is the traditionalist dissenter''s identity_locked exit a structural feature of the continuity reading (the reading denies their core conviction while demanding obedience), or is it a contingent outcome of their own self-definition against the reading?',
    'Compare traditionalist communities that have regularized (Ecclesia Dei institutes, Personal Ordinariates) with those that remain in irregular canonical status. Assess whether the continuity reading''s logic structurally excludes their self-understanding or whether regularization pathways exist that they refuse.',
    'If identity_lock is structural, the reading extracts from traditionalists by design — their conviction is the price of the reading''s coherence. If contingent, the extraction is lower and the reading could accommodate them without losing its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalist, conceptual, 'Whether traditionalist identity_lock is produced by the reading''s logic or by traditionalist self-definition.').

omega_variable(
    committer_structure_kernel_relations,
    'How does this continuity reading structurally relate to the other three declared readings of the vatican_ii_doctrinal_authority kernel?',
    'Formal analysis of the logical relations between the four readings'' core premises. Does the continuity reading foreclose, coexist with, or influence each sibling? Documented in cs_structure.reading_relations and cs_structure.axioms.',
    'The committer structure determines whether the kernel exhibits genuine pluralism (coexists_with), structural tension (influences), or logical exclusion (forecloses). This governs how the engine models cross-reading contamination and foreclosure dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_relations, conceptual, 'Committee frame structural relations to sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1985, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1995, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2005, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2015, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2025, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(vatican_ii_continuity_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.18).
narrative_ontology:measurement(vatican_ii_continuity_be_t1985, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(vatican_ii_continuity_be_t1995, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1995, 0.24).
narrative_ontology:measurement(vatican_ii_continuity_be_t2005, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement(vatican_ii_continuity_be_t2015, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(vatican_ii_continuity_be_t2025, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(vatican_ii_continuity_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(vatican_ii_continuity_su_t1985, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement(vatican_ii_continuity_su_t1995, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1995, 0.33).
narrative_ontology:measurement(vatican_ii_continuity_su_t2005, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(vatican_ii_continuity_su_t2015, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(vatican_ii_continuity_su_t2025, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_ecumenical_dialogue_structure).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_canonical_discipline).

% DUAL FORMULATION NOTE:
% Part of the vatican_ii_doctrinal_authority constraint family. This continuity_reading has low ε on doctrinal change but higher effective extraction on practice. The rupture_progressive_reading and rupture_traditionalist_reading have higher ε on doctrine but lower on practice (they accept rupture as fact). The composite_overdetermination_reading decomposes the Council into distinct structural changes with separate ε values per domain. All four stories link via affects_constraints to model the kernel's internal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
