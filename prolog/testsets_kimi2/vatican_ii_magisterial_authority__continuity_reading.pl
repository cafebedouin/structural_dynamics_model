% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Hermeneutic of Continuity (Magisterial Authority Reading)
 *   domain: ecclesiological/institutional/hermeneutics
 *
 * SUMMARY:
 *   This constraint story authors the continuity reading of the Vatican II
 *   magisterial authority kernel: the claim that the Second Vatican Council
 *   represents organic development within unbroken tradition, with no rupture
 *   from prior magisterial teaching. As a standing arrangement under contest,
 *   this reading operates as an enforceable hermeneutic that constrains
 *   conciliar implementation: the 'spirit of Vatican II' is ruled
 *   unauthorized, Sacrosanctum Concilium Â§36 on Latin preservation is
 *   treated as binding, and Dignitatis Humanae is reconciled with the
 *   Syllabus of Errors through thesis/hypothesis distinctions or
 *   development-of-doctrine frameworks. The constraint is actively enforced
 *   by the Roman Curia against progressive theologians and liturgical
 *   reformers, while benefiting traditionalist clergy and laity who receive
 *   magisterial vindication of their preferences. The story treats the
 *   continuity reading as one structural constraint among three sibling
 *   readings of the same kernel.
 *
 * KEY AGENTS:
 *   - Roman Curia doctrinal office (agenda_setter/institutional/identity_locked): administers and enforces the continuity hermeneutic globally
 *   - Traditionalist clergy (beneficiary/organized/identity_locked): receive vindication of traditional liturgical and doctrinal practice
 *   - Diocesan bishops (beneficiary+payer/institutional/identity_locked): gain reinforced apostolic authority but lose local implementation autonomy
 *   - Progressive theologians (payer/moderate/constrained): bear costs of doctrinal censorship and exclusion from teaching offices
 *   - Liturgical progressives (payer/moderate/constrained): bear costs of restricted innovation beyond conciliar text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Hermeneutic of Continuity (Magisterial Authority Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiological/institutional/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '1c8b65cc-5cfb-4799-a295-580277a30e1f').
narrative_ontology:cs_kernel_codification('1c8b65cc-5cfb-4799-a295-580277a30e1f', fixed_text).
narrative_ontology:cs_authority_grounding('1c8b65cc-5cfb-4799-a295-580277a30e1f', lineage).
narrative_ontology:cs_interpretation_layer_present('1c8b65cc-5cfb-4799-a295-580277a30e1f').
narrative_ontology:cs_reading_relation('1c8b65cc-5cfb-4799-a295-580277a30e1f', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('1c8b65cc-5cfb-4799-a295-580277a30e1f', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('1c8b65cc-5cfb-4799-a295-580277a30e1f', foundational, unbroken_magisterial_succession).
narrative_ontology:cs_axiom_status(unbroken_magisterial_succession, holdable).
narrative_ontology:cs_axiom_grounding('1c8b65cc-5cfb-4799-a295-580277a30e1f', unbroken_magisterial_succession, theological).
narrative_ontology:cs_axiom('1c8b65cc-5cfb-4799-a295-580277a30e1f', foundational, hermeneutic_of_continuity_obligatory).
narrative_ontology:cs_axiom_status(hermeneutic_of_continuity_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('1c8b65cc-5cfb-4799-a295-580277a30e1f', hermeneutic_of_continuity_obligatory, conventional).
narrative_ontology:cs_reference_frame('1c8b65cc-5cfb-4799-a295-580277a30e1f', unbroken_apostolic_tradition).
narrative_ontology:cs_drift_state('1c8b65cc-5cfb-4799-a295-580277a30e1f', post_conciliar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1c8b65cc-5cfb-4799-a295-580277a30e1f', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, roman_curia_doctrinal).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, liturgical_progressives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the magisterial interpretation of Vatican II through the DDF and its predecessors, issuing doctrinal notes, corrective notifications, and liturgical norms that bind the universal Church. Its institutional identity is fused with the claim of unbroken apostolic authority; exit would require abandoning the magisterial self-understanding entirely.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, roman_curia_doctrinal, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the continuity reading because it authorizes traditional liturgical forms, restricts progressive theological innovation, and vindicates their formation as continuous with the pre-conciliar Church. Their priestly identity depends on this continuity claim.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_clergy, beneficiary,
    organized, biographical, identity_locked, global).

% Receive reinforced authority as successors to the apostles within an unbroken tradition, but pay through constrained ability to implement local liturgical or pastoral adaptations that depart from Roman-curial enforcement of continuity. Their episcopal identity is permanently locked by sacramental consecration.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops, payer).

% Receive validation of their preference for Latin liturgy, traditional devotions, and doctrinal certainty through the continuity reading's mandate that Vatican II not be read as authorizing rupture with prior practice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_laity, beneficiary,
    organized, biographical, constrained, global).

% Bear the costs of the continuity reading through censures, loss of teaching licenses, exclusion from seminary appointments, and suppression of theological frameworks that rely on rupture or radical development. They remain inside the Church institutionally but their academic freedom is constrained by magisterial oversight.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% Seek further vernacularization, lay participation, and liturgical adaptation beyond the Council's explicit permissions. The continuity reading constrains them by binding implementation to pre-conciliar norms and restricting spirit-of-Vatican-II innovations that lack textual support in the conciliar documents.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, liturgical_progressives, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, roman_curia_doctrinal).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves ecclesial unity across time by binding the post-conciliar Church to pre-conciliar doctrine, preventing schism based on rupture claims, and maintaining a single interpretive authority for all Catholic teaching.
% TRANSFER_FUNCTION: Moves interpretive authority and liturgical control from local innovators and progressive theologians to the Roman Curia and traditionalist sectors of the episcopate, under the claim of unbroken magisterial custody.
% ABSENT_VOICES: Rupture-reading theologians and progressive implementers who read Vatican II as authorizing discontinuity are formally excluded from magisterial teaching office when they maintain this position; ecumenical partners who question the fact of unbroken tradition have no seat in the magisterial conversation.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, progressive liturgical and theological implementations would proliferate without Roman curial correction, traditionalist communities would lose their magisterial vindication, and the Church's self-understanding as an institution of unbroken doctrine would face immediate crisis â the global Catholic institutional order would rearrange around competing hermeneutic authorities.
% FOUNDING_PROBLEM: The post-conciliar period presented a genuine crisis of doctrinal and liturgical discontinuity, with widespread experimentation, theological innovation, and pastoral practice departing from pre-conciliar norms, threatening schism and loss of ecclesial identity.
% FOUNDING_PROBLEM_CORROBORATION: The Roman Curia and traditionalist clergy attest the problem is still live, citing ongoing progressive experimentation. Progressive theologians and some historians attest the founding problem was manufactured or exaggerated to justify centralized control; external ecclesiastical historians note genuine disruption post-1965 but dispute whether the continuity reading was the only possible response.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the continuity reading transfers substantial interpretive authority from local actors to the Roman Curia under a coordination cover of unity. Suppression (0.71) is high because the constraint's persistence depends on active enforcement: doctrinal censures, liturgical restrictions, and exclusion of dissenting theologians. Theater ratio (0.38) indicates moderate performative maintenance â some enforcement serves genuine doctrinal commitment, but a growing share performs curial authority rather than solving a live coordination problem. Accessibility collapse (0.68) is high because, once the continuity frame is accepted, rupture alternatives collapse doctrinally (they are classified as incompatible with Catholic faith). Resistance (0.58) reflects sustained opposition from progressive theologians and some national episcopates. The measurement series trace rising extraction and suppression from the immediate post-conciliar period (T=0, low enforcement) through the consolidation of curial control under John Paul II and Benedict XVI (T=36-48), with a slight moderation at T=60 under a papacy that still claims continuity while relaxing some disciplinary applications.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Roman Curia) experiences the constraint as necessary coordination to prevent ecclesial fragmentation; the engine should compute a lower effective extraction for this seat because it is declared a beneficiary with identity-locked exit. The payer seats (progressive theologians, liturgical progressives) experience the same structure as enforced extraction that constrains their vocational and intellectual activity; the engine should compute higher effective extraction for these seats due to their payer role and constrained exit. Diocesan bishops sit in the middle: they benefit from reinforced episcopal authority within tradition but pay through lost local autonomy, producing a computed directionality nearer symmetric than the Curia or the progressive targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the Roman Curia, traditionalist clergy, diocesan bishops, and traditionalist laity â all agents who receive low directionality because the constraint subsidizes their authority, identity, or preferences. Victim declarations map to progressive theologians and liturgical progressives, who receive high directionality because the constraint extracts interpretive freedom and implementation scope from them. The Curia's identity-locked exit amplifies its structural investment in continuity; progressives' constrained exit (they can leave the academy or the Church, but at high vocational cost) leaves them nearer the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading prevents mislabeling by exhibiting both a genuine coordination function (preserving ecclesial unity across a potentially fractious council) and clear asymmetric extraction (centralizing interpretive power, censuring dissent). Without the tangled-rope classification, the constraint might be read as pure rope by traditionalists (who see only the unity function) or pure snare by progressives (who see only the coercion). The structural data require both: active enforcement, declared beneficiaries, and declared victims. The founding problem status is contested, signaling that the coordination justification may be partly cover for extraction â the engine will measure this through the divergence between the claimed coordination and the authored metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Is the continuity reading a genuine recovery of the Council''s intent, or a later traditionalist reconstruction imposed on ambiguous conciliar texts?',
    'Historical-critical analysis of conciliar preparatory schemas, periti interventions, and textual redactions compared with post-conciliar magisterial interpretations.',
    'If the continuity reading is largely reconstruction, its extraction profile rises as it suppresses authentic conciliar pluralism; if it recovers genuine intent, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, empirical, 'Whether continuity is intrinsic to Vatican II or retrospectively imposed.').

omega_variable(
    enforcement_capacity_vs_consensus,
    'Does the continuity reading persist because it commands genuine theological consensus across the episcopate, or because the Roman Curia''s enforcement capacity suppresses dissenting readings?',
    'Survey of episcopal theological positions absent curial oversight; analysis of synodal deliberations where bishops speak without Roman editorial control.',
    'If consensus is genuine, classification shifts toward rope; if enforcement-dependent, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_consensus, empirical, 'Whether continuity is enforced or consented.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the continuity reading''s core axiom of unbroken magisterial succession logically foreclose the composite overdetermination reading, or can both be held as live hermeneutic options within Catholic theology?',
    'Analysis of whether magisterial teaching office has authoritatively ruled that conciliar texts are necessarily determinate and non-contradictory; examination of theologians who acknowledge textual tension while affirming continuity.',
    'If foreclosed, the composite reading is structurally excluded from the conversation; if coexisting, the continuity reading influences but does not eliminate the composite reading, altering the network topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Structural relation between continuity and composite readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vati_be_t12, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(vati_be_t24, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(vati_be_t36, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(vati_be_t48, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vati_su_t12, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(vati_su_t24, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(vati_su_t36, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement(vati_su_t48, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 48, 0.74).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The kernel vatican_ii_magisterial_authority decomposes into three structurally distinct readings: continuity (organic development within tradition), rupture (fundamental break with pre-conciliar teaching), and composite overdetermination (ambiguous compromise encoding incompatible visions). Each reading carries a distinct epsilon, beneficiary structure, and classification. This story authors the continuity reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
