% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture Progressive Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models the 'rupture progressive' reading of Vatican
 *   II's doctrinal authority: the Council constitutes a necessary break with
 *   pre-conciliar rigidity, and its 'spirit' authorizes ongoing reform beyond
 *   the textual limits of the sixteen documents. The reading treats religious
 *   freedom (Dignitatis Humanae) as a reversal of the Syllabus of Errors,
 *   collegiality as a structural shift in episcopal ontology, and liturgical
 *   reform as an irreversible pastoral turn. Textual ambiguities (e.g.,
 *   'subsistit in,' 'fully conscious and active participation') are read as
 *   intentional openings for development. Post-conciliar implementation (the
 *   Novus Ordo, the 1983 Code, episcopal conferences) is treated as the
 *   authentic realization of conciliar intent. The constraint operates
 *   through canonical enforcement (canon 752, 838), episcopal appointment
 *   criteria, theological faculty hiring, and the marginalization of the 1962
 *   Missal — all justified as fidelity to the Council's deeper trajectory.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary agenda_setters and beneficiaries (organized/mobile) — shape the hermeneutic
 *   - reform_bishops: Institutional agenda_setters (institutional/constrained) — implement reform locally
 *   - laity_benefiting_from_reforms: Beneficiaries (moderate/constrained) — experience tangible pastoral gains
 *   - traditionalist_clergy: Primary payers (moderate/identity_locked) — bear vocational and epistemic costs
 *   - traditionalist_laity: Payers (powerless/identity_locked) — lose formative worship and community
 *   - pre_conciliar_communities: Payers/excluded (powerless/trapped) — charisms delegitimized
 *   - roman_curia: Institutional agenda_setter (institutional/arbitrage) — administers authoritative interpretation
 *   - ecumenical_partners: Observers (organized/analytical) — assess credibility of commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority — Rupture Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '129c0333-afba-495b-a2bb-c7b963c2b7bc').
narrative_ontology:cs_kernel_codification('129c0333-afba-495b-a2bb-c7b963c2b7bc', formalized).
narrative_ontology:cs_authority_grounding('129c0333-afba-495b-a2bb-c7b963c2b7bc', lineage).
narrative_ontology:cs_interpretation_layer_present('129c0333-afba-495b-a2bb-c7b963c2b7bc').
narrative_ontology:cs_reading_relation('129c0333-afba-495b-a2bb-c7b963c2b7bc', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('129c0333-afba-495b-a2bb-c7b963c2b7bc', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('129c0333-afba-495b-a2bb-c7b963c2b7bc', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('129c0333-afba-495b-a2bb-c7b963c2b7bc', foundational, conciliar_spirit_authorizes_development_beyond_letter).
narrative_ontology:cs_axiom_status(conciliar_spirit_authorizes_development_beyond_letter, holdable).
narrative_ontology:cs_axiom_grounding('129c0333-afba-495b-a2bb-c7b963c2b7bc', conciliar_spirit_authorizes_development_beyond_letter, instrumental).
narrative_ontology:cs_axiom('129c0333-afba-495b-a2bb-c7b963c2b7bc', foundational, religious_freedom_reverses_syllabus_of_errors).
narrative_ontology:cs_axiom_status(religious_freedom_reverses_syllabus_of_errors, holdable).
narrative_ontology:cs_axiom_grounding('129c0333-afba-495b-a2bb-c7b963c2b7bc', religious_freedom_reverses_syllabus_of_errors, empirically_contingent).
narrative_ontology:cs_axiom('129c0333-afba-495b-a2bb-c7b963c2b7bc', secondary, post_conciliar_implementation_is_authentic_realization).
narrative_ontology:cs_axiom_status(post_conciliar_implementation_is_authentic_realization, holdable).
narrative_ontology:cs_axiom_grounding('129c0333-afba-495b-a2bb-c7b963c2b7bc', post_conciliar_implementation_is_authentic_realization, conventional).
narrative_ontology:cs_reference_frame('129c0333-afba-495b-a2bb-c7b963c2b7bc', conciliar_rupture_as_reform).
narrative_ontology:cs_drift_state('129c0333-afba-495b-a2bb-c7b963c2b7bc', post_traditionis_custodes, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('129c0333-afba-495b-a2bb-c7b963c2b7bc', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_benefiting_from_reforms).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_communities).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_freedom_as_fundamental_right).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, collegiality_as_constitutive_of_episcopacy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, liturgical_reform_as_pastoral_necessity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenism_as_obligation_not_option).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape the hermeneutic of 'spirit of the Council' through academic positions, curial consultancies, and episcopal advisory roles. Their interpretive authority derives from being recognized as authentic conciliar interpreters. Gain professional recognition and institutional influence; exit to secular academia is viable but reduces ecclesiastical authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, beneficiary).

% Implement conciliar reform through diocesan policy, liturgical adaptation, and pastoral restructuring. Depend on curial goodwill for appointments and governance latitude. Benefit from expanded pastoral flexibility and ecumenical credibility; exit would mean resignation or canonical marginalization.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_bishops, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_bishops, beneficiary).

% Experience vernacular liturgy, lay ministries, religious liberty, and ecumenical openness as tangible improvements. Their situation depends on local implementation fidelity; exit to other Christian communities or secular life is possible but involves community loss.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_benefiting_from_reforms, beneficiary,
    moderate, biographical, constrained, global).

% Face canonical pressure to adopt reformed liturgy and accept doctrinal developments they experience as rupture. Their priestly identity is fused with the pre-conciliar form; exit means laicization or schism, both experienced as vocational death. Bear costs of marginalization, restricted faculties, and epistemic dismissal.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    moderate, biographical, identity_locked, global).

% Lose access to the formative worship and catechesis that constituted their faith identity. Told their attachment is 'nostalgia' or 'rigidity.' Exit to traditionalist societies (FSSP, ICKSP, SSPX) is geographically constrained and socially costly; exit from the Church entirely severs sacramental identity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_laity, payer,
    powerless, biographical, identity_locked, local).

% Monasteries, parishes, and lay associations formed under the old order find their charisms and liturgical life delegitimized by the reform's normative claim. Structurally excluded from the 'spirit of the Council' conversation; their survival depends on episcopal toleration that can be withdrawn.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_communities, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_communities, excluded).

% Administers the authoritative interpretation through dicasteries (CDF, Divine Worship, Bishops). Holds the canonical machinery that determines which readings are 'authentic.' Can shift hermeneutical direction (e.g., Summorum Pontificum vs. Traditionis Custodes) but bears institutional risk of schism if pressure is miscalibrated.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_curia, agenda_setter,
    institutional, generational, arbitrage, universal).

% Protestant and Orthodox observers assess whether conciliar commitments (ecumenism, religious freedom) are irreversibly instantiated or rhetorically contingent. Their reception affects Catholic credibility in formal dialogues; they do not vote on internal hermeneutics.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partners, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimated hermeneutic for updating Catholic teaching and practice in response to modernity: religious freedom, collegiality, liturgical vernacular, ecumenical engagement. Solves the coordination problem of how a claims-to-be-universal institution adapts without admitting doctrinal mutability.
% TRANSFER_FUNCTION: Moves interpretive authority from the fixed textual corpus (pre-conciliar magisterium, conciliar documents read literally) to the living 'spirit' discerned by progressive theologians and implementing bishops. Transfers the cost of adaptation onto communities whose identity was formed by the prior stable form.
% ABSENT_VOICES: The global South laity and clergy who experienced the reform as cultural imposition rather than liberation — their reception was assumed to be positive but was never consulted. Also absent: the pre-conciliar magisterium itself (Pius XII, Pius X) whose teaching is treated as superseded rather than developed.
% DISAPPEARANCE_RATIONALE: If the 'spirit of the Council' hermeneutic vanished, the post-conciliar liturgical, ecumenical, and religious-freedom frameworks would lose their authoritative grounding. Dioceses would revert to textual literalism or fragment into competing local hermeneutics. The Curia would need a new unifying interpretive principle.
% FOUNDING_PROBLEM: Pre-conciliar Catholicism had become culturally unintelligible and pastorally ineffective in the modern world: Latin liturgy alienated the faithful, religious coercion discredited the Church before secular states, ecumenical refusal isolated Catholicism from other Christians, clericalism stifled lay vocation.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians (Rahner, Schillebeeckx, Congar) and reform bishops (Lercaro, Suenens) attested the problem was acute at the Council. Traditionalist critics (Lefebvre, Davies, later Ratzinger as prefect) attested the problem was misdiagnosed — the crisis was not the tradition but its abandonment. Sociological data (Mass attendance, vocations, Catholic identity metrics) from 1965–present is cited by both sides as corroborating their reading.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the 'spirit' hermeneutic extracts interpretive authority from fixed texts and transfers it to a living magisterial class, while imposing the costs of adaptation on communities whose identity was constituted by the prior form. Suppression (0.55) is moderate — the constraint does not rely on total coercion (traditionalist communities persist at the margins) but uses canonical pressure, appointment control, and epistemic dismissal to maintain hermeneutical hegemony. Theater ratio (0.22) is low-moderate: the coordination function (genuine adaptation to modernity) is real, but a growing share of enforcement activity (Traditionis Custodes, doctrinal notifications) defends the hermeneutic's monopoly rather than the faithful's good. Accessibility collapse (0.45) is partial — alternative hermeneutics (continuity, traditionalist rupture) remain live and articulate. Resistance (0.62) is substantial — the traditionalist movement, the 'reform of the reform' project, and the global South's reception all contest the progressive reading's monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive_theologians and reform_bishops seats, the constraint computes as rope/coordination: a genuine solution to the Church's modernity problem, actively maintained because the alternative is irrelevance. From the traditionalist_clergy and traditionalist_laity seats, it computes as snare: an enforced rupture that extracts their formative identity under cover of 'development.' The roman_curia seat experiences it as a tactical management problem — the hermeneutic must be defended to prevent schism, but its enforcement generates the very traditionalist resistance that threatens unity. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and reform bishops are structural beneficiaries (d near 0.0-0.2): they collect interpretive authority, career advancement, and pastoral latitude. Laity benefiting from reforms sit near symmetric (d ~0.4-0.5): genuine coordination gains, diffuse indirect costs. Traditionalist clergy are identity-locked targets (d ~0.8-0.9): their priestly self-concept is fused with the pre-conciliar form; exit is vocational death. Traditionalist laity are similarly identity-locked (d ~0.85) but with less institutional power. Pre-conciliar communities are trapped (d ~0.9): no voice in the hermeneutic, survival depends on toleration. The Curia sits at d ~0.3: benefits from institutional control but bears schism risk. Ecumenical partners are analytical (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by acknowledging BOTH the genuine coordination function (Catholicism's adaptation to religious freedom, ecumenism, vernacular worship — without which the Church would have lost credibility in the modern world) AND the asymmetric extraction (the cost of adaptation borne by communities whose identity was the prior form). The 'spirit' hermeneutic is not pure coordination (it suppresses alternatives) nor pure extraction (it solves a real adaptation problem). It is tangled_rope: a hybrid that requires active enforcement (canonical, episcopal, academic) to maintain the hermeneutic monopoly while delivering genuine pastoral goods to the majority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_monopoly_vs_pluralism,
    'Is the progressive reading''s claim to exclusive hermeneutic authority structurally necessary for the coordination function, or is it an extractive overlay on a genuinely pluralistic reception?',
    'Counterfactual: if the Curia formally recognized the continuity_reading and rupture_traditionalist_reading as legitimate hermeneutical options (as Summorum Pontificum partially did), would the coordination function (religious freedom, ecumenism, vernacular) collapse or persist?',
    'If coordination persists without monopoly, the hermeneutic enforcement is pure extraction; if coordination collapses, the monopoly is structurally necessary and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_monopoly_vs_pluralism, conceptual, 'Whether the progressive hermeneutic''s enforcement monopoly is coordination-necessary or extractive').

omega_variable(
    identity_lock_mechanism_traditionalists,
    'Is the traditionalist identity-lock structural (canonical barriers, geographic isolation) or internalized (theological conviction that exit is apostasy)?',
    'Post-exit trajectory study: traditionalist clergy/laity who joined FSSP/ICKSP (canonically regular) vs. SSPX (irregular) vs. Orthodoxy vs. secular exit — measure suppression persistence after structural barriers are removed.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. This affects directionality derivation for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalists, empirical, 'Structural vs. internalized suppression for identity-locked traditionalist agents').

omega_variable(
    global_south_reception_ambiguity,
    'Does the progressive reading''s claim to represent ''the Church''s adaptation to modernity'' hold in the global South, where inculturation often means retrieving pre-conciliar forms (Latin, chant, ad orientem) as counter-cultural?',
    'Synodal listening data from African, Asian, and Latin American episcopal conferences on liturgical and catechetical preferences; comparison of vocations trends in progressive vs. traditionalist orders globally.',
    'If the global South reception contradicts the progressive reading''s universal claim, the constraint''s coordination function is regionally partial and its extraction from Southern traditionalist communities is structurally unacknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_reception_ambiguity, empirical, 'Whether the progressive reading''s coordination claim is globally valid or Northern-centric').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_tr_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_be_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_progressive_reading_su_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes 'Vatican II doctrinal authority' into four readings with distinct ε values and beneficiary/victim structures. The rupture_progressive_reading claims high ε on doctrinal change (religious freedom as Syllabus reversal) and treats post-conciliar implementation as authentic realization. The continuity_reading claims near-zero ε (organic development). The rupture_traditionalist_reading claims high ε but reads it as error. The composite_overdetermination_reading claims the label 'Vatican II' conflates distinct structural shifts. All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, institutional, 0.3).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, organized, 0.15).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, moderate, 0.85).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
