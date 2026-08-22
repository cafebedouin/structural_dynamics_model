% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture Traditionalist Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the rupture_traditionalist_reading of
 *   the contested kernel 'vatican_ii_doctrinal_authority'. The reading holds
 *   that Vatican II (1962-1965) constitutes a substantive rupture with the
 *   Church's doctrinal and liturgical tradition, not an organic development.
 *   The conciliar texts (especially Dignitatis Humanae, Unitatis
 *   Redintegratio, Nostra Aetate, Gaudium et Spes, Sacrosanctum Concilium)
 *   contain studied ambiguities and positive errors — the fruit of compromise
 *   between progressive and conservative Council Fathers — which the
 *   post-conciliar implementation apparatus predictably exploited to advance
 *   a heterodox agenda. The constraint is the hermeneutic of rupture itself:
 *   the interpretive framework that reads the Council as a break, identifies
 *   the textual flaws, and treats the subsequent half-century of liturgical,
 *   doctrinal, and pastoral chaos as the necessary consequence of those
 *   flaws. This reading is structurally distinct from the
 *   rupture_progressive_reading (which values the rupture) and the
 *   continuity_reading (which denies it). The engine will compute per-seat
 *   classifications from the structural data below.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary beneficiary (organized/mobile) — gain theological authority from ambiguities
 *   - liturgical_reformers: Primary beneficiary (institutional/arbitrage) — control implementation apparatus
 *   - ecumenical_activists: Beneficiary (organized/mobile) — direct ecumenical structures
 *   - post_conciliar_episcopate: Agenda setter + beneficiary (institutional/constrained) — administer and profit from the hermeneutic
 *   - traditional_liturgy_adherents: Primary victim (powerless/identity_locked) — bear liturgical displacement
 *   - doctrinal_clarity_defenders: Victim (moderate/constrained) — bear cost of defending undermined doctrines
 *   - missionary_zeal_bearers: Victim (powerless/identity_locked) — bear evangelistic collapse
 *   - pre_conciliar_formation_communities: Excluded (powerless/trapped) — dissolved or marginalized
 *   - historical_theologian_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.65).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority — Rupture Traditionalist Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'ee5cb385-b61c-4b74-a303-b987d6aa0118').
narrative_ontology:cs_kernel_codification('ee5cb385-b61c-4b74-a303-b987d6aa0118', formalized).
narrative_ontology:cs_authority_grounding('ee5cb385-b61c-4b74-a303-b987d6aa0118', lineage).
narrative_ontology:cs_interpretation_layer_present('ee5cb385-b61c-4b74-a303-b987d6aa0118').
narrative_ontology:cs_reading_relation('ee5cb385-b61c-4b74-a303-b987d6aa0118', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ee5cb385-b61c-4b74-a303-b987d6aa0118', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee5cb385-b61c-4b74-a303-b987d6aa0118', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('ee5cb385-b61c-4b74-a303-b987d6aa0118', foundational, conciliar_texts_contain_positive_errors).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_positive_errors, holdable).
narrative_ontology:cs_axiom_grounding('ee5cb385-b61c-4b74-a303-b987d6aa0118', conciliar_texts_contain_positive_errors, empirically_contingent).
narrative_ontology:cs_axiom('ee5cb385-b61c-4b74-a303-b987d6aa0118', foundational, hermeneutic_of_rupture_is_necessary).
narrative_ontology:cs_axiom_status(hermeneutic_of_rupture_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ee5cb385-b61c-4b74-a303-b987d6aa0118', hermeneutic_of_rupture_is_necessary, deontological).
narrative_ontology:cs_axiom('ee5cb385-b61c-4b74-a303-b987d6aa0118', secondary, post_conciliar_implementation_fulfills_conciliar_errors).
narrative_ontology:cs_axiom_status(post_conciliar_implementation_fulfills_conciliar_errors, holdable).
narrative_ontology:cs_axiom_grounding('ee5cb385-b61c-4b74-a303-b987d6aa0118', post_conciliar_implementation_fulfills_conciliar_errors, empirically_contingent).
narrative_ontology:cs_reference_frame('ee5cb385-b61c-4b74-a303-b987d6aa0118', pre_conciliar_magisterial_tradition).
narrative_ontology:cs_drift_state('ee5cb385-b61c-4b74-a303-b987d6aa0118', post_conciliar_implementation_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ee5cb385-b61c-4b74-a303-b987d6aa0118', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_activists).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopate).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_adherents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity_defenders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_bearers).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_formation_communities).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, hermeneutic_of_rupture).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, textual_ambiguity_enables_heterodoxy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_documents_contain_errors).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, implementation_predictable_from_flawed_texts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leverage conciliar ambiguities to advance theological positions previously excluded; hold academic chairs, influence seminary curricula, shape episcopal appointments through theological networks. Exit means leaving the institutional church for secular academia or other traditions — possible but costly to vocation identity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians, beneficiary,
    organized, generational, mobile, global).

% Control the implementation apparatus (Congregation for Divine Worship, ICEL, national liturgical commissions); translate textual ambiguities into binding liturgical norms. Collect institutional authority, publishing revenue, consultancy roles. Exit options include moving between curial posts, academia, and NGO sectors — high arbitrage.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_reformers, beneficiary,
    institutional, generational, arbitrage, global).

% Use conciliar opening to non-Catholic Christians as mandate for doctrinal convergence; direct ecumenical dialogue structures, joint commissions, shared prayer initiatives. Funding flows from institutional budgets and private foundations. Exit to interfaith NGOs or secular diplomacy is straightforward.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_activists, beneficiary,
    organized, biographical, mobile, global).

% Administer the constraint: interpret texts, enforce liturgical norms, govern seminary formation, appoint pastors. Benefit from expanded episcopal authority over a fluid tradition. Exit is constrained — resignation is rare, transfer limited, retirement the only normal exit; identity fused with office.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopate, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopate, beneficiary).

% Bear the cost of liturgical displacement: loss of formative worship, marginalization in parishes, canonical irregularity for preferring pre-conciliar rites. Exit means either submitting to the new rite (identity fracture) or moving to fringe communities (canonical peril, social isolation). Identity is constituted through the traditional liturgy — leaving it is leaving oneself.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_adherents, payer,
    powerless, biographical, identity_locked, global).

% Bear the cost of teaching and defending doctrines that conciliar ambiguities undermine (papal infallibility, real presence, sacramental causality, moral absolutes). Invest in apologetics, publishing, alternative formation structures. Exit options: withdraw into parallel institutions (SSPX, FSSP, independent chapels) — costly but structured; or acquiesce to ambiguity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_clarity_defenders, payer,
    moderate, generational, constrained, global).

% Bear the cost of evangelistic collapse: conciliar texts on religious liberty and non-Christian religions read as undermining the urgency of conversion; missionary orders lose vocations, funding, institutional support. Identity fused with the mandate to convert — the constraint redefines their purpose out of existence. Exit means abandoning the vocation that constitutes them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_bearers, payer,
    powerless, biographical, identity_locked, global).

% Seminary faculties, religious orders, lay movements formed in the pre-conciliar framework — structurally excluded from governance after the Council. Their institutions were dissolved, reformed, or marginalized. No voice in the hermeneutic that judges them. Exit is trapped: too old to restart, too identified to switch, too dispersed to organize effectively.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_formation_communities, excluded,
    powerless, generational, trapped, regional).

% Studies the conciliar texts, their drafting history (acta synodalia), the implementation trajectory, and the reception contests. Sees the structural ambiguity as a negotiated compromise between Council Fathers, not a deliberate error. Tracks how each reading mobilizes different textual evidence. Neither collects nor pays — analyzes.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, historical_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council purported to coordinate the Church's engagement with modernity: religious liberty, ecumenism, liturgical participation, episcopal collegiality. The traditionalist reading argues this coordination function was a pretext — the texts were deliberately or negligently ambiguous, enabling a transfer of authority from the perennial magisterium to a progressive implementation apparatus.
% TRANSFER_FUNCTION: Moves doctrinal authority and liturgical determinacy from the fixed magisterial tradition (pre-conciliar encyclicals, councils, catechisms) to a living implementation apparatus (post-conciliar popes, curial congregations, episcopal conferences, theological experts). The transfer is effected by reading ambiguities as openings rather than errors.
% ABSENT_VOICES: The Council Fathers who opposed the ambiguous formulations (e.g., Cardinal Ottaviani, Cardinal Siri, Archbishop Lefebvre) — their interventions are recorded in acta synodalia but excluded from the authoritative reception. The laity of the 1960s-70s who experienced the liturgical rupture without consultation. Future generations who inherit a fragmented tradition — they cannot object to what formed them.
% DISAPPEARANCE_RATIONALE: If the rupture reading's authority vanished overnight, the hermeneutic of continuity would become the sole authorized reading; traditional liturgical communities would be normalized; doctrinal ambiguity would be resolved by reference to pre-conciliar magisterium; missionary structures would be rebuilt on the pre-conciliar model. The entire post-conciliar institutional configuration (liturgical, catechetical, canonical, ecumenical) would require restructuring.
% FOUNDING_PROBLEM: The Church's perceived irrelevance to the modern world: loss of intellectual credibility, missionary stagnation, liturgical passivity, ecumenical isolation, episcopal subordination to Rome. The Council was convoked to address these by 'updating' (aggiornamento) the Church's self-presentation and structures.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and the post-conciliar episcopate attest the founding problem remains live (secularization continues, new frontiers emerge). Traditionalist theologians (e.g., de Mattei, Rorate Caeli contributors) and pre-conciliar formation communities attest the problem was misdiagnosed — the Church's crisis was not irrelevance but infidelity, and the Council's remedy worsened it. The acta synodalia (published 1990s onward) corroborate that the Council Fathers themselves disputed whether the problem was external (modernity) or internal (crisis of faith).
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the hermeneutic of rupture transfers authority from a fixed tradition to a living, controllable implementation apparatus — the beneficiaries control what the texts mean, and the victims pay the cost of that control. Suppression is substantial (0.65) because the constraint persists through canonical enforcement (liturgical norms, doctrinal policing, episcopal appointments, seminary control) and the victims' exit options are identity-locked or trapped. Theater ratio is moderate (0.42): the coordination function (engagement with modernity) is real but increasingly performative — the actual function is maintaining the interpretive monopoly. Accessibility collapse is moderate (0.48): alternative hermeneutics (continuity, progressive rupture) exist and are argued, but the institutional enforcement makes the rupture reading operationally dominant. Resistance is significant (0.58): traditionalist communities, some episcopal resistance, and the continuity reading's intellectual defense constitute active pushback. The measurement series track the constraint's intensification from the Council's close (1965) through the implementation decades to the present.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (post-conciliar episcopate, liturgical reformers) experience this constraint as coordination — they built the implementation apparatus to solve the founding problem. The payer seats (traditional liturgy adherents, doctrinal clarity defenders, missionary zeal bearers) experience it as extraction — their formative structures were dismantled, their identities declared obsolete, their vocations hollowed out. The engine computes this divergence from the structural power/exit data; the authored claim (tangled_rope) reflects the structural reality that both coordination and extraction are present and actively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar episcopate and liturgical reformers are structural beneficiaries (d near 0.0) — they administer the hermeneutic and collect authority from it. Progressive theologians and ecumenical activists are beneficiaries (d ~0.1-0.2) — they gain institutional access and funding. Traditional liturgy adherents and missionary zeal bearers are full targets (d near 1.0) — identity-locked, the constraint redefines their constitutive purpose as error. Doctrinal clarity defenders are high targets (d ~0.8) — constrained exit, bearing the cost of defending what the hermeneutic dissolves. Pre-conciliar formation communities are trapped (d ~0.9) — no voice, no exit. The historical theologian observer sits at d=0.5 (analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Church's relevance to modernity) is contested: progressives say it's live, traditionalists say it was misdiagnosed and the remedy worsened the disease. The constraint persists not because the founding problem is solved, but because the implementation apparatus (episcopate, curia, theological establishment) benefits from the hermeneutic's ambiguity — it authorizes their authority. Mandatrophy is unresolved: the arrangement has outlived its original justification (if the justification was ever sincere) and now serves the interests of its administrators. The 'spirit of the Council' functions as a blank check for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_intent_vs_negligence,
    'Are the conciliar textual ambiguities deliberate (progressive Fathers knowingly inserting exploitable language) or negligent (poor drafting under time pressure, translation issues)?',
    'Detailed acta synodalia analysis of specific passages: track intervention texts, voting records, periti influence on final drafts. Compare Latin originals to vernacular translations used in implementation.',
    'If deliberate, the constraint is a designed snare — extraction was the point. If negligent, it is a tangled_rope where coordination failed and extraction opportunistically filled the gap. Affects claimed_type and mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intent_vs_negligence, empirical, 'Whether textual ambiguities were engineered or emergent.').

omega_variable(
    continuity_hermeneutic_viability,
    'Can the continuity reading (Benedict XVI''s ''hermeneutic of reform in continuity'') coherently resolve the textual tensions the rupture reading identifies, or does it require ignoring the plain sense of key passages?',
    'Systematic passage-by-passage comparison: does the continuity reading''s interpretive rules yield determinate orthodox readings of Dignitatis Humanae 2, Unitatis Redintegratio 3, Nostra Aetate 3, Gaudium et Spes 22, without importing extrinsic magisterial authority?',
    'If continuity reading succeeds, the rupture reading''s claim that ambiguities are ''errors'' collapses — they are resolvable tensions. If it fails, the rupture reading''s textual critique stands and the constraint''s extraction is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_hermeneutic_viability, conceptual, 'Whether the rival hermeneutic can internally resolve the textual ambiguities.').

omega_variable(
    composite_overdetermination_relation,
    'Does the composite_overdetermination_reading (multiple distinct shifts packaged as one) foreclose, coexist with, or influence this rupture_traditionalist_reading?',
    'Structural analysis: if the composite reading shows that liturgical, ecumenical, ecclesiological, and political changes have independent causal histories, does that undermine the rupture reading''s claim of a unified conciliar break? Or does it explain why the rupture reading''s single-constraint model feels extractive?',
    'If composite forecloses rupture, this constraint decomposes into multiple constraints. If composite coexists, this constraint remains valid as one reading of the doctrinal-authority kernel. If composite influences, it modifies the rupture reading''s ε without replacing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_overdetermination_relation, conceptual, 'Relationship between this reading and the composite_overdetermination_reading.').

omega_variable(
    identity_lock_mechanism_traditionalists,
    'What specific identity-fusion mechanism binds traditional_liturgy_adherents and missionary_zeal_bearers to the constraint — liturgical formation, vocational self-concept, ecclesial identity, or all three?',
    'Sociological study of traditionalist communities: measure correlation between liturgical practice, vocational persistence, and ecclesial loyalty. Track exit trajectories of those who leave.',
    'If identity lock is primarily liturgical, Summorum Pontificum (2007) and Traditionis Custodes (2021) are the key suppression levers. If vocational, the constraint extracts by redefining missionary identity. If ecclesial, the constraint operates through canonical status. Affects directionality derivation and suppression measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalists, empirical, 'Mechanism of identity lock for primary victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t1995, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2007, 0.4).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t2015, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tr_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t1995, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2007, 0.72).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t2015, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_be_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.1).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t1965, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t1985, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t1995, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2007, 0.6).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t2015, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_su_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the kernel vatican_ii_doctrinal_authority. The rupture_traditionalist_reading and rupture_progressive_reading share high ε on doctrinal change but invert the valence. The continuity_reading claims ε ≈ 0 (organic development). The composite_overdetermination_reading decomposes the single constraint into multiple distinct structural shifts. All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
