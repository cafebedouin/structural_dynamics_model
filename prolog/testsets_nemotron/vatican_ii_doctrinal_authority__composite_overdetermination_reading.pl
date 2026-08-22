% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Composite Overdetermination — Convergent Structural Changes Packaged as Unified Reform
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) is conventionally treated as a single event —
 *   either a rupture or a continuity — but structurally it is a convergence
 *   of four distinct changes: (1) liturgical vernacularization and reform,
 *   (2) ecumenical engagement and religious freedom, (3) ecclesiological
 *   shift to collegiality and communio, (4) political reorientation toward
 *   modernity and human rights. These were negotiated by different conciliar
 *   coalitions, have different extractiveness profiles, and are enforced by
 *   different mechanisms post-conciliar. The 'composite overdetermination'
 *   reading holds that the Council's texts deliberately overdetermine key
 *   formulations (e.g., 'subsistit in', 'seeds of the Word') to hold the
 *   reform coalition together — the ambiguities are not bugs but the
 *   structural glue. This constraint story models the COMPOSITE PACKAGING as
 *   the constraint: the demand that all four components be accepted or
 *   rejected as a unified 'spirit of the Council' rather than evaluated on
 *   their independent merits.
 *
 * KEY AGENTS:
 *   - progressive_episcopate: Primary beneficiary (institutional/biographical) — captures reform agenda, controls implementation
 *   - traditionalist_laity: Primary victim (organized/biographical) — bears liturgical/ecclesiological disruption, constrained exit
 *   - pre_conciliar_formation_clergy: Victim (moderate/biographical) — formation rendered obsolete, identity disruption
 *   - liturgical_reform_movement: Beneficiary (organized/biographical) — institutionalizes vernacular liturgy as career/life project
 *   - ecumenical_establishment: Beneficiary (institutional/generational) — gains structural foothold for interfaith infrastructure
 *   - reformist_theologians: Beneficiary (moderate/biographical) — professional validation, academic positions
 *   - traditionalist_bishops: Victim (powerful/biographical) — marginalized in governance, see authority structure inverted
 *   - traditionalist_religious_orders: Victim (organized/biographical) — vocations collapse, charism questioned
 *   - magisterium_post_conciliar: Agenda setter (institutional/civilizational) — administers composite enforcement
 *   - historical_theologian: Observer (analytical/civilizational) — sees component independence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.38).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Composite Overdetermination — Convergent Structural Changes Packaged as Unified Reform").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '1b25f944-cc36-4bc9-900c-cba0b02d7d58').
narrative_ontology:cs_kernel_codification('1b25f944-cc36-4bc9-900c-cba0b02d7d58', fixed_text).
narrative_ontology:cs_authority_grounding('1b25f944-cc36-4bc9-900c-cba0b02d7d58', lineage).
narrative_ontology:cs_interpretation_layer_present('1b25f944-cc36-4bc9-900c-cba0b02d7d58').
narrative_ontology:cs_reading_relation('1b25f944-cc36-4bc9-900c-cba0b02d7d58', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b25f944-cc36-4bc9-900c-cba0b02d7d58', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b25f944-cc36-4bc9-900c-cba0b02d7d58', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('1b25f944-cc36-4bc9-900c-cba0b02d7d58', foundational, conciliar_texts_are_deliberately_overdetermined).
narrative_ontology:cs_axiom_status(conciliar_texts_are_deliberately_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('1b25f944-cc36-4bc9-900c-cba0b02d7d58', conciliar_texts_are_deliberately_overdetermined, conventional).
narrative_ontology:cs_axiom('1b25f944-cc36-4bc9-900c-cba0b02d7d58', foundational, continuity_rupture_debate_is_category_error).
narrative_ontology:cs_axiom_status(continuity_rupture_debate_is_category_error, holdable).
narrative_ontology:cs_axiom_grounding('1b25f944-cc36-4bc9-900c-cba0b02d7d58', continuity_rupture_debate_is_category_error, conventional).
narrative_ontology:cs_axiom('1b25f944-cc36-4bc9-900c-cba0b02d7d58', secondary, component_independent_extractiveness).
narrative_ontology:cs_axiom_status(component_independent_extractiveness, holdable).
narrative_ontology:cs_axiom_grounding('1b25f944-cc36-4bc9-900c-cba0b02d7d58', component_independent_extractiveness, empirically_contingent).
narrative_ontology:cs_reference_frame('1b25f944-cc36-4bc9-900c-cba0b02d7d58', conciliar_coalition_unity).
narrative_ontology:cs_drift_state('1b25f944-cc36-4bc9-900c-cba0b02d7d58', post_synodal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1b25f944-cc36-4bc9-900c-cba0b02d7d58', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_episcopate).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reform_movement).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, reformist_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, pre_conciliar_formation_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_bishops).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_religious_orders).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, collegiality_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, religious_freedom_principle).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_vernacularization).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the conciliar implementation apparatus: episcopal conferences, liturgical commissions, catechetical offices, seminary formation. They collect the institutional prestige of 'being the Council's true interpreters' and direct resources to aligned movements. Exit is arbitrage-grade: they could defect to traditionalist positions but would lose all accumulated institutional capital.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_episcopate, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the lived disruption of liturgical vernacularization, ecclesiological reorientation, and pastoral practices that treat their formation as obsolete. Their exit is identity-locked: the traditional Mass and pre-conciliar catechesis constitute their Catholic self-understanding; leaving the Church is existentially costly, staying means internal exile. They organize (FSSPX, traditionalist associations, online communities) but remain structurally targeted by the composite's enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_laity, payer,
    organized, biographical, identity_locked, global).

% Ordained and formed in the pre-conciliar framework (Latin liturgy, Thomistic manuals, juridical ecclesiology). Their professional identity and spiritual formation are rendered discontinuous by the composite. Exit options are constrained: too old for re-formation, too invested for laicization, too loyal for schism. They become 'living anachronisms' in their own dioceses.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, pre_conciliar_formation_clergy, payer,
    moderate, biographical, constrained, global).

% Consensus of periti, composers, pastoral musicians, and liturgists who built careers on the vernacular reform. They benefit from the composite's enforcement of the Novus Ordo as exclusive ordinary form. Exit is mobile: their skills transfer to academic, publishing, and consultancy roles even if the composite weakened.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reform_movement, beneficiary,
    organized, biographical, mobile, global).

% Pontifical Council for Promoting Christian Unity, World Council of Churches Catholic delegates, national ecumenical commissions. The composite's religious freedom and ecumenical components gave them structural mandate and funding. Exit is arbitrage-grade: the ecumenical infrastructure is now embedded in global Christianity; they would retain position even if the composite framing changed.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Rahner, Küng, Schillebeeckx, Congar, Ratzinger (early), and their institutional heirs. The composite validated their theological methods (historical-critical, ressourcement) and secured academic chairs. Exit is mobile: theological prestige transfers across ecclesiastical boundaries; many already operate in secular academia.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, reformist_theologians, beneficiary,
    moderate, biographical, mobile, global).

% Bishops like Lefebvre, Castro Mayer, Burke, Schneider — they hold episcopal authority but are disciplined by the composite's enforcement (canonical irregularity, restricted faculties, exclusion from synods). Their power is real but constrained: they can ordain, govern dioceses, attract vocations, but operate under permanent suspicion. Exit is constrained: schism (Lefebvre) carries excommunication; silence carries irrelevance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_bishops, payer,
    powerful, biographical, constrained, regional).

% Orders founded on pre-conciliar charisms (e.g., traditional Franciscans, Dominicans, contemplatives using 1962 books). The composite's liturgical and ecclesiological components undermine their founding identity. Vocations collapse when the charism is questioned. Exit is identity-locked: the order IS the charism; reforming the charism dissolves the order. They bear diffuse costs across generations.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_religious_orders, payer,
    organized, generational, identity_locked, global).

% Paul VI through Francis: the papal office administers the composite, interprets its texts, and enforces its unity. It is both author and prisoner of the composite — it cannot dissolve the package without delegitimizing its own authority (which rests on 'faithful implementation'). Exit is analytical: the magisterium can reinterpret but cannot externally exit the constraint it embodies.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterium_post_conciliar, agenda_setter,
    institutional, civilizational, analytical, universal).

% Scholars (O'Malley, Hahnenberg, Gaillardetz, traditionalist historians) who study the Council as historical event. They see the four components' independent negotiation histories, voting coalitions, and reception trajectories. They neither collect nor pay; they map the structure this constraint story models.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, historical_theologian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite package solved a genuine multi-front coordination problem: how to simultaneously (1) make liturgy accessible without fracturing rite unity, (2) engage other Christians without relativizing truth claims, (3) redefine Church-world relations without surrendering mission, (4) restructure episcopal authority without dissolving papal primacy. The overdetermined formulations ('subsistit in', 'seeds of the Word', 'active participation') were the coordination mechanism — they held the coalition together across theological differences.
% TRANSFER_FUNCTION: The composite transfers interpretive authority and institutional resources from the pre-conciliar framework (Roman Curia central control, Latin liturgy, juridical ecclesiology, confessional state model) to the post-conciliar coalition (episcopal conferences, vernacular liturgy, communio ecclesiology, religious freedom advocacy). It transfers liturgical agency from clergy to laity ('active participation'), ecumenical initiative from center to periphery, and political theology from integralism to human rights. The transfer is asymmetric: traditionalist positions lose institutional footing; progressive positions gain it.
% ABSENT_VOICES: The 'silent majority' of 1960s Catholics who experienced the transition as disorientation without consultation — they were not at the Council, not in the periti groups, not in the implementation commissions. Their absence is structural: the composite was negotiated by elites (bishops, periti, curial officials) and imposed as reception. Also absent: Eastern Catholic voices (their liturgical/ecclesiological traditions were marginalized by Latin-centric reform), Global South bishops (collegiality promised voice but curial implementation recentralized), and lay women (excluded from periti/minority roles until late).
% DISAPPEARANCE_RATIONALE: If the composite packaging vanished overnight — meaning each component (liturgy, religious freedom, ecumenism, collegiality) were evaluated independently — the coalitions would fracture. Progressive bishops would lose the liturgical lever that enforces ecclesiological conformity. Traditionalist laity would lose the single target that unifies their resistance. The magisterium would lose the hermeneutic that legitimates its authority. The four component constraints would float free, each finding its own equilibrium (some rope, some snare, some mountain). The world rearranges because the composite IS the structure holding the post-conciliar Church together.
% FOUNDING_PROBLEM: How can the Catholic Church engage the modern world (secularism, pluralism, scientific rationality, human rights discourse) without either retreating into ghettoization or dissolving into accommodation? The Council's four components were the negotiated answer: vernacular liturgy for accessibility, religious freedom for pluralism, ecumenism for Christian unity, collegiality for governance reform. The composite packaging was the political technology that made the package pass the Council's voting thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians (O'Malley, Gaillardetz) attest the problem is live — modernity has mutated into post-modernity, new engagement needed. Traditionalist scholars (de Mattei, Kwasniewski) attest the problem was misdiagnosed — the Church should have converted the world, not accommodated it. Independent historians (Chenu's periti archives, Alberigo's five-volume history) corroborate the negotiated, coalition-dependent nature of the composite. No single party's attestation is definitive; the contested status is the structural fact.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the composite packaging extracts assent to components a party might reject (e.g., a bishop accepting collegiality but resisting liturgical reform) by making the package the unit of loyalty. Suppression (0.38) is moderate: dissent is managed through canonical irregularity declarations, faculty restrictions, and appointment control — not total silencing. Theater ratio (0.55) is high: by 2025, a majority of 'conciliar implementation' activity is performative maintenance of the coalition (synodal processes, commemorative documents) rather than functional coordination. Accessibility collapse (0.45) is partial: component-level alternatives exist (extraordinary form, Eastern Catholic praxis, Orthodox ecclesiology) but are marginalized by the composite frame. Resistance (0.58) is significant: traditionalist movements, sedevacantism, and 'reform of the reform' demonstrate ongoing contestation. The measurement series shows extraction rising 1965-1985 as implementation hardens, then stabilizing; theater rising monotonically as functional coordination atrophies; suppression spiking 1965-1985 then flat — the enforcement infrastructure matured early.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive episcopate and reformist theologians are structural beneficiaries (d ≈ 0.15-0.25): they collect institutional power, academic prestige, and control of the hermeneutic. Traditionalist laity and pre-conciliar clergy are structural targets (d ≈ 0.75-0.85): they bear the disruption of identity, liturgy, and formation with constrained exit (schism costs are existential). Traditionalist bishops are powerful but targeted (d ≈ 0.65): they have institutional platform but are disciplined by the composite's enforcement. Liturgical reform movement and ecumenical establishment are organized beneficiaries (d ≈ 0.2): they built careers/institutions on specific components. Magisterium is agenda_setter with analytical exit (d ≈ 0.3): it administers but is also constrained by the composite's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1965) was: 'How does the Church engage modernity without losing identity?' — a genuine coordination problem across four fronts. The composite package solved it by overdetermination. By 2025, the modernity-engagement problem has mutated (secularism, not modernity, is the context), and the composite's components have diverged: liturgy shows high extraction/theater, religious freedom shows low extraction, ecumenism shows moderate coordination, collegiality shows contested implementation. The mandate has atrophied differentially across components. The composite_overdetermination_reading prevents mislabeling: it is not a snare (there is real coordination in religious freedom/ecumenism), not a rope (liturgical enforcement is extractive), not a mountain (it is constructed, not natural), not a scaffold (no sunset), not a piton (active enforcement persists). Tangled rope captures the hybrid: genuine coordination on some components, asymmetric extraction on others, held by active enforcement of the package.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a single reading of the contested kernel ''vatican_ii_doctrinal_authority'', and does the composite_overdetermination_reading instantiate a distinct constraint with its own ε from sibling readings?',
    'Per Rule 1: generate only this reading as a clean ε-invariant constraint. The other readings (continuity_reading, rupture_progressive_reading, rupture_traditionalist_reading) are other constraints in other files. This omega records the kernel/reading identity so the committer frame is traceable.',
    'If this reading were folded into a single constraint with siblings, ε would become measurement-dependent (violating ε-invariance). The decomposition into separate stories with network.affects_constraints links is the structural fix.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel/reading identity and ε-invariance compliance for composite_overdetermination_reading of vatican_ii_doctrinal_authority').

omega_variable(
    component_extractiveness_independence,
    'Do the liturgical, ecumenical, ecclesiological, and political components of Vatican II have independently measurable extractiveness values, or does the composite packaging force a single ε?',
    'Separate constraint stories for each component (liturgical_reform_constraint, religious_freedom_constraint, ecumenical_engagement_constraint, ecclesiological_collegiality_constraint) with their own ε measurements, linked via network.affects_constraints to this composite story. If ε values differ significantly across components, the composite_overdetermination_reading is validated.',
    'If components have divergent ε (e.g., liturgical = 0.65 extraction, religious_freedom = 0.25, ecumenism = 0.30, collegiality = 0.40), the single-ε continuity/rupture debate is a category error. This reading''s claim that ''ambiguities are structural feature not bug'' is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_extractiveness_independence, empirical, 'Independent extractiveness of Vatican II''s convergent structural components').

omega_variable(
    ambiguity_as_feature_vs_bug,
    'Are the documented ambiguities in Vatican II texts (e.g., ''subsistit in'', ''seeds of the Word'', ''active participation'') deliberately overdetermined to hold the coalition together, or are they genuine failures of conciliar precision?',
    'Historical analysis of conciliar debates (acta synodalia), periti interventions, and voting records on ambiguous formulations. If ambiguities correlate with coalition-maintenance votes rather than theological precision votes, they are structural features.',
    'If ambiguities are deliberate overdetermination, the constraint functions as a tangled_rope holding a coalition; if genuine failures, the constraint is a piton (degraded coordination) or snare (extraction via ambiguity). This reading asserts the former.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_vs_bug, conceptual, 'Whether conciliar ambiguities are structural features enabling coalition or bugs enabling heterodox capture').

omega_variable(
    enforcement_mechanism_post_conciliar,
    'What enforces the composite packaging post-conciliar — magisterial authority, institutional inertia, or the coalition''s self-policing?',
    'Trace post-conciliar disciplinary actions (e.g., 1974 Catechetical Directory, 1983 Code of Canon Law liturgical norms, 1990 Ex Corde Ecclesiae, 2007 Summorum Pontificum / 2021 Traditionis Custodes arc) to see whether enforcement targets deviation from the composite package or from individual components.',
    'If enforcement selectively targets traditionalist dissent on liturgy while tolerating progressive dissent on ecclesiology, the composite is maintained by asymmetric enforcement (snare dynamics). If enforcement is even, it is genuine coordination maintenance (rope/tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_post_conciliar, empirical, 'Post-conciliar enforcement asymmetry across the composite''s components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.52).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.54).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.22).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.25).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_religious_freedom).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_ecumenical_engagement).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_ecclesiological_collegiality).

% DUAL FORMULATION NOTE:
% This constraint is the composite packaging story. The four affected constraints are the component-level stories, each with independent ε. The composite story extracts by making the package the unit of loyalty; component stories extract (or coordinate) on their own terms. The BGS pattern applies: upstream components (religious_freedom ≈ mountain/rope, ecumenism ≈ rope) influence downstream composite (tangled_rope), while liturgical_reform (snare/tangled_rope) and collegiality (tangled_rope) are contested mid-level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, institutional, 0.3).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, powerful, 0.65).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, organized, 0.2).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
