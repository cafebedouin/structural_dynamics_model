% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Composite Overdetermination Authority Structure
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962–1965) produced sixteen documents whose theological
 *   rationales were shaped by factional compromise between ressourcement
 *   theologians (Congar, de Lubac, Rahner) and neo-scholastic curial
 *   officials. The resulting texts contain irreducible contradictions:
 *   Dignitatis Humanae grounds religious liberty in human dignity
 *   (personalist), while pre-conciliar teaching grounded it in error's lack
 *   of rights (objectivist); Lumen Gentium teaches episcopal collegiality
 *   'with and under' the pope, but the relatio explicativa (Commission on
 *   Faith) asserted this does not diminish papal primacy—leaving the relation
 *   formally undefined; Sacrosanctum Concilium calls liturgy 'the work of
 *   Christ' and 'action of the Church,' but the 1969 Novus Ordo reshaped the
 *   rite around assembly participation. The
 *   composite_overdetermination_reading holds that these are not tensions
 *   awaiting synthesis but structural fractures encoding the council's
 *   political theology. Institutional authority (Roman Curia, CDF, papal
 *   magisterium post-1965) claims a 'hermeneutic of continuity' that resolves
 *   contradictions by fiat. Scholars recognizing complexity (e.g., O'Malley,
 *   Faggioli, Gaillardetz) benefit from the reading's explanatory power.
 *   Laity subjected to contradictory directives (e.g., Humanae Vitae vs.
 *   pastoral practice on conscience; communion for divorced/remarried vs.
 *   canon law) bear extraction without representation.
 *
 * KEY AGENTS:
 *   - complexity_recognizing_scholars: Primary beneficiary (organized/biographical) — gains explanatory framework for post-conciliar crisis
 *   - pastoral_adaptation_practitioners: Secondary beneficiary (moderate/biographical) — licensed to navigate contradictions pastorally
 *   - institutional_authority_claiming_univocal_interpretation: Primary victim/payer (institutional/generational) — must suppress contradiction to maintain legitimacy
 *   - laity_subjected_to_contradictory_directives: Secondary victim/payer (organized/biographical, identity_locked) — bears compliance costs under incompatible norms
 *   - traditionalist_catholics: Excluded (organized/biographical, identity_locked) — rejects the council entirely, would object to composite reading as legitimizing rupture
 *   - progressive_catholics: Excluded (organized/biographical, constrained) — reads council as rupture-for-good, would object to composite reading as denying progress
 *   - theological_observer: Observer (analytical/civilizational, analytical) — maps the structural field
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Composite Overdetermination Authority Structure").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'fb0ecfaa-d338-4b73-9cf5-ce90c60024c0').
narrative_ontology:cs_kernel_codification('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', formalized).
narrative_ontology:cs_authority_grounding('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', extraction).
narrative_ontology:cs_interpretation_layer_present('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0').
narrative_ontology:cs_reading_relation('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', vatican_ii_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', vatican_ii_authority__rupture_reading, influences).
narrative_ontology:cs_axiom('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', foundational, conciliar_texts_encode_irreducible_factional_contradictions).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_irreducible_factional_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', conciliar_texts_encode_irreducible_factional_contradictions, empirically_contingent).
narrative_ontology:cs_axiom('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', foundational, univocal_interpretation_claim_functions_as_extraction_not_coordination).
narrative_ontology:cs_axiom_status(univocal_interpretation_claim_functions_as_extraction_not_coordination, holdable).
narrative_ontology:cs_axiom_grounding('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', univocal_interpretation_claim_functions_as_extraction_not_coordination, instrumental).
narrative_ontology:cs_reference_frame('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', conciliar_composite_as_authoritative_settlement).
narrative_ontology:cs_drift_state('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', post_amoris_laetitia_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb0ecfaa-d338-4b73-9cf5-ce90c60024c0', '2026-08-04T14:22:11Z').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, complexity_recognizing_scholars).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, pastoral_adaptation_practitioners).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_authority_claiming_univocal_interpretation).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, laity_subjected_to_contradictory_directives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, institutional_authority_claiming_univocal_interpretation).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, pastoral_adaptation_practitioners).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_of_complexity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, factional_compromise_as_ecclesial_reality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Roman Curia (especially CDF/DDF), papal magisterium, and episcopal conferences acting in communion with Rome. They set the authoritative interpretation of Vatican II, control episcopal appointments, define orthodoxy boundaries, and enforce liturgical/canonical norms. They benefit from the univocality claim (legitimizes governance, prevents schism) but also bear the cost of maintaining it (suppressing contradiction, managing crises). Exit is arbitrage: they could reform the interpretive framework but would lose the legitimacy the univocality claim provides.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_authority_claiming_univocal_interpretation, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, institutional_authority_claiming_univocal_interpretation, beneficiary).

% Theologians, historians, and ecclesiologists (e.g., O'Malley, Faggioli, Gaillardetz, Alberigo, Komonchak) who recognize the council as factional compromise. They gain explanatory power, academic recognition, and institutional positions (universities, journals, synodal consultancies) from this reading. They do not administer the constraint. Exit is mobile: they can publish elsewhere, change fields, or leave academic theology without losing their primary identity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, complexity_recognizing_scholars, beneficiary,
    organized, biographical, mobile, global).

% Parish priests, campus ministers, spiritual directors, diocesan tribunal officials who navigate contradictory directives pastorally (e.g., communion for divorced/remarried, LGBTQ+ pastoral care, liturgical adaptation). They benefit from the composite reading's license to read complexity into practice. They pay in episcopal scrutiny, canonical risk, and psychological burden of representing contradictory norms. Exit is constrained: leaving ministry carries high vocational cost; staying requires constant negotiation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, pastoral_adaptation_practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, pastoral_adaptation_practitioners, payer).

% Baptized Catholics who receive contradictory teachings: Humanae Vitae's absolute norm vs. pastoral practice on conscience; canon law's marriage indissolubility vs. Amoris Laetitia's discernment; pre-conciliar liturgy vs. Novus Ordo as 'organic development.' They bear compliance costs (conscience conflict, sacramental exclusion, epistemic whiplash) without representation in the interpretive process. Exit is identity_locked: Catholic identity is constitutive of self (baptismal character, sacramental economy, communion of saints); leaving is not a consumer choice but an ontological rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, laity_subjected_to_contradictory_directives, payer,
    organized, biographical, identity_locked, universal).

% Catholics who reject Vatican II's legitimacy (SSPX, sedevacantists, traditionalist institutes). They would object to the composite reading as legitimizing the council's contradictions rather than exposing its errors. Their reading (rupture_reading) is foreclosed by the institutional univocality claim, yet they are excluded from the authoritative conversation. Exit is identity_locked: their Catholic identity is bound to the pre-conciliar form; the post-conciliar church is experienced as a counter-church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_catholics, excluded,
    organized, biographical, identity_locked, global).

% Catholics who read Vatican II as rupture-for-good (spirit of the council, women's ordination, married priesthood, LGBTQ+ inclusion). They would object to the composite reading as denying the council's progressive thrust and legitimizing institutional inertia. Their reading is marginally tolerated but structurally excluded from magisterial authority. Exit is constrained: they remain in the church hoping for reform; leaving carries vocational/communal cost but is thinkable.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, progressive_catholics, excluded,
    organized, biographical, constrained, global).

% External analysts (sociologists of religion, political theologians, comparative ecclesiologists) who map the structural field without ecclesial commitment. They neither collect nor pay. Exit is analytical: the constraint is an object of study, not a lived arrangement.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, theological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The council solved real coordination problems: religious liberty (Dignitatis Humanae) resolved Church-state conflict in pluralist societies; ecumenism (Unitatis Redintegratio) opened divided Christians to dialogue; liturgical reform (Sacrosanctum Concilium) enabled vernacular participation; lay vocation (Apostolicam Actuositatem) recognized baptismal priesthood. These functions are genuine and persist.
% TRANSFER_FUNCTION: The constraint moves interpretive authority and compliance costs from the institutional center (which claims univocality) to the periphery (laity, lower clergy, local churches) who must live under contradictory norms. It moves explanatory legitimacy to scholars who recognize complexity. It moves pastoral risk to practitioners who navigate contradictions. The extraction is the gap between the univocality claim and the composite reality.
% ABSENT_VOICES: The laity subjected to contradictory directives have no formal voice in the interpretive process (synods are consultative, not deliberative). Traditionalist Catholics are excluded as schismatic/irregular. Progressive Catholics are excluded as heterodox. The 'sensus fidelium' is invoked but not institutionalized. These voices would object to the univocality claim but are structurally silenced by the same authority that claims to represent them.
% DISAPPEARANCE_RATIONALE: If the univocality claim vanished overnight, the institutional authority would lose its primary legitimating device. Episcopal conferences would gain genuine interpretive authority. Laity would gain recognized conscience autonomy. Scholars would lose their oppositional stance. The Church would reorganize around confessed pluralism (Anglican Communion model) or fragment (Protestant model). The coordination functions (religious liberty, ecumenism, liturgy) would persist but under different authority structures.
% FOUNDING_PROBLEM: The council was built to solve the Church's crisis of modernity: loss of Catholic states, secularization, Protestant division, liturgical passivity, clericalism. The factional compromise between ressourcement and neo-scholasticism was the political price of assembling a council at all.
% FOUNDING_PROBLEM_CORROBORATION: Congar's and de Lubac's own journals (published posthumously) document the compromise negotiations. O'Malley's 'What Happened at Vatican II' (2008) and Faggioli's 'True Reform' (2012) corroborate from outside the benefiting parties (neither is a curial official). The International Theological Commission's 2012 document 'Theology Today' acknowledges 'tensions' but frames them as 'hermeneutic challenges'—an internal corroboration that the founding problem (modernity) has mutated beyond the council's solutions.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68 (end of interval): The authority structure extracts compliance from laity and lower clergy under contradictory norms while claiming univocality. The cost of maintaining this claim rises over time (suppression 0.35→0.72) as contradictions become more visible (post-conciliar conflicts, cultural secularization, clerical abuse crisis revealing governance failures). Theater ratio 0.41: substantial performative maintenance (curial documents on 'hermeneutic of continuity,' papal catecheses on continuity) while functional coordination (unified teaching) degrades. Accessibility collapse 0.45: alternatives exist (schism, exit, internal dissent) but ecclesial identity and sacramental economy constrain them. Resistance 0.58: significant but fragmented across traditionalist, progressive, and reformist vectors. The constraint is a tangled_rope: genuine coordination (the council did solve real problems: religious liberty, ecumenism, liturgical participation, lay vocation) AND asymmetric extraction (authority claims univocality to legitimize its governance while suppressing the evidence that the council's own texts encode factional compromise). Active enforcement required: CDF investigations, episcopal appointments, liturgical norms, canon law revisions all enforce the univocality claim.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional authority seat (agenda_setter), the constraint appears as rope: a genuine coordination achievement (the council) whose meaning must be authoritatively unified to prevent fragmentation. From the laity seat (payer, identity_locked), it appears as snare: contradictory directives bind conscience while exit is ecclesially unthinkable. From the scholar seat (beneficiary), it appears as mountain: the composite structure is a fact of conciliar history, not a human arrangement. The engine computes this divergence from the structural data—beneficiary/victim declarations, exit options, power levels—without author adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional authority (agenda_setter, institutional power, generational horizon, arbitrage exit, universal scope) is the structural beneficiary: it administers the interpretation, controls appointments, defines orthodoxy, and collects compliance. Its d is low (near 0.0) because the constraint subsidizes its governance. Scholars recognizing complexity (beneficiary, organized, biographical, mobile, global) gain explanatory power without bearing governance costs—d near 0.0. Laity (payer, organized, biographical, identity_locked, universal) bear the compliance costs under contradictory norms with no exit that preserves ecclesial identity—d near 1.0. Pastoral practitioners (secondary_role: beneficiary/payer, moderate, biographical, constrained, regional) navigate the contradictions pastorally—d ~0.5. Traditionalists and progressives (excluded, organized, biographical, identity_locked/constrained, global) are structurally excluded from the authoritative interpretation—their readings are foreclosed by the univocality claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (addressing modernity, religious liberty, ecumenism, liturgical participation) was live in 1962. By 2015, the problem has mutated: secularization has deepened, the religious liberty settlement is contested from both sides, ecumenism has stalled, liturgical participation has declined. The arrangement (authoritative univocal interpretation of composite texts) persists but the founding problem is dead/contested. The constraint exhibits mandatrophy: the authority structure's claim to univocality now serves primarily to extract compliance and maintain institutional coherence, not to solve the problems the council addressed. The theater_ratio rise (0.18→0.41) and suppression rise (0.35→0.72) track this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the vatican_ii_authority kernel, and what distinguishes it from the continuity_reading and rupture_reading siblings?',
    'Structural comparison of beneficiary/victim sets, extractiveness profiles, and cs_structure axioms across the three readings. The composite_overdetermination_reading denies the univocal interpretability that both siblings assume; this denial is its foundational axiom.',
    'If the reading is mischaracterized as a mere ''middle position'' between continuity and rupture, its distinct structural claim (ambiguity as irreducible product of factional compromise) is lost. The engine must treat it as a separate constraint with its own ε and seat structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to this reading as a distinct kernel instantiation, not a synthesis of siblings').

omega_variable(
    authority_extraction_mechanism,
    'Does the institutional authority''s claim to univocal interpretation extract compliance through genuine coordination (unity of teaching) or through suppressing the evidence of irreducible contradiction?',
    'Historical analysis of post-conciliar magisterial interventions (e.g., doctrinal congregations'' responses to theologians, disciplinary actions against dissent): when authority acts, does it address the contradiction substantively or suppress its articulation?',
    'If suppression of contradiction is the primary mechanism, the constraint''s extraction is asymmetric and the tangled_rope classification holds. If coordination predominates, the reading collapses toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_extraction_mechanism, empirical, 'Whether authority''s univocality claim functions as coordination or extraction').

omega_variable(
    laity_exit_options_ambiguity,
    'Are laity subjected to contradictory directives truly identity_locked (ecclesial identity makes exit unthinkable) or constrained (alternatives exist but carry high cost)?',
    'Sociological data on Catholic disaffiliation patterns post-1965: correlation between exposure to contradictory teachings and exit vs. internal dissent vs. compartmentalization.',
    'If identity_locked, laity''s effective extraction is amplified (d→1.0). If constrained, extraction is high but not maximized. Determines whether laity seat computes as snare-level or tangled_rope-level victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(laity_exit_options_ambiguity, empirical, 'Exit structure for laity under contradictory magisterial directives').

omega_variable(
    factional_compromise_irreducibility,
    'Are the documented theological contradictions (e.g., religious liberty vs. confessional state, collegiality vs. papal primacy, liturgy as sacrifice vs. liturgy as assembly) genuinely irreducible, or do they admit a higher synthesis not yet articulated?',
    'Systematic theology engagement: can a coherent theological framework integrate Gaudium et Spes anthropology with Dignitatis Humanae liberty, Lumen Gentium collegiality with Pastor Aeternus primacy, and Sacrosanctum Concilium liturgical theology without remainder? The burden of synthesis is on the continuity claim.',
    'If irreducible, the composite_overdetermination_reading''s ε is validated as structural fact. If synthesizable, the constraint reduces to a coordination problem (rope) with temporary interpretive friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(factional_compromise_irreducibility, conceptual, 'Whether conciliar contradictions admit theological synthesis or are structurally permanent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_composite_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(vatican_ii_composite_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vatican_ii_composite_tr_t1985, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(vatican_ii_composite_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vatican_ii_composite_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(vatican_ii_composite_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.41).

% Extraction over time
narrative_ontology:measurement(vatican_ii_composite_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(vatican_ii_composite_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vatican_ii_composite_be_t1985, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(vatican_ii_composite_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(vatican_ii_composite_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(vatican_ii_composite_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_composite_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(vatican_ii_composite_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(vatican_ii_composite_su_t1985, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(vatican_ii_composite_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(vatican_ii_composite_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(vatican_ii_composite_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__composite_overdetermination_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, humanae_vitae_authority).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, liturgical_reform_authority).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, collegiality_papal_primacy_tension).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, religious_liberty_anthropology_tension).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel decomposes into three constraint stories: this composite_overdetermination_reading (tangled_rope, ε=0.68), continuity_reading (rope→mountain claim, low ε), and rupture_reading (snare claim, high ε from traditionalist seat). The ε-invariance principle requires separate stories because the continuity reading treats the council as a Mountain of tradition (ε≈0.1), the rupture reading treats it as a Snare of innovation (ε≈0.8 from traditionalist seat), and this reading treats the composite structure as a Tangled Rope (ε=0.68) with genuine coordination function and asymmetric extraction. The contradictions (religious liberty, collegiality, liturgy) are themselves child constraints in the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
