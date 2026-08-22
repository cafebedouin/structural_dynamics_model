% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Christ Created and Subordinate to the Father
 *   domain: ecclesiastical/theological/political
 *
 * SUMMARY:
 *   The Arian Christology emerged as a coordinating framework for
 *   understanding Christ's relationship to the Father, but rapidly became a
 *   tool for managing ecclesiastical authority and imperial control. Arius
 *   and his successors articulated a reading of Scripture that posits Christ
 *   as created, subordinate to the Father, and of dissimilar substance (not
 *   homoousios). This reading found support in distributed episcopal networks
 *   and was weaponized by successive imperial courts (especially under
 *   Constantius II) to enforce religious uniformity aligned with imperial
 *   power structures. The constraint is CLAIMED as tangled_rope (genuine
 *   theological coordination + asymmetric extraction of authority and
 *   doctrinal control) while the authored metrics describe substantial
 *   extraction defended through active suppression — the engine measures this
 *   divergence.
 *
 * KEY AGENTS:
 *   - Arian theologians and non-Nicene bishops: defend the subordinationist reading, lead distributed networks, resist imperial pro-Nicene enforcement
 *   - Pro-Nicene episcopacy: bears the cost of doctrinal conflict, must re-litigate Nicene doctrine across successive imperial regimes
 *   - Imperial court Arian faction: uses Arian theology as a tool for managing religious unity, deploys imperial apparatus, withdraws support when politically convenient
 *   - Eastern provincial populations: absorb Arian theology into local identity and practice, especially in Syria and parts of Anatolia
 *   - Pro-Nicene believers in Arian regions: trapped, silenced, bearing costs of doctrinal displacement
 *   - Non-Arian rival theological schools: excluded from the Arian-Nicene binary, suppressed through structural silence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.72).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Christ Created and Subordinate to the Father").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "ecclesiastical/theological/political").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, 'af761690-e17a-4565-861b-9914b3846607').
narrative_ontology:cs_kernel_codification('af761690-e17a-4565-861b-9914b3846607', fixed_text).
narrative_ontology:cs_authority_grounding('af761690-e17a-4565-861b-9914b3846607', lineage).
narrative_ontology:cs_interpretation_layer_present('af761690-e17a-4565-861b-9914b3846607').
narrative_ontology:cs_reading_relation('af761690-e17a-4565-861b-9914b3846607', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('af761690-e17a-4565-861b-9914b3846607', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('af761690-e17a-4565-861b-9914b3846607', foundational, christ_created_not_eternal).
narrative_ontology:cs_axiom_status(christ_created_not_eternal, overridden).
narrative_ontology:cs_axiom_grounding('af761690-e17a-4565-861b-9914b3846607', christ_created_not_eternal, empirically_contingent).
narrative_ontology:cs_axiom('af761690-e17a-4565-861b-9914b3846607', foundational, father_alone_is_god_fullest_sense).
narrative_ontology:cs_axiom_status(father_alone_is_god_fullest_sense, overridden).
narrative_ontology:cs_axiom_grounding('af761690-e17a-4565-861b-9914b3846607', father_alone_is_god_fullest_sense, deontological).
narrative_ontology:cs_reference_frame('af761690-e17a-4565-861b-9914b3846607', apostolic_scriptural_subordinationism).
narrative_ontology:cs_drift_state('af761690-e17a-4565-861b-9914b3846607', post_constantinople_imperial_consolidation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('af761690-e17a-4565-861b-9914b3846607', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, non_nicene_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, imperial_court_arian_faction).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_episcopacy).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, theological_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, eastern_provincial_populations).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_believers_arian_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the non-Nicene ecclesiastical network, author theological defenses of subordinationism, ordain clergy, teach congregations. Set the doctrinal agenda for their constituencies and resist conciliar definitions imposed from Constantinople. Their institutional authority derives from apostolic succession claims and episcopal collegiality, but faces systematic delegitimization by pro-Nicene councils and imperial courts. They cannot simply exit their commitment to the subordinationist reading without doctrinal betrayal of their entire theological tradition and episcopal office.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_theologians_non_nicene_bishops, agenda_setter,
    institutional, generational, constrained, continental).

% Defend the Nicene homoousios against Arian reading and its imperial backing. Must repeatedly defend their doctrinal position through councils, conciliar letters, and mobilization of sympathetic imperial courts. Bear the institutional cost of doctrinal fragmentation — their victory at Nicaea is undermined when imperial backing shifts; they must re-litigate the same theological ground across decades. Constrained exit because they are doctrinally committed to homoousios and cannot compromise without ceasing to be pro-Nicene.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_episcopacy, payer,
    institutional, generational, constrained, continental).

% Deploy the Arian reading as a tool for managing religious uniformity aligned with imperial will. Find Arianism's emphasis on Christ's subordination compatible with imperial hierarchy. Use imperial apparatus (councils, bishops appointments, patronage withdrawal) to enforce Arian dominance under favorable emperors. Withdraw support when political winds shift (as in succession disputes). High mobility because their commitment to Arianism is purely instrumental — they will adopt pro-Nicene doctrine when it serves empire-building better.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, imperial_court_arian_faction, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, imperial_court_arian_faction, agenda_setter).

% In Syria, parts of Anatolia, and Mesopotamia, populations absorb Arian theology through catechism, hymnody, liturgical practice, and episcopal leadership. The reading becomes interwoven with local ecclesiastical identity, family tradition, and community belonging. They experience genuine coordination benefit (unified doctrine, coherent cosmology explaining Christ's place in the divine order), but this benefit is inseparable from identity fusion — exiting the reading would require renouncing community and self-concept. Exit is identity-locked: economically and socially trapped, with centuries of tradition and family lineage tied to the reading.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, eastern_provincial_populations, beneficiary,
    organized, biographical, identity_locked, regional).

% In regions where Arian bishops dominate (much of the East after Constantius II), pro-Nicene believers are outnumbered, excluded from office, and face liturgical displacement. They cannot leave (rooted in locality, family, economic ties); they cannot change their doctrinal conviction without betraying apostolic faith (as they understand it); they cannot persuade the dominant episcopacy. They experience multiple overlapping suppressions: structural (no options), internalized (isolated from sympathetic voices, surrounded by contrary teaching), and performative (required attendance at Arian liturgies that contradict their belief). The suppression here is near-maximal.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_believers_arian_regions, payer,
    powerless, biographical, trapped, local).

% Gnostic, Docetic, Modalist, and other non-orthodox readings have no institutional seat in the Arian-Nicene dispute. The enforcement machinery operates as though the binary (Arian or Nicene) exhausts theological possibility. Alternative Christologies are suppressed not through overt persecution but through structural exclusion from councils and conciliar discourse. Their voices would complicate the dispute, so the contest is framed to render them inaudible.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, non_arian_non_nicene_schools, excluded,
    moderate, biographical, constrained, continental).

% Councils (Nicaea 325, Antioch 328, Serdica 342, Constantinople I 381) are the formal apparatus for adjudicating orthodoxy. They claim to ground themselves in apostolic authority and conciliar consensus but are heavily influenced by imperial pressure and episcopal politics. From the analytical seat, councils operate as a method of translating power into doctrinal authority — imperial backing determines which reading wins formal status, which is then enforced through ecclesiastical machinery.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, ecumenical_councils, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, imperial_court_arian_faction).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent theological framework for understanding Christ's relationship to the Father, reconciling Christ's divine dignity with strict monotheism (the Father alone is God in the fullest sense). Offers a rationale for cosmic, ecclesiastical, and political hierarchy: just as Christ is subordinate to the Father, the church and empire should be coordinated under supreme authority. Coordinates belief across dispersed episcopal networks and creates a unified doctrinal system that appeals to both theological systematicity and imperial governance logic.
% TRANSFER_FUNCTION: Moves ecclesiastical authority from pro-Nicene bishops to Arian-aligned episcopacy. Extracts doctrinal allegiance from believers, channeling it toward Arian networks and imperial-backed bishops. Transfers theological legitimacy from alternative Christologies (Gnostic, Docetic) to the binary Arian-Nicene framework. Imperial courts extract religious utility (appearance of unified doctrine under their authority) and use the constraint to manage ecclesiastical subordination to imperial will.
% ABSENT_VOICES: Non-Arian and non-Nicene theological schools (Gnostic, Docetic, Modalist traditions) would argue that both Arianism and Nicene orthodoxy rest on false metaphysical premises and that the apostolic tradition supports their own readings. Lay believers in Arian-dominant regions who harbor private doubts about subordinationism have no institutional voice — they are isolated from corroborating witnesses and rendered tongueless by liturgical and catechetical enforcement. Pro-Nicene dissidents in Arian strongholds are silenced by social pressure and institutional exclusion.
% DISAPPEARANCE_RATIONALE: If the Arian reading and its enforcement apparatus vanished overnight, the distributed Arian episcopal networks would lose their doctrinal legitimacy and institutional coherence. Congregations in Arian-dominant regions would face a choice between adopting Nicene orthodoxy (now unopposed) or fragmenting into smaller sects or alternative traditions. Imperial courts would lose their tool for managing religious uniformity and would have to rely on brute force or ideological compromise with pro-Nicene establishment. The ecclesiastical map of the Mediterranean would reorganize rapidly around pro-Nicene consolidation (as in fact occurred by 381). The very fact that successive imperial regimes and councils must explicitly defend or condemn Arianism shows that the constraint is not stable without active maintenance — disappearance would trigger immediate rearrangement.
% FOUNDING_PROBLEM: The early Christian community faced an unresolved theological crisis: if Christ is truly divine, how can monotheism hold (did Christians become polytheists)? If Christ is fully human, is he not merely another creature rather than savior? How can God remain one while three persons are worshipped? The Arian reading solves this by positing Christ as a subordinate divine being — created by the Father before all time, granted divine powers and worthy of worship, but not identical in substance with the Father. This preserves strict monotheism (the Father alone is theos, 'God' in the fullest sense) while honoring Christ's exalted status. The reading provides a systematic answer grounded in close attention to scriptural language (the Son is 'made,' 'begotten,' 'subordinate') and appeals to both theological rigor and political order.
% FOUNDING_PROBLEM_CORROBORATION: The Arian bishops and their imperial sponsors attest that the founding problem (reconciling Christ's divinity with monotheism without polytheism) remains live and unsolved except by subordinationism. They cite scriptural language of the Son's subordination and generation and argue that homoousios (Nicene alternative) is unscriptural and implicitly leads to Sabellianism (indistinguishability of persons). Pro-Nicene theologians (Athanasius, Gregory of Nyssa, Gregory of Nazianzus) attest from outside the Arian benefiting parties that the founding problem is indeed real, but that Arianism solves it by degrading Christ to a creature and violating the apostolic witness to Christ's full divinity. Independent observers (secular historians, comparative theology scholars) attest that the founding problem is a genuine theological puzzle with multiple defensible solutions, but that by the end of the interval (and certainly by subsequent centuries) the Arian solution has ceased to be credible within Christian orthodoxy — the problem persists, but Arianism's answer is no longer treated as a live option in mainstream Christian theology. This marks the constraint as having atrophied from a genuine theological solution into institutional inertia maintained by historical succession and regional tradition rather than living persuasiveness.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (early Arian ascendancy, relatively unopposed) to 0.68 (hardened enforcement under Constantius II and beyond) and plateaus there. The rise reflects increasing imperial deployment of machinery to enforce Arian dominance against Nicene resistance — more active coercion is needed as resistance grows. Suppression follows a parallel trajectory, rising from 0.45 to 0.72, indicating escalating enforcement intensity. Theater ratio climbs from 0.25 to 0.41, suggesting that as Arianism becomes institutionalized doctrine, more of the constraint's operation involves performative reaffirmation (conciliar pronouncements, liturgical enforcement, episcopal conformity) and less involves genuine theological persuasion. The plateau at t=80 marks the shift toward Arianism as ossified imperial-backed orthodoxy losing internal vitality — the constraint persists through institutional inertia and imperial backing, not through living theological commitment. All measurements are on a single time grid (every metric authored at every time point).
 *
 * PERSPECTIVAL GAP:
 *   The constraint is experienced as coordination from the agenda-setter seat (Arian bishops + imperial court) because they author doctrine and benefit from the framework's coherence. It is experienced as enforced extraction from the payer seat (pro-Nicene bishops) because their doctrinal position is systematically delegitimized and their institutional authority eroded. The imperial court's high exit mobility (they can shift backing) creates a fundamentally different experience than the pro-Nicene bishops' constrained exit (they cannot simply abandon Nicene theology without doctrinal betrayal). This is the mandatrophy point: does the constraint persist because it genuinely solves a coordination problem, or because it extracts sufficient benefit to those with enforcement capacity that they will defend it against alternatives? The measurement series show increasing theater ratio as Arianism becomes imperial doctrine — more performance, less persuasion — which suggests the coordination function is attenuating while extraction machinery is hardening.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial court Arian faction: d = 0.15 (arbitrage exit, they can shift theological allegiance; beneficiaries of extracted authority but not dependent on theological truth). Arian bishops: d = 0.35 (constrained exit, depend on imperial backing and episcopal constituencies; beneficiaries of doctrinal control but vulnerable to regime change). Pro-Nicene bishops: d = 0.82 (constrained exit, doctrinal investment; victims of delegitimization). Pro-Nicene believers in Arian regions: d = 0.93 (trapped + identity-locked; victims of erasure and sacramental displacement). Eastern provincial populations in Arian regions: d = 0.55 (organized power, biographical horizon, but identity-locked through liturgy and community — the identity fusion complicates beneficiary/payer distinction). The d variation across seats with different power/exit profiles is what generates per-seat type divergence: from the agenda-setter seat this looks like rope (genuine coordination); from the payer seat it looks like snare (enforcement-dependent extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy signals: (1) Rising theater ratio (0.25 → 0.41) indicates increasing performative maintenance as internal conviction erodes. (2) Plateau in extractiveness and suppression at t=60+ suggests the constraint has reached a stable enforced state that does not improve its coordination output but does maintain its extraction machinery. (3) The founding problem (reconciling Christ's divinity with monotheism) remains live in theological debate, but the Arian solution has become atrophied within Christian orthodoxy — the Arian reading solves the founding problem cogently, but by the end of the interval (t=80), Arianism has lost credibility as a live theological option within mainstream Christianity, surviving only as imperial doctrine and regional tradition. The constraint persists not because the founding problem requires subordinationism but because imperial backing and institutional inertia hold it in place. The pro-Nicene reading eventually triumphs (Constantinople I formalizes this, beyond the interval), marking the constraint's functional death despite institutional persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordinationism_vs_arianism_scope,
    'Is the Arian reading''s core claim (Christ is created and subordinate) a logically coherent theological system, or does it collapse under internal scrutiny (as pro-Nicene theologians argued)?',
    'Systematic theological analysis of Arian texts (Arius''s Thalia, Eusebius of Caesarea''s defenses, later Eunomian developments) against pro-Nicene critiques; examination of whether the Arian framework can account for the full scriptural witness and maintain coherent Christology.',
    'If Arianism is internally coherent, the constraint''s persistence reflects genuine theological disagreement and institutional/political power. If Arianism logically self-refutes, the constraint''s persistence is pure extraction dressed in theological language — a snare, not a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationism_vs_arianism_scope, conceptual, 'Logical coherence vs. dogmatic cover story for subordinationism.').

omega_variable(
    identity_lock_mechanism_provincial,
    'In provinces where Arianism becomes embedded in local identity (Syria, parts of Anatolia), is the observed suppression of pro-Nicene believers structural (external barriers, institutional exclusion) or internalized (populations fuse Arian doctrine with self-concept)?',
    'Post-suppression trajectory analysis: if pro-Nicene populations regain institutional voice (post-Constantinople I), do pro-Nicene believers in Arian-dominant regions quickly shift to Nicene orthodoxy (structural suppression), or do they resist, maintain Arian identity, and fragment into separate communities (internalized identity-lock)?',
    'If internalized, the constraint''s effective suppression is higher than the 0.72 scalar suggests — victims carry the constraint''s logic with them after institutional enforcement lifts. The suppression persists as cognitive/identity pattern rather than external coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_provincial, empirical, 'Structural vs. internalized suppression in identity-locked provincial populations.').

omega_variable(
    imperial_theological_capture,
    'Does the imperial court genuinely believe the Arian reading is theologically superior, or do they deploy it as a convenient tool for religious uniformity, with no deeper theological commitment?',
    'Historical examination of imperial correspondence, council records, and succession patterns: does imperial favor for Arianism persist across regimes and emperors, or does it shift when political utility changes? Do imperial theologians develop sophisticated defenses of subordinationism, or merely assert it as doctrine?',
    'If genuine theological commitment, the constraint exhibits rope-like coordination (real belief alignment). If pure tool-use, the constraint is a pure snare with theological language as cover, and the imperial court is the sole captor. This determines whether beneficiary consolidation is real or theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_theological_capture, empirical, 'Imperial theological commitment vs. strategic religious engineering.').

omega_variable(
    kernel_vs_reading_framing,
    'Is the Arian-Nicene-Semi-Arian dispute a contest over the correct reading of a fixed kernel (the question ''What is Christ?''), or does each reading instantiate a fundamentally different kernel (different presuppositions about divinity, substance, scriptural authority)?',
    'Genealogical and philosophical analysis: do the three readings operate within a shared framework of debate (same authorities, same logical methods, same metaphysical vocabulary), or do they privilege different authorities (Arius appeals to Lucian of Antioch, Nicene appeals to apostolic succession, etc.) such that they are not debating the same question?',
    'If shared kernel, the readings are genuine alternatives within a unified theological tradition. If different kernels, the ''contest'' is partly talking past each other, and the Arian reading''s persistence reflects institutional power rather than theological persuasiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_framing, conceptual, 'Single contested kernel vs. incommensurable theological frameworks.').

omega_variable(
    arian_afterlife_and_constraint_status,
    'After the pro-Nicene victory (Constantinople I, 381), does Arianism persist as a live theological option in mainstream Christian orthodoxy, or only as a historical memory and exotic regional tradition (Gothic Christianity, parts of the East)?',
    'Post-interval ecclesiastical history: does Arianism generate new defenders, new theological development, or new institutional power after pro-Nicene consolidation? Or does it become a dead doctrine maintained only by institutional inertia in isolated communities?',
    'If Arianism persists as live option, the constraint''s type remains tangled_rope or snare. If it dies (becomes historical artifact), the constraint transitions to piton status — institutional/liturgical maintenance of atrophied doctrine. This marks the endpoint of the constraint''s functional life.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arian_afterlife_and_constraint_status, empirical, 'Arianism as live theological force vs. atrophied doctrine maintained by institutional inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__arian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(homo_tr_t0, observed).
narrative_ontology:measurement(homo_tr_t10, homoousios_christology__arian_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(homo_tr_t10, observed).
narrative_ontology:measurement(homo_tr_t20, homoousios_christology__arian_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(homo_tr_t20, observed).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__arian_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(homo_tr_t40, observed).
narrative_ontology:measurement(homo_tr_t60, homoousios_christology__arian_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(homo_tr_t60, observed).
narrative_ontology:measurement(homo_tr_t80, homoousios_christology__arian_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(homo_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__arian_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(homo_be_t0, observed).
narrative_ontology:measurement(homo_be_t10, homoousios_christology__arian_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(homo_be_t10, observed).
narrative_ontology:measurement(homo_be_t20, homoousios_christology__arian_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(homo_be_t20, observed).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__arian_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(homo_be_t40, observed).
narrative_ontology:measurement(homo_be_t60, homoousios_christology__arian_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(homo_be_t60, observed).
narrative_ontology:measurement(homo_be_t80, homoousios_christology__arian_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(homo_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__arian_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(homo_su_t0, observed).
narrative_ontology:measurement(homo_su_t10, homoousios_christology__arian_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(homo_su_t10, observed).
narrative_ontology:measurement(homo_su_t20, homoousios_christology__arian_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(homo_su_t20, observed).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__arian_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(homo_su_t40, observed).
narrative_ontology:measurement(homo_su_t60, homoousios_christology__arian_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(homo_su_t60, observed).
narrative_ontology:measurement(homo_su_t80, homoousios_christology__arian_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(homo_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__arian_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__semi_arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three structurally distinct constraints, each a different reading of the same historical theological commitment. The Arian reading (this file) posits Christ as created and subordinate; the pro-Nicene reading posits homoousios consubstantiality; the semi-Arian reading offers a compromise. These are not the same constraint viewed from different angles — their ε values, stakeholder maps, and types diverge substantially. The ε-invariance test: measuring subordinationism vs. homoousios yields clearly different extraction profiles depending on how you count authority, doctrinal control, and the cost to dissidents. Each reading is a separate constraint file. The ε referent for this Arian reading is the standing arrangement of subordinationist Christology as the Arian tradition sees it (high extraction from dissidents, genuine theological coordination within the Arian network); the pro-Nicene reading's ε referent is the same standing arrangement as seen by Nicene tradition (higher extraction, less coordination). Both readings assess the same historical arrangement (Arian doctrine under imperial backing); values stay reading-indexed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__arian_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
