% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Divine Nature (Father Alone Is God; Son/Spirit Subordinate or Created)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested biblical_divine_nature
 *   kernel: the unitarian reading, in which numerical monotheism is read
 *   strictly — the Father alone is identical with God, and the Son and Spirit
 *   are subordinate, derivative, or created beings, not co-equal persons
 *   sharing one divine essence. This reading has recurred across Christian
 *   history (early subordinationist and 'monarchian' currents, fourth-century
 *   Arian and semi-Arian controversies, Socinianism, and various modern
 *   Unitarian and restorationist movements) always in direct tension with the
 *   eventual trinitarian settlement. ε is authored for the standing
 *   arrangement this reading actually produces on the ground: a reading that
 *   functions as genuine textual/interpretive coordination for those who hold
 *   it, but which also generates real institutional conflict, exclusion, and
 *   — where creedal orthodoxy holds civil or ecclesiastical power —
 *   persecution of its holders. The sibling readings (trinitarian_reading,
 *   modalist_reading) are separate constraint files with their own ε and
 *   stakeholder sets; this file does not average over them or describe the
 *   contest internally, per Rule 1.
 *
 * KEY AGENTS:
 *   - unitarian_congregational_leaders: agenda_setter (moderate/constrained) — teach and defend the reading, gain congregational legitimacy
 *   - lay_readers_of_plain_text: beneficiary (powerless/constrained) — gain interpretive access without technical metaphysics
 *   - creedal_orthodoxy_institutions: payer (institutional/arbitrage) — bear a direct legitimacy challenge to 1500+ years of settlement
 *   - trinitarian_clergy_hierarchy: payer (organized/constrained) — vocational legitimacy threatened
 *   - excommunicated_unitarian_believers: payer (powerless/trapped) — bear direct exclusion and historical persecution costs
 *   - trinitarian_reading_holders: excluded — hold the sibling reading, not modeled as a party here
 *   - historians_of_early_christian_doctrine: observer (analytical) — corroborate the founding problem from outside both camps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.58).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Divine Nature (Father Alone Is God; Son/Spirit Subordinate or Created)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '7d829274-5897-4b79-a175-3b045114ccde').
narrative_ontology:cs_kernel_codification('7d829274-5897-4b79-a175-3b045114ccde', fixed_text).
narrative_ontology:cs_authority_grounding('7d829274-5897-4b79-a175-3b045114ccde', practice).
narrative_ontology:cs_interpretation_layer_present('7d829274-5897-4b79-a175-3b045114ccde').
narrative_ontology:cs_reading_relation('7d829274-5897-4b79-a175-3b045114ccde', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('7d829274-5897-4b79-a175-3b045114ccde', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('7d829274-5897-4b79-a175-3b045114ccde', foundational, father_alone_is_numerically_identical_to_god).
narrative_ontology:cs_axiom_status(father_alone_is_numerically_identical_to_god, holdable).
narrative_ontology:cs_axiom_grounding('7d829274-5897-4b79-a175-3b045114ccde', father_alone_is_numerically_identical_to_god, conventional).
narrative_ontology:cs_axiom('7d829274-5897-4b79-a175-3b045114ccde', foundational, son_and_spirit_are_ontologically_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_and_spirit_are_ontologically_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('7d829274-5897-4b79-a175-3b045114ccde', son_and_spirit_are_ontologically_subordinate_or_created, conventional).
narrative_ontology:cs_reference_frame('7d829274-5897-4b79-a175-3b045114ccde', apostolic_monotheistic_confession).
narrative_ontology:cs_drift_state('7d829274-5897-4b79-a175-3b045114ccde', post_nicene_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7d829274-5897-4b79-a175-3b045114ccde', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregational_leaders).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, lay_readers_of_plain_text).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, creedal_orthodoxy_institutions).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_clergy_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, excommunicated_unitarian_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and defend a reading of scripture in which the Father alone is numerically identical with God, and the Son and Spirit hold a subordinate or derived status. They administer local congregational authority independent of any creedal hierarchy, and gain doctrinal legitimacy and congregational cohesion from the reading's claim to restore 'plain' biblical monotheism against what they see as later philosophical accretion.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregational_leaders, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, unitarian_congregational_leaders, beneficiary).

% Encounter a reading of scripture that does not require mastering technical metaphysical vocabulary (ousia, hypostasis, homoousion) to feel doctrinally competent. They gain a sense of interpretive access and are relieved of dependence on councils and creeds they cannot evaluate, but their standing in wider Christian institutional life is jeopardized wherever this reading is treated as heretical.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, lay_readers_of_plain_text, beneficiary,
    powerless, biographical, constrained, local).

% Bodies whose authority is built on the Nicene and post-Nicene settlement bear a direct legitimacy challenge from this reading: if the unitarian reading is correct, fifteen centuries of councils, creeds, and disciplinary machinery rest on a mistaken metaphysical premise. They retain enormous institutional resources to resist the reading (excommunication, denial of sacraments, exclusion from ecumenical recognition) even as this reading erodes their claim to have settled the question.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, creedal_orthodoxy_institutions, payer,
    institutional, civilizational, arbitrage, global).

% Clergy whose ordination, teaching authority, and professional identity are built on trinitarian confession must treat this reading as a live threat to their vocational legitimacy, not merely an academic disagreement. Their exit options are limited by career and identity investment in the office they hold.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_clergy_hierarchy, payer,
    organized, biographical, constrained, national).

% Historical and contemporary individuals and groups (from the fourth-century subordinationist controversies through modern Unitarian, Socinian, and Christadelphian communities) who hold this reading and are excluded from mainstream sacramental life, denied burial rights, or subject to civil penalty in jurisdictions where trinitarian orthodoxy is legally established. They bear the direct cost of the doctrinal contest without institutional power to contest their exclusion.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, excommunicated_unitarian_believers, payer,
    powerless, biographical, trapped, local).

% Hold the sibling reading of the same kernel — three hypostases sharing one ousia. They are not represented as parties within this constraint story (which is authored from the unitarian reading's own lights) but their objection is the structural pressure this reading exists in response to and against.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_reading_holders, excluded,
    institutional, civilizational, analytical, global).

% Study the textual and historical record of how monotheistic confession, subordinationist Christology, and eventual homoousian settlement developed, without a stake in which reading is doctrinally correct. Their scholarship is drawn on by all three readings in the kernel contest, often selectively.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, historians_of_early_christian_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, textually-grounded account of God's unity that avoids the technical metaphysical apparatus (essence/person distinctions) required to hold simultaneous full deity of Father, Son, and Spirit while affirming monotheism — solving a real interpretive problem for readers who find that apparatus either unscriptural or philosophically unmotivated.
% TRANSFER_FUNCTION: Moves doctrinal and institutional legitimacy away from creedal hierarchies and their credentialing monopoly, and toward congregational or individual scriptural interpretation; where enforced by hostile authorities, moves social standing, sacramental access, and sometimes civil standing away from those who hold it.
% ABSENT_VOICES: Trinitarian and modalist reading-holders are not stakeholders within this story (each reading is authored separately per the kernel-decomposition rule), but their objection — that the unitarian reading collapses distinctions the biblical text itself requires — is the live counter-pressure this reading is structured against.
% DISAPPEARANCE_RATIONALE: If the unitarian reading vanished as a live doctrinal position, the entire fourth-century Arian/subordinationist controversy loses its counter-pole, ecumenical creeds lose their primary historical rival, and modern Unitarian, Socinian, Christadelphian, and Jehovah's Witness communities lose their doctrinal identity — congregations reorganize, institutional excommunication machinery built partly in response to this reading becomes vestigial, and centuries of polemical literature lose their target.
% FOUNDING_PROBLEM: How to affirm that God is numerically one (as the Shema and repeated biblical monotheistic statements assert) while accounting for the New Testament's exalted language about Jesus and the Spirit, without introducing metaphysical categories (essence, person, hypostatic union) that many early readers found absent from or in tension with the plain text.
% FOUNDING_PROBLEM_CORROBORATION: Historians of early Christian doctrine (a seat outside both the unitarian congregational leadership and the creedal institutions) attest that subordinationist and monarchian readings predate and run parallel to the Nicene settlement, and that the interpretive problem — how NT Christology relates to Jewish monotheism — remains a genuine textual-historical question independent of which side is judged correct; this corroboration comes from scholarship, not from either party's own apologetics.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is authored moderate-high: the reading itself is a genuine interpretive coordination device for its holders (low extraction at that level), but the historical operation of the constraint — its role as grounds for excommunication, civil penalty in trinitarian-established polities, and doctrinal warfare — imposes real costs on both the institutions it challenges and the individuals who hold it under hostile regimes. Suppression (0.72) is authored high and reflects TWO directions of coercive pressure simultaneously: creedal institutions have historically suppressed unitarian holders (councils, exile, execution in some jurisdictions), while unitarian congregational structures themselves exert real internal pressure to conform once adopted locally. Theater ratio (0.4) reflects that a substantial share of the ongoing doctrinal contest is maintained by polemical and credentialing activity (heresiological literature, denominational boundary-policing) rather than live textual engagement. Accessibility collapse (0.45) is moderate — once inside a unitarian congregational tradition, alternative readings are not fully foreclosed the way accessibility collapses within a hard creedal monopoly, but resistance (0.75) is high because both trinitarian institutions and unitarian holders themselves are highly invested in defending their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian congregational leaders and lay readers sit toward the beneficiary end: the reading subsidizes their interpretive autonomy and congregational legitimacy without requiring submission to an external creedal hierarchy. Creedal orthodoxy institutions and trinitarian clergy sit toward the target end structurally in the sense that this reading's persistence is a standing challenge to their legitimacy claim, though their institutional power gives them strong means to resist (hence 'arbitrage' exit for the institutions, even though 'constrained' for individual clergy whose careers are locked in). Excommunicated unitarian believers sit at the extreme target end: they bear direct, often severe, costs (loss of sacramental standing, civil penalty, execution in historical cases) with no institutional power to resist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling strict monotheism with exalted NT language about Christ and the Spirit without metaphysical apparatus many found unscriptural — remains a live textual-historical question (status: contested, not dead), which prevents this reading from being dismissed as pure mandatrophy or as a pure relic. What has clearly drifted is the INSTITUTIONAL apparatus built around suppressing or defending it: fourth-century imperial enforcement of Nicene orthodoxy, medieval and early-modern anti-Unitarian statutes, and denominational excommunication machinery persist as enforcement infrastructure long after the immediate political conditions (a fractured Roman imperial church needing doctrinal uniformity for political cohesion) that generated the sharpest suppression have receded. This is exactly the divergence the framework is built to register: the coordination function (a live interpretive question) and the extraction/suppression apparatus (excommunication, civil penalty, career-ending heresy charges) are separable, and the metrics track the apparatus while the founding_problem status tracks the question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitarian_reading_of_kernel_biblical_divine_nature,
    'This constraint instantiates the unitarian reading of the biblical_divine_nature kernel — numerical monotheism read strictly, with Father alone identical to God. What would the sibling readings (trinitarian_reading: three hypostases/one ousia; modalist_reading: sequential modes of one person) change structurally if adopted instead?',
    'Not empirically resolvable — this is a doctrinal/interpretive commitment, not a factual dispute settleable by further textual or historical data alone, though textual-historical scholarship narrows the plausible readings of specific passages.',
    'Adopting the trinitarian reading instead would flip the victim set entirely: creedal institutions become the coordination-preserving beneficiary, and this reading''s own holders become the excluded/suppressed party. Adopting the modalist reading would produce yet a third victim/beneficiary structure (modalism was itself independently condemned by both trinitarian and many subordinationist authorities). The disagreement is located in whether ''God'' in monotheistic confession denotes a single person (unitarian, modalist) or a single essence instantiated in three persons (trinitarian) — this is the specific structural element the readings differ on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unitarian_reading_of_kernel_biblical_divine_nature, conceptual, 'This constraint is one reading of a contested kernel; the sibling readings are separate constraint files, not part of this one''s classification.').

omega_variable(
    natural_reading_vs_theological_development,
    'Is the unitarian reading the more natural or original reading of the biblical text prior to later philosophical development, or is the trinitarian settlement itself the outworking of implications already present in the earliest texts?',
    'Comparative philological and historical-critical analysis of first- and second-century Christian and Jewish-Christian sources, weighed against the theological-development thesis (that trinitarian doctrine articulates rather than adds to apostolic faith); this is contested terrain where confessional commitment shapes evidentiary weighting on both sides.',
    'If the unitarian reading is judged the more original, the creedal apparatus built to suppress it functions more clearly as an imposed extraction structure over an earlier authentic coordination. If theological development is judged legitimate doctrinal unfolding, the suppression apparatus is more defensible as protecting a genuine (if later-articulated) truth rather than extracting from a prior settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_reading_vs_theological_development, conceptual, 'Whether unitarian monotheism or trinitarian doctrine represents the earlier/more authentic reading is itself contested and shapes how the suppression apparatus is judged.').

omega_variable(
    suppression_mechanism_institutional_vs_internalized,
    'Where unitarian believers today hold this reading within traditions that have long faced marginalization, is the low institutional standing they accept primarily externally imposed (denial of ecumenical recognition, exclusion from interfaith bodies) or partly internalized (self-marginalization, minority-identity accommodation)?',
    'Comparative study of unitarian communities operating under different levels of external institutional hostility — do internal markers of marginalization (self-description as ''heretical,'' defensive theological posture) persist even where external hostility has substantially receded (e.g., in modern pluralistic, disestablished contexts)?',
    'If internalized, the effective suppression these communities carry is higher than current external institutional pressure alone would suggest, and the constraint''s resistance metric should weight historical memory alongside present institutional conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism among unitarian-reading holding communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bibl_tr_t300, biblical_divine_nature__unitarian_reading, theater_ratio, 300, 0.28).
narrative_ontology:measurement(bibl_tr_t600, biblical_divine_nature__unitarian_reading, theater_ratio, 600, 0.33).
narrative_ontology:measurement(bibl_tr_t900, biblical_divine_nature__unitarian_reading, theater_ratio, 900, 0.35).
narrative_ontology:measurement(bibl_tr_t1200, biblical_divine_nature__unitarian_reading, theater_ratio, 1200, 0.37).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.39).
narrative_ontology:measurement(bibl_tr_t1700, biblical_divine_nature__unitarian_reading, theater_ratio, 1700, 0.4).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bibl_be_t300, biblical_divine_nature__unitarian_reading, base_extractiveness, 300, 0.42).
narrative_ontology:measurement(bibl_be_t600, biblical_divine_nature__unitarian_reading, base_extractiveness, 600, 0.5).
narrative_ontology:measurement(bibl_be_t900, biblical_divine_nature__unitarian_reading, base_extractiveness, 900, 0.53).
narrative_ontology:measurement(bibl_be_t1200, biblical_divine_nature__unitarian_reading, base_extractiveness, 1200, 0.55).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.57).
narrative_ontology:measurement(bibl_be_t1700, biblical_divine_nature__unitarian_reading, base_extractiveness, 1700, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bibl_su_t300, biblical_divine_nature__unitarian_reading, suppression_requirement, 300, 0.55).
narrative_ontology:measurement(bibl_su_t600, biblical_divine_nature__unitarian_reading, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(bibl_su_t900, biblical_divine_nature__unitarian_reading, suppression_requirement, 900, 0.7).
narrative_ontology:measurement(bibl_su_t1200, biblical_divine_nature__unitarian_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(bibl_su_t1700, biblical_divine_nature__unitarian_reading, suppression_requirement, 1700, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.1).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% Three constraint files decompose the single natural-language label 'the nature of God in Christian theology' per the ε-invariance principle: unitarian_reading (this file, ε=0.58, tangled_rope), trinitarian_reading (separate file, expected higher institutional coordination but also higher enforcement extraction historically), and modalist_reading (separate file, condemned by both other readings, likely highest suppression/lowest institutional standing). Each reading names its own beneficiaries, victims, and coordination function; none averages over the others. Network edges are declared bidirectionally in spirit — each reading's persistence structurally pressures the others' legitimacy conditions and available institutional resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
