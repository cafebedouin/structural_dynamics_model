% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Rupture Hermeneutic as Operative Magisterial Constraint
 *   domain: ecclesiological/institutional-history/hermeneutical
 *
 * SUMMARY:
 *   This story instantiates the RUPTURE READING of the kernel
 *   vatican_ii_magisterial_authority: the claim that Vatican II constitutes a
 *   fundamental break with the pre-conciliar Church, that the texts encode a
 *   new ecclesiology incompatible with prior teaching, and that this
 *   incompatibility is acknowledged doctrinal progress. As an operative
 *   constraint, the rupture reading governs how conciliar texts bind: it
 *   authorizes radical implementation (wholesale liturgical reconstruction
 *   rather than incremental adaptation), declares pre-conciliar positions
 *   (error-has-no-rights, the pre-conciliar religious-liberty teaching)
 *   superseded, and legitimates experimentation as development. The epsilon
 *   referent is the standing arrangement under contest - the post-conciliar
 *   magisterial authority regime as the rupture reading operates it -
 *   assessed by the rupture reading's own lights: even a self-aware rupture
 *   advocate must count the displacement of a formed minority (their liturgy,
 *   institutions, and scholarly legitimacy) alongside the reading's genuine
 *   coordination achievement. CONSTRAINT FAMILY: this is one of three linked
 *   stories decomposing the colloquial label 'what Vatican II is.' The
 *   continuity_reading sibling shares the referent but attributes the
 *   displacement to a misreading of the texts, authoring materially lower
 *   epsilon; the composite_overdetermination_reading sibling finds no
 *   determinate victim/beneficiary structure and hedges epsilon across
 *   sub-readings, which is why it decomposes further. The epsilon values
 *   differ because the readings differ - not because the observable varies
 *   within one constraint. The claim/metrics gap is deliberate: claimed_type
 *   states my structural judgment (tangled_rope: genuine coordination
 *   function plus asymmetric extraction under active enforcement); the
 *   metrics state descriptive operation; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - conciliar_implementation_apparatus: agenda_setter (institutional/mobile) - translates the rupture reading into enforceable programs (missal, calendar, formation curricula)
 *   - liturgical_reform_establishment: primary beneficiary (institutional/mobile) - receives authority over vernacular liturgy; the extraction's gains demonstrably accrue here
 *   - progressive_theology_faculties: beneficiary (institutional/mobile) - legitimation and career space for discontinuity historiography
 *   - national_bishops_conferences: beneficiary/agenda_setter (institutional/constrained) - collect enlarged jurisdiction while administering implementation nationally
 *   - ecumenical_interfaith_offices: beneficiary (organized/constrained) - exist only because the DH reversal is framed as authoritative change
 *   - traditionalist_clergy: primary target (organized/identity_locked) - formed practice displaced; refusal met with suspension or expulsion
 *   - traditionalist_laity: target (moderate/identity_locked) - absorb rite replacement, parish reorientation, catechetical rupture
 *   - continuity_theologians: target (institutional/constrained) - chairs, journals, and synod seats closed during rupture dominance
 *   - ordinary_parish_laity: dual beneficiary/payer (moderate/constrained) - receive vernacular access and ecumenical openness; absorb disruption without a consultative seat
 *   - pre_conciliar_devotional_leadership: excluded (organized/trapped) - organizational world dissolved; no seat in any implementation body
 *   - cdf_doctrine_office: analytical observer (institutional/analytical) - adjudicates boundaries on both flanks and eventually names the rupture hermeneutic erroneous
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.52).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.66).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Rupture Hermeneutic as Operative Magisterial Constraint").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiological/institutional-history/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'fb494ee4-014f-4696-ac28-7c0c2d3f7fc1').
narrative_ontology:cs_kernel_codification('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', fixed_text).
narrative_ontology:cs_authority_grounding('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', lineage).
narrative_ontology:cs_interpretation_layer_present('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1').
narrative_ontology:cs_reading_relation('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', foundational, dh_contradicts_prior_irreformable_teaching).
narrative_ontology:cs_axiom_status(dh_contradicts_prior_irreformable_teaching, holdable).
narrative_ontology:cs_axiom_grounding('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', dh_contradicts_prior_irreformable_teaching, empirically_contingent).
narrative_ontology:cs_axiom('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', foundational, conciliar_break_is_doctrinal_progress).
narrative_ontology:cs_axiom_status(conciliar_break_is_doctrinal_progress, holdable).
narrative_ontology:cs_axiom_grounding('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', conciliar_break_is_doctrinal_progress, theological).
narrative_ontology:cs_reference_frame('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', council_as_new_founding_moment).
narrative_ontology:cs_drift_state('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', post_hermeneutic_of_continuity_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fb494ee4-014f-4696-ac28-7c0c2d3f7fc1', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liturgical_reform_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theology_faculties).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, ecumenical_interfaith_offices).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, continuity_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, ordinary_parish_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, ordinary_parish_laity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, historical_critical_exegesis_supremacy).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, development_of_doctrine_discontinuity_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, aggiornamento_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The post-conciliar bodies that translated the rupture reading into enforceable programs: the Consilium and its successors, diocesan liturgy commissions, episcopal-conference bureaucracies, and formation authorities. They decided what counted as legitimate implementation, drafted the new missal and calendar, set seminary curricula, and administered the transition. Personnel circulate through curia, universities, and dioceses, so exit from any single post is easy; the apparatus as a whole owns the standard it enforces.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, conciliar_implementation_apparatus, agenda_setter,
    institutional, generational, mobile, global).

% Translation bodies, national liturgical offices, and liturgical publishers. They received authority over vernacular texts and ritual form as inherited liturgy was displaced, converting that displacement into permanent institutional position: staffing, edition mandates, consulting contracts, and control of the worship books every parish uses. Their position exists because the rupture reading authorized reconstruction rather than adaptation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liturgical_reform_establishment, beneficiary,
    institutional, generational, mobile, global).

% Theology faculties and their scholars, freed by the rupture frame from neoscholastic censure norms. Historical-critical reconstruction of doctrine became legitimate work; discontinuity historiography became a career track with journals, chairs, and conference circuits. Mobility across secular and pontifical institutions is high; the frame's legitimation is what their publication economy runs on.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theology_faculties, beneficiary,
    institutional, biographical, mobile, continental).

% Diocesan and pontifical dialogue offices whose existence depends on the religious-liberty reversal being authoritative change rather than error. Joint declarations, bilateral dialogues, and interreligious relationships are organized under the rupture-framed reading of Dignitatis Humanae; if the reversal were reclassified as discontinuity-with-error, their warrant dissolves. Exit is constrained because their function has no home outside the frame that authorizes it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecumenical_interfaith_offices, beneficiary,
    organized, generational, constrained, global).

% Conferences collected enlarged jurisdiction over worship, catechesis, and discipline from collegiality readings the rupture frame amplified, while administering implementation nationally - approving translations, regulating practice, managing dissent. They are dual-positioned: they collect authority and they run the enforcement locally. Bound to territory and communion, they cannot relocate their function.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences, agenda_setter).

% Priests formed in the pre-conciliar rites saw their liturgy replaced wholesale by legislation they had no seat in shaping. Refusal carried suspension or expulsion; the Lefebvre movement's refusal of implementation ended in the 1988 excommunications, while compliant continuance institutes operate under precarious canonical arrangements granted and revocable at will. Their identity fuses with the old form: leaving it means abandoning the priesthood as they understand it, so exit is not a practical option but a self-dissolution.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    organized, generational, identity_locked, global).

% Lay communities attached to inherited devotion and liturgy absorbed rite replacement, parish reorientation, and catechetical rupture. They petitioned for indults, sustained underground and later indult-era chapels, and migrated to traditionalist institutes when space allowed. Attachment to the old form constitutes their Catholic self-understanding; the available exits - endure marginalization, migrate to precarious institutes, or leave the Church - each carry identity cost that makes the constraint's pull structural rather than preferential.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity, payer,
    moderate, generational, identity_locked, regional).

% Scholars defending development-without-contradiction found chairs, journal space, and synod invitations closed during the rupture-dominant decades; their work was recast as nostalgia or pre-conciliar residue. They remained inside the system - their vocation and audience are ecclesial - and worked through the 1985 Extraordinary Synod and subsequent papacies to regain standing. Exit would mean taking the dispute to an audience with no authority to settle it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, continuity_theologians, payer,
    institutional, biographical, constrained, continental).

% Received vernacular liturgy, direct scriptural access, and ecumenical openness - genuine gains. Absorbed abrupt liturgical change, loss of inherited devotional practice, catechetical discontinuity, and declining communal density - real costs. Held no consultative seat in any implementation body; their participation was assumed rather than solicited. Cultural and familial ties to parishes constrain exit without fusing identity to either liturgical form.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ordinary_parish_laity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, ordinary_parish_laity, payer).

% Leaders of sodalities, confraternities, guilds, and devotional associations whose organizational world dissolved as parishes reoriented around the new liturgical forms. They would have argued for transition designs preserving devotional continuity - gradual introduction, parallel forms, protected guild structures - but held no seat in the Consilium, the conference liturgy commissions, or the synods. Their organizations lacked anywhere to go: the parish structure that hosted them was the thing being rebuilt.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_devotional_leadership, excluded,
    organized, biographical, trapped, regional).

% Adjudicates boundaries on both flanks: disciplined rupture-excess theologians in the Curran and Kung cases, sanctioned the Lefebvre movement's schism, and eventually declared the rupture hermeneutic itself erroneous in the 1985 Synod relatio and the 2005 Christmas address naming the hermeneutic of discontinuity. Takes testimony from every seat, commissions historical study, and controls the canonical instruments that raise or lower the enforcement ceiling for all parties.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, cdf_doctrine_office, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, liturgical_reform_establishment).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the conciliar texts' deliberate ambiguities into a single implementable program: a determinate reading of Dignitatis Humanae against the prior religious-liberty teaching, a mandate for liturgical reconstruction rather than incremental adaptation, and a shared standard across dioceses and faculties for what counts as faithful implementation. Without some such standard, implementation fragments into local improvisation and the reforming coalition cannot act in concert.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional position from the pre-conciliar apparatus - Tridentine formation networks, neoscholastic faculties, devotional infrastructure - to the post-conciliar reforming class: liturgical commissions, rebuilt theology faculties, conference bureaucracies. Moves liturgical and devotional practice from inherited forms to newly composed ones, and concentrates the transition's costs on those formed in the prior paradigm.
% ABSENT_VOICES: The Council Fathers' majority, who understood their work as development within continuity (attested by the 1985 Extraordinary Synod's unanimous rejection of the rupture hermeneutic), are historically unseated - dead or retired, their interpretive intent enters only as citation. Rank-and-file laity of the implementation decades held no consultative mechanism. Eastern Catholic hierarchs, promised preservation of their traditions, watched further Latinization proceed without remedy. Pre-conciliar devotional leadership (stakeholder: pre_conciliar_devotional_leadership) is the seated representative of this exclusion class. All of these object from outside the room, in the historical record, petition archives, and the eventual official counter-hermeneutic.
% DISAPPEARANCE_RATIONALE: Institutions organized around the rupture reading - liturgy offices, discontinuity-oriented faculties, ecumenical bureaucracies warranted by the reversal-framing of Dignitatis Humanae - would lose their authorizing standard overnight and reorganize around a competing reading of the same texts; traditionalist communities would gain immediate relief from enforcement; the liturgical establishment's translation and edition mandates would lose their justification. The kernel itself (the conciliar texts' binding authority) would persist under the sibling readings, so the rearrangement is institutional rather than civilizational - but for the named seats, arrangements demonstrably depend on this reading.
% FOUNDING_PROBLEM: The rupture reading was built to solve the implementation crisis of deliberately compromised conciliar texts: how to reconcile Dignitatis Humanae with Quanta Cura's error-has-no-rights teaching if continuity with prior irreformable magisterium is mandatory; how to justify wholesale liturgical reconstruction if the Council merely permitted adaptation; how to authorize reforming intellectuals to build anew rather than renovate. Declaring the break real and progressive converts textual ambiguity into mandate.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Bologna-school historians (academic observers, not liturgical-office beneficiaries) corroborate that the implementation crisis and the texts' internal tensions were real, while stopping short of endorsing rupture as the texts' exclusive meaning. Sociologists of religion corroborate the pastoral-dislocation premise with participation and practice data. Inside the Church but outside the rupture beneficiary set, the 1985 Extraordinary Synod attested the crisis was real while unanimously rejecting rupture as its solution - corroboration of the problem, denial of the reading. No source outside the beneficiary set attests that rupture is the only resolution of the founding problem; several independent seats attest the problems themselves were real.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: substantial but below snare range - the reading transferred interpretive authority, institutional position, and formation control from the pre-conciliar apparatus to the reforming class, while the reading itself classifies the displaced minority's losses as developmental cost rather than rent; the omega extraction_versus_collateral_cost tests whether that self-classification survives tracing the gain flows. Suppression 0.66: real coercion - rite replacement by legislation, canonical sanctions culminating in the 1988 excommunications, seminary and hiring gates - but episodic rather than totalizing, with indult periods relaxing it. Theater_ratio 0.27: implementation activity was mostly functional (new missals, new curricula, real institutional construction); theatricality concentrated in 'spirit of the Council' rhetoric detached from the texts and in anniversary/synodal consensus performance. Accessibility_collapse 0.30: alternatives do NOT collapse - the continuity reading is not merely available but is now the official magisterial framing, and the composite reading thrives in academic historiography; understanding the rupture reading forecloses nothing for its rivals. Resistance 0.72: massive, organized, durable, and partially successful (indults, Summorum Pontificum, the 1985 Synod's unanimous rejection of the hermeneutic). SUPPRESSION NOTE: suppression is authored as a raw structural property and is not scaled by power or scope - only extractiveness is scaled, by directionality and scope, in the engine's computation. MEASUREMENT DESIGN: all three tracked series run on one shared seven-point grid (every metric authored at every examined time point); the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change - it oscillates with papal administrations (rise through the imposition decade, peak at the Lefebvre confrontation, relaxation under the indult and Summorum Pontificum machinery, renewed rise under Traditionis Custodes). The oscillation is administrative-cycle-driven, not intermittent reinforcement: each swing reflects which hermeneutic the enforcement apparatus served, and the cycle's amplitude is itself evidence that the constraint's persistence depends on capturing the enforcement machinery rather than on voluntary uptake.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute very different types from identical structural data. From the traditionalist clergy and laity seats, the constraint operates as enforced dispossession of a formed identity: the liturgy they were ordained or raised in was replaced by legislation, refusal carried canonical penalty, and exit is identity-fused (leaving the old form means leaving the priesthood or the community's constitutive practice - relational and ideological fusion, not mere preference). From the liturgical establishment and theology faculty seats, the same structure operates as long-delayed authorization: release from neoscholastic censure norms and from liturgical immobilism, with careers and offices built on the new standard. From the implementation apparatus seat, it is necessary governance of a delicate transition. Ordinary parish laity sit near symmetric - genuine gains (vernacular scripture access, ecumenical openness) against real costs (abrupt change, catechetical discontinuity) with no consultative seat. The engine computes this divergence; the authored claim does not adjudicate it. If the traditionalist identity frame broke - if attachment to the pre-conciliar form ceased to constitute Catholic self-understanding - exit would widen, target-side effective extraction would fall, and the constraint's enforcement burden would drop sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the four collecting seats: the liturgical establishment (receives translation and ritual authority), theology faculties (receive legitimation and career space), ecumenical offices (exist by virtue of the DH reframing), and bishops' conferences (collect enlarged jurisdiction while administering implementation). Victim declarations drive high directionality for traditionalist clergy and laity (bear the transfer directly, identity-locked exit amplifies their effective extraction) and continuity theologians (bear delegitimation with constrained, vocation-bound exit). Ordinary parish laity carry a dual declaration and derive near-symmetric directionality. The excluded devotional leadership bears costs but holds no seat - their exclusion is the enforcement object's shadow, not a directionality input. No directionality overrides are authored: the beneficiary/victim declarations plus exit-option differentiation cleanly separate every seat, so the structural derivation chain suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents symmetric mislabeling. Reading the rupture frame as pure snare erases its genuine coordination achievement: the conciliar texts were deliberately ambiguous compromises, and SOME determinate reading was structurally necessary for any coordinated implementation at all - the rupture reading supplied one that aligned Dignitatis Humane with the constitutional religious-liberty settlement and gave dispersed reformers a shared standard. Reading it as pure rope erases the documented dispossession: a formed minority lost its liturgy, its institutions, and its scholarly standing through the same structure that enriched the reforming class, under active enforcement. Mandatrophy status: the founding problem (implementation crisis of compromised texts) is CONTESTED, not dead - rupture advocates attest it demanded rupture-scale solutions; continuity advocates attest it was solvable within development. Accordingly mandatrophy_resolved is NOT declared, and the R5 mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges: no dead-mandate zombie flag fires, correctly - the constraint's mandate is disputed, not lapsed. The piton path is also closed by the receipt surface: gains demonstrably accrue to a named seat, so this is not an unowned inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel vatican_ii_magisterial_authority (reading: rupture_reading). At which specific structural element do the sibling readings disagree, and what would adopting a sibling change?',
    'Locate the disagreement precisely: it sits in the semantic relation between the conciliar texts (especially Dignitatis Humanae and the liturgical constitution) and the prior irreformable magisterium - real contradiction versus development-without-contradiction. The continuity_reading sibling instantiates a constraint with a smaller victim set (displacement attributed to misreading rather than to the texts'' meaning) and materially lower epsilon; the composite_overdetermination_reading sibling refuses a determinate victim/beneficiary structure altogether and hedges epsilon across sub-readings.',
    'If the disagreement resolves toward continuity, this story''s victim set reframes as casualties of a misreading rather than of the texts'' authority, and epsilon falls; if it resolves toward rupture, the current official counter-hermeneutic becomes itself the extractive layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    epsilon_configuration_indexing,
    'Is the authored epsilon stable across the interval, or is it indexed to the rupture-dominant configuration of roughly years 5-25?',
    'Track the reading''s operative share of dioceses, faculties, and liturgical bodies against the enforcement series: if the operative share keeps shrinking under the official continuity hermeneutic, epsilon trends toward a historical-curiosity floor; if Traditionis Custodes-line enforcement generalizes and rupture-aligned coercion consolidates, epsilon rises again.',
    'A shrinking operative share would date the constraint''s effective decline earlier than the scalar end-state suggests; a consolidating share would support reclassification pressure toward snare flavor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_configuration_indexing, empirical, 'Whether epsilon describes the whole interval or only the rupture-dominant configuration.').

omega_variable(
    extraction_versus_collateral_cost,
    'Are the losses borne by traditionalist clergy, traditionalist laity, and continuity theologians TRANSFER to identifiable beneficiaries, or collateral cost of a reform nobody captured?',
    'Trace gain flows against displacement timing: translation and edition contracts, liturgical-office staffing, faculty lines, and conference jurisdiction expansions that materialized contemporaneously with the displacement of inherited forms indicate transfer; absence of contemporaneous gain capture indicates collateral cost.',
    'If the losses are substantially collateral, the constraint slides from tangled_rope toward rope-with-heavy-costs; if transfer dominates, snare flavor increases and the coordination story weakens toward cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_collateral_cost, conceptual, 'Whether victim losses constitute extraction-transfer or reform collateral.').

omega_variable(
    suppression_structural_vs_identity_sustained,
    'How much of the measured suppression is structural (canonical penalties, rite restriction, hiring and formation gates) versus sustained by identity fusion among the targets themselves?',
    'Post-Summorum-Pontificum demand persistence: when enforcement relaxed (2007-2021), traditionalist attachment did not decay proportionally - attendance and institute growth persisted without coercive maintenance. Persistence without enforcement indicates an internalized component; measure the decay constant of attachment under zero enforcement.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after enforcement removal, raising the constraint''s true suppressive footprint; if enforcement explains most variance, the structural measure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_identity_sustained, empirical, 'Structural versus identity-fusion components of the suppression profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rupture_reading_tr_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(rupture_reading_tr_t0, observed).
narrative_ontology:measurement(rupture_reading_tr_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(rupture_reading_tr_t10, observed).
narrative_ontology:measurement(rupture_reading_tr_t20, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(rupture_reading_tr_t20, observed).
narrative_ontology:measurement(rupture_reading_tr_t30, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(rupture_reading_tr_t30, observed).
narrative_ontology:measurement(rupture_reading_tr_t40, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(rupture_reading_tr_t40, observed).
narrative_ontology:measurement(rupture_reading_tr_t50, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(rupture_reading_tr_t50, observed).
narrative_ontology:measurement(rupture_reading_tr_t60, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(rupture_reading_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(rupture_reading_be_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(rupture_reading_be_t0, observed).
narrative_ontology:measurement(rupture_reading_be_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(rupture_reading_be_t10, observed).
narrative_ontology:measurement(rupture_reading_be_t20, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(rupture_reading_be_t20, observed).
narrative_ontology:measurement(rupture_reading_be_t30, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(rupture_reading_be_t30, observed).
narrative_ontology:measurement(rupture_reading_be_t40, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(rupture_reading_be_t40, observed).
narrative_ontology:measurement(rupture_reading_be_t50, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement_basis(rupture_reading_be_t50, observed).
narrative_ontology:measurement(rupture_reading_be_t60, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(rupture_reading_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(rupture_reading_su_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(rupture_reading_su_t0, observed).
narrative_ontology:measurement(rupture_reading_su_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(rupture_reading_su_t10, observed).
narrative_ontology:measurement(rupture_reading_su_t20, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(rupture_reading_su_t20, observed).
narrative_ontology:measurement(rupture_reading_su_t30, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(rupture_reading_su_t30, observed).
narrative_ontology:measurement(rupture_reading_su_t40, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(rupture_reading_su_t40, observed).
narrative_ontology:measurement(rupture_reading_su_t50, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(rupture_reading_su_t50, observed).
narrative_ontology:measurement(rupture_reading_su_t60, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(rupture_reading_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, information_standard).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel vatican_ii_magisterial_authority per the epsilon-invariance principle. The colloquial label 'what Vatican II is relative to prior teaching' covers three structurally distinct claims with distinct epsilon values, victim sets, and failure modes: rupture_reading (this file - incompatibility acknowledged as progress; substantial extraction from a formed minority under active enforcement), continuity_reading (development without contradiction; displacement attributed to misreading; materially lower epsilon), and composite_overdetermination_reading (no determinate single meaning; hedged, unstable epsilon requiring further decomposition). Upstream/downstream: rupture historiography (the Bologna school's inventory of textual tensions) supplies raw material the composite reading systematizes, while the official continuity hermeneutic exerts reverse pressure on both. Each family member links the others via affects_constraints; orphaning any member would hide the contamination paths along which a verdict on one reading propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
