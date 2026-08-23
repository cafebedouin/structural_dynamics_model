% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Decalogue Image Prohibition — Iconoclast Reading (Categorical Ban on All Religious Imagery)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   Between 726 and 843 CE the Byzantine imperial state enforced a
 *   categorical ban on religious imagery: icons were removed from churches,
 *   the painters' trade was criminalized, and the image-bearing monastic
 *   communities anchoring popular devotion were stripped, garrisoned, and
 *   persecuted. This story instantiates the ICONOCLAST READING of the
 *   decalogue_image_prohibition kernel — the claim that any material
 *   representation used in worship violates the commandment and constitutes
 *   idolatry — as that reading stood when it was the enforced law of the
 *   empire. It is one member of a three-story constraint family: the
 *   iconodule reading (worship of images forbidden, honor through images
 *   permitted, matter sanctified by the Incarnation) and the moderate
 *   iconoclast reading (statuary alone prohibited, flat images regulated) are
 *   separate stories with their own epsilon, victim sets, and types, linked
 *   through network edges. Per the epsilon-invariance principle the
 *   colloquial label 'the image prohibition' decomposes; nothing in this file
 *   averages over the siblings. KEY AGENTS (by structural relationship): -
 *   iconoclast_imperial_authority: Agenda-setting beneficiary
 *   (institutional/arbitrage) — issues the edicts, directs enforcement,
 *   absorbs confiscated wealth, and can reverse the policy by decree -
 *   monastic_communities: Primary target (organized/identity_locked) — bear
 *   confiscation, torture, and exile; icon-veneration is fused with vocation
 *   - icon_painter_workshops: Target (moderate/constrained) — principal
 *   product criminalized, patronage withdrawn, migration possible but costly
 *   - icon_venerating_laity: Diffuse target (powerless/constrained) —
 *   household devotion driven underground, no voice in the deciding councils
 *   - iconoclast_clergy_hierarchy: Beneficiary-administrator
 *   (institutional/mobile) — staffs purged offices, administers oaths,
 *   recants on cue at each restoration - eastern_army_loyalists: Enforcement
 *   beneficiary (powerful/mobile) — receives land grants carved from
 *   confiscated monastic estates - papacy_rome: Excluded objector
 *   (institutional/arbitrage) — condemns from beyond coercion reach, breaks
 *   communion, leverages the Frankish alliance -
 *   diaspora_iconophile_theologians: Excluded objector (moderate/arbitrage) —
 *   writes systematic defense from territory the officers cannot reach -
 *   historical_analysis_seat: Analytical observer (analytical/analytical) —
 *   reads the record from outside every party's commitments EPSILON
 *   INDEXATION: base_properties.extractiveness is reading-indexed over the
 *   fixed referent — the standing prohibition arrangement itself — priced by
 *   THIS reading's own lights (OQ-26/OQ-258): from inside the iconoclast
 *   framework the arrangement is obedience to divine command, its costs
 *   disciplinary rather than extractive, hence the low scalar (0.22). The
 *   temporal base_extractiveness series, by contrast, records the operational
 *   trace (confiscations, purges, office transfers) as objectively as the
 *   record permits. The gap between the reading's self-price and the recorded
 *   peak (0.68) is part of what this corpus exists to measure; it is
 *   documented here, not reconciled. CLAIM/METRIC INDEPENDENCE: claimed_type
 *   is 'mountain' because the iconoclast reading presents the prohibition as
 *   categorically fixed divine decree — a summit claim, wall-type in this
 *   family's vocabulary. The metrics describe the enforced operation as the
 *   historical record shows it: heavily suppressed, heavily resisted,
 *   enforcement-dependent, with named beneficiaries and a named capturing
 *   seat. The divergence is deliberate; the engine adjudicates.
 *
 * KEY AGENTS:
 *   - iconoclast_imperial_authority: Agenda-setting beneficiary (institutional/arbitrage) — sets and enforces the ban, receives confiscated estates into the treasury, reverses or revives the policy by dynastic choice
 *   - monastic_communities: Primary target (organized/identity_locked) — lose estates, houses, abbots, and limbs; endurance is fused with vocational identity
 *   - icon_painter_workshops: Target (moderate/constrained) — commissions criminalized; exit runs through exile to Rome, Lombard Italy, or caliphal territory
 *   - icon_venerating_laity: Diffuse target (powerless/constrained) — practice continues by concealment; consent manufactured by acclamation
 *   - iconoclast_clergy_hierarchy: Beneficiary-administrator (institutional/mobile) — inherits purged offices, drafts the theology, performs recantation at each restoration
 *   - eastern_army_loyalists: Enforcement beneficiary (powerful/mobile) — garrisons the monasteries and holds land granted from their confiscation
 *   - papacy_rome: Excluded objector (institutional/arbitrage) — ruptures communion and reroutes protection to the Franks
 *   - diaspora_iconophile_theologians: Excluded objector (moderate/arbitrage) — supplies the resistance's intellectual scaffolding from untouchable ground
 *   - historical_analysis_seat: Analytical observer (analytical/analytical) — retrodicts enforcement intensity and property flows from conciliar, hagiographic, and archaeological traces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.22).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.85).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, mountain).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition — Iconoclast Reading (Categorical Ban on All Religious Imagery)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).
domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '82839c9e-a12c-450f-b7e3-723a3f6b01ec').
narrative_ontology:cs_kernel_codification('82839c9e-a12c-450f-b7e3-723a3f6b01ec', fixed_text).
narrative_ontology:cs_authority_grounding('82839c9e-a12c-450f-b7e3-723a3f6b01ec', extraction).
narrative_ontology:cs_interpretation_layer_present('82839c9e-a12c-450f-b7e3-723a3f6b01ec').
narrative_ontology:cs_reading_relation('82839c9e-a12c-450f-b7e3-723a3f6b01ec', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('82839c9e-a12c-450f-b7e3-723a3f6b01ec', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('82839c9e-a12c-450f-b7e3-723a3f6b01ec', foundational, material_mediation_categorically_idolatrous).
narrative_ontology:cs_axiom_status(material_mediation_categorically_idolatrous, holdable).
narrative_ontology:cs_axiom_grounding('82839c9e-a12c-450f-b7e3-723a3f6b01ec', material_mediation_categorically_idolatrous, theological).
narrative_ontology:cs_axiom('82839c9e-a12c-450f-b7e3-723a3f6b01ec', foundational, incarnation_does_not_license_matter_in_worship).
narrative_ontology:cs_axiom_status(incarnation_does_not_license_matter_in_worship, holdable).
narrative_ontology:cs_axiom_grounding('82839c9e-a12c-450f-b7e3-723a3f6b01ec', incarnation_does_not_license_matter_in_worship, theological).
narrative_ontology:cs_reference_frame('82839c9e-a12c-450f-b7e3-723a3f6b01ec', eternal_total_image_decree).
narrative_ontology:cs_drift_state('82839c9e-a12c-450f-b7e3-723a3f6b01ec', post_triumph_of_orthodoxy_843, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('82839c9e-a12c-450f-b7e3-723a3f6b01ec', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_hierarchy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, eastern_army_loyalists).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_painter_workshops).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_venerating_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the edicts against religious images, appoints and dismisses patriarchs, convenes the councils that ratify the ban, and directs the soldiers and officials who strip images from churches. Receives confiscated monastic estates into the imperial treasury and grants them onward to loyal commanders. Holds the policy as a standing choice rather than a settled fact: successors reversed it by decree in 787 and revived it in 814, and the throne's coalition calculus, not the theology, timed both turns.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_imperial_authority, beneficiary).

% Staffs the bishoprics and patriarchal offices vacated when image-defending clergy are purged. Drafts the theological defense of the ban, administers loyalty oaths, and prescribes liturgies stripped of visual veneration. Careers advance through conformity; at each restoration a number of its members publicly recanted and kept office under the returning order.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_hierarchy, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_hierarchy, agenda_setter).

% Supply the armed force behind the ban — garrisoning monasteries, escorting confiscations, breaking resistance in the themes. Receive landed grants carved from confiscated monastic estates, binding their households' fortunes to the policy's survival. Commanders who backed a losing side at a restoration saw those grants clawed back.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, eastern_army_loyalists, beneficiary,
    powerful, immediate, mobile, regional).

% Keep the great image-bearing houses of Bithynia, the Anatolian interior, and the capital; their liturgy and interior life are built around icons. Under the ban their estates are seized, their houses converted to barracks and stables, their abbots flogged, exiled, or killed. Abandoning the habit would spare the body but dissolve the community they understand themselves to be; remaining means persecution. Some endure torture rather than hand over painted panels they regard as windows into heaven.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, identity_locked, continental).

% Live by ecclesiastical commission — panel icons, frescoes, mosaic figures. The ban criminalizes their principal product; commissions shift to crosses, ornament, and secular subjects at lower margins. Migration to Rome, Lombard Italy, or caliphal territory keeps the craft alive but severs workshop, guild, and family ties; staying means renouncing the trade that names them.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_painter_workshops, payer,
    moderate, biographical, constrained, regional).

% Keep household icons, kiss them, light lamps before them, and organize parish life around feast-day images. Outward conformity is available — attending imageless services, hiding panels at home — and the practice persists underground, handed down through families. They hold no corporate voice in the councils that decide the matter; their consent enters the record as acclamation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_venerating_laity, payer,
    powerless, biographical, constrained, continental).

% Condemns the ban from beyond the reach of imperial coercion, refuses communion with the iconoclast patriarchate, and appeals to the Frankish court for protection, eventually transferring its political reliance northward. Its protests carry weight in the west but enter the imperial conversation only as nuisance: the councils that legislate the ban never seat Roman legates with deciding voice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, papacy_rome, excluded,
    institutional, generational, arbitrage, continental).

% Write the systematic defense of images from monasteries and courts outside imperial jurisdiction — Damascus, the Levant, Lateran Rome. Imperial officers cannot seize them; their tracts circulate secretly inside the empire and give the resistance its intellectual scaffolding. They hold no office whose recognition the ban requires, so nothing about them can be revoked.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, diaspora_iconophile_theologians, excluded,
    moderate, biographical, arbitrage, continental).

% Reads the surviving record — conciliar acts, hagiography, chronicles, seals, and the archaeology of overwritten mosaics — from outside every party's commitments. Retrodicts enforcement intensity, property flows, and the sequence of reversals; attributes nothing and collects nothing.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, historical_analysis_seat, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, iconoclast_imperial_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Inside the iconoclast frame, solves the problem of how a war-battered polity collectively answers military catastrophe interpreted as divine anger: it names one actionable sin — image-veneration — and coordinates its removal across every church under a single imperial standard. It simultaneously replaces monastery-mediated sanctity with a uniform, centrally legible worship form, giving scattered garrison populations a shared loyalty practice independent of monastic networks.
% TRANSFER_FUNCTION: Moves land, treasure, offices, and religious authority itself: confiscated monastic estates flow to the treasury and thence to loyal commanders; bishoprics and abbacies flow to conformist clergy; the practical monopoly on mediating the holy shifts from image-bearing monasteries to the imperial-chancery church.
% ABSENT_VOICES: The pentarchial sees outside imperial control (Rome, Jerusalem, Alexandria, Antioch), the icon-venerating laity who acclimate rather than assent, the painters whose trade is legislated away without consultation, and the image-defending clergy purged before the ratifying councils convened. Unanimity at Hiera in 754 and at the later iconoclast councils was produced by seating only the conforming.
% DISAPPEARANCE_RATIONALE: Tested empirically twice: at each restoration (787, 843) icons returned to every church within months, estates and offices were reclaimed or exchanged hands again, iconoclast clergy were deposed or performed recantation, and the army's grant economy inverted — the arrangement's disappearance visibly rearranged property, careers, liturgy, and imperial legitimacy each time it occurred.
% FOUNDING_PROBLEM: Seventh-century military catastrophe — the loss of Syria, Egypt, and Carthage, and the sieges of Constantinople itself — read in Leo III's circle as divine punishment for idolatrous image-veneration, compounded by the need to integrate large numbers of eastern conscripts from image-hostile regions into a unified imperial church.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: Pope Gregory II's letters to Leo III dispute the ban's premise while acknowledging the military crisis; the acts of Nicaea II (787) record the restoration consensus of bishops who had lived under the ban; the chronicle of Theophanes, cross-checked against Syriac and Arabic sources, dates the crisis framing independently. No source sympathetic to the iconoclast beneficiaries independently attests that the founding problem stayed live past the crises — and the iconoclast conciliar acts survive chiefly through their opponents' quotations, which is itself evidence about who controlled the record.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, ExtMetricName, E),
    domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Suppression (0.85) is authored as the raw structural property it is — unscaled by power or scope: enforcement meant troops quartered in monasteries, judicial mutilation, forced oaths, and informant networks. The constraint's own survival curve proves enforcement-dependence: image-practice resumed within months at each lapse of enforcement (787, 843), which no self-sustaining arrangement does. Accessibility collapse (0.62): alternatives to compliance were partly real — concealed household icons, flight to Rome, Lombard Italy, or caliphal lands — but closed to anyone bound to imperial office. Resistance (0.78) is high and sustained across the century: papal rupture, diaspora theology, monastic martyrdom. Theater (0.40): enforcement activity was functionally real, but a persistent minority was staged — the iconoclast councils seated only conformists and manufactured unanimity by roster, and at each restoration the apparatus flipped through mass-performed recantation, which is why the theater series peaks at the transition points rather than at the persecution peaks.
 *   
 *   MEASUREMENT GRID: one shared grid at t = {0, 17, 35, 61, 88, 105, 117}, mapped to 726, c. 743, c. 761, 787, 814, c. 831, and 843 CE; every tracked metric carries a value at every grid point. The series shows two full build-and-dismantle cycles of the suppression machinery. CYCLICAL DYNAMICS: the oscillation was driven by dynastic succession — each new palace coalition re-priced the ban's utility and rebuilt or demolished the enforcement apparatus accordingly. The oscillation is itself an extraction mechanism (intermittent reinforcement): each flip transferred property, offices, and status between factions and punished whichever party had most recently won, so no faction could safely invest in permanence. Rising base_extractiveness segments (t0-t35, t88-t105) feed accumulation hypotheses under T17. The suppression series collapses at the endpoints because the machinery was physically dismantled at each restoration; the scalar suppression (0.85) describes the standing arrangement's force at its operative peak, and the divergence from the terminal series value is the honest record of termination, not an inconsistency. The terminal base_extractiveness value (0.22) coincides with the reading-indexed scalar only incidentally: 843 still extracted through reverse-purges and compelled recantation even as the arrangement died.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat experiences the arrangement as sacral duty and political instrument at once: the emperor prices obedience cheaply because he wrote the price. The paying seats compute a different arrangement from the same structure — for the monastic seat the ban is dispossession of the community's entire mediatory world; for the painter seat, criminalization of livelihood; for the laity, criminalization of inherited devotion. The excluded seats compute a third thing again: Rome and the diaspora theologians read the ban as evidence of imperial apostasy, not as law at all. None of these perspectives is authored; each is computed per seat from power, exit, and role, and the spread across seats is the perspectival measurement this story contributes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the subsidized end for the imperial authority (collects confiscated wealth, army loyalty, and the mediation monopoly), the conformist clergy (collects vacated offices), and the loyalist army (collects land grants) — all three sit far from the target end despite institutional power, because the arrangement subsidizes them. Victim declarations drive the monastic communities, painter workshops, and venerating laity toward the full-target end, with the monastic seat pushed furthest by identity-lock (exit would dissolve the self, not merely cost it) and the laity by powerlessness compounded by constrained exit. Continental spatial scope scales effective extraction upward for the paying seats by making verification and appeal harder; the emperor's arbitrage-grade exit (policy reversible by decree) pulls the agenda-setter seat nearest the beneficiary pole. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the known asymmetries without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification apparatus guards both error directions here. Against summit-credulity: the reading claims a fixed divine wall, but named victims, a named capturing seat, enforcement-dependence proven twice by natural experiment, and cheap reversal available to the agenda-setter are exactly the structural profile the false-summit signature screens for — the beneficiary declaration on a mountain claim routes this story through FSM evaluation rather than letting the naturality claim pass unexamined. Against overcorrection: the arrangement was not mere cover — a sincere theological coalition held it, it solved a live coordination problem inside its own frame, and a pure-extraction label would erase that content; the claimed mountain with extractive metrics and open omegas preserves the ambiguity for per-seat computation instead of pre-adjudicating it. Mandatrophy is declared resolved: the founding problem (crisis-propitiation and eastern army integration) died with the crises that raised it, the second-period revival ran on dynastic legitimacy needs rather than the original function, and the enforcing authority itself terminated the arrangement — the zombie condition the R5 interview flags, cross-checkable against the computed theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_or_construction,
    'Is the total image prohibition a genuine divine commandment binding all Christian worship in all places, or a constructed arrangement whose persistence under enforcement served identifiable beneficiaries?',
    'Pre-726 reception history of Exodus 20:4 and Deuteronomy 5:8 across Greek, Latin, and Syriac Christianity — did the tradition before imperial enforcement ever read the text as a categorical ban on all religious imagery? Plus the counterfactual test: where no imperial enforcement exists, does the prohibition bind practice?',
    'Genuine commandment supports the mountain claim as the reading states it; demonstrated construction confirms the false-summit signature and the arrangement reclassifies toward enforced extraction with theological cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_or_construction, conceptual, 'Whether the prohibition is natural/divine law or a constructed instrument of consolidation (schema-required for a mountain claim with declared beneficiaries).').

omega_variable(
    reading_dispute_location,
    'This constraint is the iconoclast reading of the decalogue_image_prohibition kernel; the iconodule and moderate iconoclast siblings instantiate different constraints. Where exactly is the disagreement located — the semantic range of ''graven image,'' the latria/dulia distinction, or whether the Incarnation confers representational capacity on matter?',
    'Exegetical adjudication of the commandment''s terms against Second Temple and patristic usage, plus conciliar precedent separating what counts as worship from what counts as honor.',
    'If the dispute locates in the latria/dulia line, the sibling readings are separable policies rather than rivals and their victim sets diverge sharply; if it locates in the Incarnation''s material efficacy, the readings are theologically inseparable and the family is one contested boundary wearing three costumes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dispute_location, conceptual, 'Location of the structural dispute among sibling readings of the same kernel (committer structure routed to omega per the kernel-reading rules).').

omega_variable(
    coalition_vs_conviction_persistence,
    'Did the prohibition persist because enforcing coalitions needed it — dynastic legitimacy, army payment, monastic wealth — or because the enforcing elites held the theological conviction?',
    'Compare reigns holding theology constant while coalition incentives vary — Constantine V''s dependence on theme-army loyalty against Leo V''s post-usurpation legitimacy deficit — and measure enforcement intensity against incentive gradients.',
    'Coalition-driven persistence strengthens the extraction reading of the arrangement; conviction-driven persistence strengthens its coordination content and complicates pure-cover accounts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_vs_conviction_persistence, empirical, 'Persistence mechanism: coalition maintenance versus elite conviction.').

omega_variable(
    monastic_identity_lock_depth,
    'Was monastic refusal to surrender images sustained by internalized conviction — identity fused with icon-mediated devotion — or by structural confinement of vow, enclosure, and estate dependence?',
    'Post-843 trajectory: if icon-bearing monasticism flourished immediately and voluntarily once coercion lifted, the lock was internalized rather than structural.',
    'An internalized lock validates the identity_locked exit attribution for the monastic seat and raises the effective suppression the arrangement achieved; purely structural confinement would predict rapid defection after 843, which the record contradicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monastic_identity_lock_depth, empirical, 'Structural versus internalized component of monastic endurance under the ban.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 117).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(deca_tr_t17, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 17, 0.33).
narrative_ontology:measurement(deca_tr_t35, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(deca_tr_t61, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 61, 0.48).
narrative_ontology:measurement(deca_tr_t88, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 88, 0.36).
narrative_ontology:measurement(deca_tr_t105, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 105, 0.39).
narrative_ontology:measurement(deca_tr_t117, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 117, 0.44).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(deca_be_t17, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 17, 0.58).
narrative_ontology:measurement(deca_be_t35, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(deca_be_t61, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 61, 0.14).
narrative_ontology:measurement(deca_be_t88, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 88, 0.52).
narrative_ontology:measurement(deca_be_t105, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 105, 0.66).
narrative_ontology:measurement(deca_be_t117, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 117, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(deca_su_t17, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 17, 0.71).
narrative_ontology:measurement(deca_su_t35, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 35, 0.84).
narrative_ontology:measurement(deca_su_t61, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 61, 0.24).
narrative_ontology:measurement(deca_su_t88, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 88, 0.63).
narrative_ontology:measurement(deca_su_t105, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 105, 0.81).
narrative_ontology:measurement(deca_su_t117, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 117, 0.16).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% 'The Decalogue image prohibition' is one colloquial label over three structurally distinct constraints (epsilon-invariance decomposition): this iconoclast reading (total ban; victims include painters, monastics, and the venerating laity; capturing seat: the centralizing authority), the iconodule reading (worship-at-images banned, honor through images licensed; small victim set, low extraction), and the moderate iconoclast reading (statuary banned, flat images regulated; intermediate victim set). The bare text sits upstream of all three; Incarnation theology exerts downstream pressure shaping the iconodule variant. Edges here link the siblings so foreclosure and contamination analysis traverses the family; sibling files carry reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
