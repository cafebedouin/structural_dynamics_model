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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Subordinationist Reading of the Father-Son Relation
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   This constraint is the Arian/subordinationist reading of the
 *   fourth-century Christological controversy: Christ as a created being,
 *   subordinate to and ontologically distinct from the Father, denying
 *   identical divine substance (homoousios). This reading is authored as its
 *   own constraint per the ε-invariance principle — it is not the same
 *   constraint as the pro-Nicene homoousios reading or the semi-Arian
 *   homoiousios compromise, which have their own ε values, beneficiary
 *   structures, and stakeholder sets in sibling files. The subordinationist
 *   reading periodically captured imperial backing (notably under Constantius
 *   II, at councils such as Antioch, Sirmium, and Constantinople 360) and
 *   used that backing to depose and exile Nicene bishops, most visibly
 *   Athanasius of Alexandria on repeated occasions. The reading's
 *   coordination function — a coherent monotheistic answer to a genuinely
 *   open exegetical question — is real, but its persistence in office during
 *   Arian-favorable reigns depended on active imperial enforcement against
 *   Nicene rivals, which is why this is authored as tangled_rope rather than
 *   pure rope.
 *
 * KEY AGENTS:
 *   - non_nicene_bishops: primary agenda-setter and beneficiary (institutional/constrained) — administers sees under the subordinationist reading
 *   - constantius_ii_and_arian_sympathetic_emperors: enforcing power (institutional/arbitrage) — provides the imperial backing that makes the reading's ecclesiastical capture possible
 *   - nicene_clergy_under_arian_emperors: primary target (moderate/trapped) — deposed and exiled when the reading holds imperial favor
 *   - alexandrian_see: institutional payer (powerful/constrained) — repeatedly loses its see to Arian-aligned claimants despite its own institutional strength
 *   - trinitarian_laity_in_contested_dioceses: diffuse payer (powerless/trapped) — bears the sacramental confusion and occasional violence of contested succession
 *   - later_church_historians: analytical observer — reconstructs the political-theological interplay retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.61).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.72).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Subordinationist Reading of the Father-Son Relation").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, '0503a447-72b5-45c4-a7a0-6db5ae2a1bb8').
narrative_ontology:cs_kernel_codification('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', distributed).
narrative_ontology:cs_authority_grounding('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', extraction).
narrative_ontology:cs_interpretation_layer_present('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8').
narrative_ontology:cs_reading_relation('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', foundational, son_is_created_being_distinct_from_father).
narrative_ontology:cs_axiom_status(son_is_created_being_distinct_from_father, holdable).
narrative_ontology:cs_axiom_grounding('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', son_is_created_being_distinct_from_father, theological).
narrative_ontology:cs_axiom('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', secondary, strict_divine_monarchy_requires_ontological_subordination).
narrative_ontology:cs_axiom_status(strict_divine_monarchy_requires_ontological_subordination, holdable).
narrative_ontology:cs_axiom_grounding('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', strict_divine_monarchy_requires_ontological_subordination, theological).
narrative_ontology:cs_reference_frame('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', pre_nicene_subordinationist_exegesis).
narrative_ontology:cs_drift_state('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', post_constantinople_381, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0503a447-72b5-45c4-a7a0-6db5ae2a1bb8', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, non_nicene_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, eastern_provincial_churches).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_court_faction).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_clergy_under_arian_emperors).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, alexandrian_see).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, trinitarian_laity_in_contested_dioceses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, eastern_provincial_churches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops across the eastern provinces (Nicomedia, Antioch, and allied sees) who hold that the Son is a distinct, created being, brought into existence by the Father's will and therefore ontologically subordinate. They administer their own sees, ordain like-minded clergy, and press their reading through provincial synods, court connections, and periodic exile of Nicene rivals when imperial favor turns their way. Their exit from the broader church controversy is constrained by the fact that communion, ordination validity, and property all hinge on which reading holds imperial recognition at any given moment.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, non_nicene_bishops, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, non_nicene_bishops, beneficiary).

% Court theologians and imperial advisors (under emperors such as Constantius II) who gain ecclesiastical appointments, influence over episcopal succession, and doctrinal legitimacy when the subordinationist reading has imperial backing. They can shift allegiance between competing formulas as political winds change, and their standing rises and falls with which bishops the emperor currently favors.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_court_faction, beneficiary,
    powerful, biographical, mobile, continental).

% Local congregations in regions where subordinationist bishops hold sees benefit from theological continuity with pre-Nicene traditions they regard as more scripturally faithful and less philosophically foreign (less reliant on Greek metaphysical vocabulary). They also bear the cost of repeated schism, competing consecrations, and periodic violent factional conflict when rival bishops contest the same see.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, eastern_provincial_churches, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, eastern_provincial_churches, payer).

% Bishops and clergy holding the homoousios position who are deposed, exiled, or barred from their sees when an Arian-sympathetic emperor enforces the subordinationist reading as imperial orthodoxy (as happened to Athanasius multiple times). Their exit options are effectively closed: leaving the priesthood forfeits vocation and community standing, while remaining risks banishment or worse.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_clergy_under_arian_emperors, payer,
    moderate, biographical, trapped, regional).

% The Alexandrian patriarchate, historically the strongest institutional advocate of homoousios, repeatedly loses its bishop to exile, has rival Arian-aligned candidates installed by imperial fiat, and expends significant resources contesting canonical legitimacy in each cycle of imperial favor. Its institutional power is real but is repeatedly overridden by direct imperial intervention on the subordinationist side.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, alexandrian_see, payer,
    powerful, generational, constrained, regional).

% Ordinary believers in dioceses where the see changes hands between Nicene and subordinationist bishops experience shifting sacramental validity, competing claims about which bishop can licitly baptize or ordain, and occasional street violence between rival congregations. They have no meaningful voice in the doctrinal contest and cannot easily relocate to a diocese with stable leadership.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, trinitarian_laity_in_contested_dioceses, payer,
    powerless, biographical, trapped, local).

% Western bishops, largely committed to the Nicene formula from early on, are structurally excluded from the eastern political and theological negotiations that produce and sustain the subordinationist reading's periods of imperial favor. They protest through letters and councils but hold little direct leverage over eastern imperial appointments.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, roman_and_western_bishops, excluded,
    institutional, generational, analytical, continental).

% Emperors who favor the subordinationist reading convene councils (e.g., Antioch, Sirmium, Constantinople 360) to formalize creeds affirming the Son's subordination or unlikeness to the Father, and use imperial power to depose Nicene bishops and install compliant ones. Their theological preference is inseparable from a political preference for a formula that centralizes doctrinal authority in the emperor rather than in an increasingly independent Alexandrian-Roman axis.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, constantius_ii_and_arian_sympathetic_emperors, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Assess the fourth-century controversy retrospectively, reconstructing which formulas held favor when, and to what degree doctrinal claims tracked genuine exegetical conviction versus court faction and imperial power politics.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, non_nicene_bishops).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The subordinationist reading offers a coherent, scripturally-grounded account of monotheism that many bishops found easier to reconcile with the Old Testament's strict unity of God and with a plain reading of texts describing the Son as begotten, sent, and obedient to the Father. It coordinates a real theological community around a shared exegetical and liturgical tradition predating the Nicene formula.
% TRANSFER_FUNCTION: When backed by imperial power, the arrangement moves ecclesiastical office, see revenues, and doctrinal legitimacy from Nicene incumbents to subordinationist claimants, and moves the burden of exile, deposition, and social disruption onto Nicene clergy and their congregations.
% ABSENT_VOICES: Western bishops and the broader Latin-speaking church had little seat at the eastern councils that produced Arian-leaning creeds; ordinary laity in contested sees had no voice in which bishop's ordinations they were told to consider valid.
% DISAPPEARANCE_RATIONALE: If imperial enforcement of the subordinationist reading vanished, deposed Nicene bishops would return to their sees, the eastern episcopate would reconsolidate around homoousios far more quickly, and the pattern of council-by-council doctrinal reversal (Nicaea, Antioch, Sirmium, Constantinople) that defined the fourth century would not have occurred in the same form — the outcome ultimately settled at Constantinople 381 in the Nicene direction, but only after decades in which imperial backing repeatedly rearranged who held office and on what doctrinal terms.
% FOUNDING_PROBLEM: The pre-Nicene church lacked a single settled formula for the Father-Son relation; various subordinationist, monarchian, and adoptionist christologies coexisted informally. The Arian reading crystallized one available answer to a genuinely open exegetical question — how can the Son be truly divine, truly derived from the Father, and the unity of God be preserved — into an organized ecclesiastical faction.
% FOUNDING_PROBLEM_CORROBORATION: Non-Nicene bishops themselves attest the problem is live: they argue homoousios imports non-scriptural Greek metaphysical categories and that the strict monotheism the subordinationist reading protects remains theologically necessary. Nicene opponents and, eventually, the imperially-convened council of Constantinople in 381 attest the problem was resolved in the homoousios direction and that the subordinationist reading's persistence after that point was sustained mainly by regional political and ethnic-Gothic institutional loyalty rather than unresolved exegetical necessity. No source entirely outside both factions survives from this period to adjudicate independently.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.61) reflects that during periods of imperial favor, the arrangement transfers real ecclesiastical office, revenue, and legitimacy away from Nicene incumbents onto subordinationist claimants — this is not merely a doctrinal disagreement but a structure with winners who capture sees and losers who are exiled. Suppression (0.72) is high because holding the subordinationist reading in a contested see required active imperial enforcement — troops, exile orders, and the threat of violence — not persuasion alone; it peaks around 350-360 during Constantius II's most aggressive phase and eases somewhat by 370-381 as Nicene consolidation began. Accessibility collapse (0.45) is only moderate: unlike a mountain, alternative theological formulas (homoousios, homoiousios, and other variants) remained live and contested throughout the interval — this was never settled by logical necessity, only by shifting political and conciliar outcomes. Resistance (0.78) is high because Nicene clergy, most famously Athanasius, actively and repeatedly resisted exile and deposition, refusing to concede doctrinal ground even under direct imperial pressure — this is a constraint that met sustained, organized resistance, not passive acceptance. Theater ratio (0.28) captures that a portion of conciliar activity (repeated councils producing successive creed revisions between 341 and 360) functioned as performative doctrinal maneuvering to accommodate shifting court factions rather than settling substantive theological questions.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Nicene bishops and the Arian court faction sit near the beneficiary end: they gain sees, imperial favor, and doctrinal legitimacy when this reading holds power, and their exit options (mobility between competing formulas, court access) are relatively favorable. Nicene clergy under Arian emperors and the Alexandrian see sit near the target end: they bear deposition, exile, and property loss, and their exit options are genuinely constrained or trapped — abandoning the priesthood or the see forfeits vocation and community standing built over a lifetime. Trinitarian laity in contested dioceses are the most powerless payers: they have essentially no leverage over which bishop's ordinations are deemed valid and cannot readily relocate. The emperors themselves occupy an unusual agenda-setter position with arbitrage-grade exit options — they could and did shift theological favor between formulas as political utility dictated, which is itself evidence that the doctrinal question was substantially entangled with a political one about where doctrinal authority (emperor vs. increasingly independent sees) should sit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to reconcile scriptural monotheism with the divine status of the Son — was a genuinely live exegetical question in the third and early fourth centuries, which is why the coordination_function is authored as real rather than as pure cover. But by the time of Constantinople 381, the broader church's resolution in the homoousios direction suggests the subordinationist reading's continued political enforcement in the interim (340s-370s) increasingly rode on imperial and regional-factional momentum rather than unresolved theological necessity — this is why founding_problem_status is authored as contested rather than flatly live or dead: the exegetical question the Arian reading answers remains a genuine theological position within its own tradition, but the specific ecclesiastical arrangement of capturing sees by imperial force had, by the reading's own later opponents' account, outlived whatever settling function it might have served.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_conviction_vs_political_faction,
    'To what extent did bishops holding the subordinationist reading do so from genuine exegetical conviction versus alignment with whichever faction currently held imperial favor?',
    'Comparative analysis of individual bishops'' doctrinal consistency across reigns — did the same bishops hold the same position under both Nicene-favoring and Arian-favoring emperors, or did positions shift with imperial favor? Surviving correspondence and conciliar records provide partial evidence.',
    'If conviction dominates, this reading is better modeled as a genuine theological coordination function with incidental political entanglement; if faction dominates, the extraction component is understated and the constraint is closer to pure political capture wearing theological cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_conviction_vs_political_faction, empirical, 'Whether subordinationist allegiance tracked genuine belief or political faction.').

omega_variable(
    authority_grounding_ambiguity,
    'Is the legitimacy of the subordinationist reading properly grounded in pre-Nicene ecclesiastical tradition and independent scriptural exegesis (a genuine competing lineage), or was it substantially constructed and sustained by imperial intervention in a way that makes ''tradition'' post-hoc legitimation for what was actually court-driven doctrinal imposition?',
    'Trace the reading''s presence and institutional standing in the decades before Constantine''s involvement in church affairs (pre-325) versus its dependence on imperial backing after 337; a reading present and institutionally stable before imperial entanglement supports genuine independent lineage.',
    'If genuinely independent lineage, the constraint is closer to a legitimate minority theological tradition subjected to eventual suppression (shifting the type toward snare against the reading rather than tangled_rope); if substantially imperially constructed, the tangled_rope classification with real but politically-amplified coordination function is the more accurate read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, conceptual, 'Whether the reading''s authority is grounded in independent tradition or imperial construction.').

omega_variable(
    cs_framing_kernel_vs_authority_narrative,
    'Should the commitment-system framing take the kernel to be the theological formula itself (the christological content, fixed_text-adjacent) or the narrative of conciliar procedural legitimacy (which council counts as authoritative) that determines which formula wins? These two framings produce different authority_grounding assessments: the formula-as-kernel framing suggests ''lineage'' (continuity with pre-Nicene subordinationist exegesis), while the procedure-as-kernel framing suggests ''extraction'' (imperial power to convene and ratify councils determines outcome, and this power is the actual site of contest).',
    'This is not empirically resolvable from surviving evidence alone; it depends on which layer of the dispute the analyst treats as primary. The choice made here is authority_grounding: extraction, foregrounding that in this era ecclesiastical councils'' authority was substantially a function of which emperor convened and enforced them.',
    'Under the lineage framing, the subordinationist reading would appear as a stable minority tradition experiencing suppression once conciliar power shifted against it (pushing the classification toward snare-against). Under the extraction framing adopted here, the reading is itself an active participant in a contest over who controls conciliar legitimacy, which supports the tangled_rope classification with real coordination and real active-enforcement extraction on both the giving and receiving end across the century.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_authority_narrative, conceptual, 'Alternative framings of the CS kernel (theological content vs. conciliar procedure) yield different authority_grounding assessments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 320, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t320, homoousios_christology__arian_reading, theater_ratio, 320, 0.1).
narrative_ontology:measurement(homo_tr_t330, homoousios_christology__arian_reading, theater_ratio, 330, 0.15).
narrative_ontology:measurement(homo_tr_t340, homoousios_christology__arian_reading, theater_ratio, 340, 0.2).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__arian_reading, theater_ratio, 350, 0.25).
narrative_ontology:measurement(homo_tr_t360, homoousios_christology__arian_reading, theater_ratio, 360, 0.3).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__arian_reading, theater_ratio, 370, 0.29).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__arian_reading, theater_ratio, 381, 0.28).

% Extraction over time
narrative_ontology:measurement(homo_be_t320, homoousios_christology__arian_reading, base_extractiveness, 320, 0.35).
narrative_ontology:measurement(homo_be_t330, homoousios_christology__arian_reading, base_extractiveness, 330, 0.42).
narrative_ontology:measurement(homo_be_t340, homoousios_christology__arian_reading, base_extractiveness, 340, 0.5).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__arian_reading, base_extractiveness, 350, 0.58).
narrative_ontology:measurement(homo_be_t360, homoousios_christology__arian_reading, base_extractiveness, 360, 0.63).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__arian_reading, base_extractiveness, 370, 0.6).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t320, homoousios_christology__arian_reading, suppression_requirement, 320, 0.4).
narrative_ontology:measurement(homo_su_t330, homoousios_christology__arian_reading, suppression_requirement, 330, 0.5).
narrative_ontology:measurement(homo_su_t340, homoousios_christology__arian_reading, suppression_requirement, 340, 0.6).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__arian_reading, suppression_requirement, 350, 0.72).
narrative_ontology:measurement(homo_su_t360, homoousios_christology__arian_reading, suppression_requirement, 360, 0.78).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__arian_reading, suppression_requirement, 370, 0.75).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__arian_reading, suppression_requirement, 381, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the homoousios_christology kernel family. pro_nicene_reading and semi_arian_reading are sibling constraints with independently authored ε, beneficiary/victim structures, and stakeholder sets — they are not alternative measurements of this constraint but structurally distinct constraints sharing a contested kernel (the Father-Son ontological relation). The arian_reading (this file) is authored with a tangled_rope claim reflecting genuine pre-Nicene exegetical coordination combined with imperially-enforced extraction during periods of court favor; the pro_nicene_reading sibling would be expected to author its own ε reflecting the eventual conciliar victory and its own enforcement dynamics against subordinationist holdouts after 381; the semi_arian_reading sibling occupies the contested middle position and would be expected to show its own distinct beneficiary/victim structure as a compromise faction squeezed between the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
