% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Subordinationist Ecclesiastical Settlement (Homoousios Kernel, Arian Reading)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   Between Arius's quarrel with Bishop Alexander of Alexandria (c. 318) and
 *   the Council of Constantinople (381), the claim that the Son is created
 *   and subordinate to the Father — not of identical substance with him —
 *   organized a full ecclesiastical arrangement: councils that drafted
 *   formulas, episcopal courts that deposed opponents, an imperial
 *   enforcement arm under Constantius II, and a mission (Ulfilas among the
 *   Goths) that carried the doctrine beyond the empire's borders. This file
 *   instantiates the arian_reading of the homoousios_christology kernel only.
 *   The referent of epsilon is the subordinationist arrangement itself, in
 *   its own seats and with its own enforcement — not the Nicene arrangement
 *   it fought. Epsilon is authored by the reading's own lights over that
 *   fixed referent: the Arian party counted its enforcement of the line as
 *   legitimate discipline rather than extraction, and the moderate epsilon
 *   records the magnitude of that discipline as the reading's lights register
 *   it, not as its opponents did. The claimed type (tangled_rope) and the
 *   metrics are authored independently: the claim states what I believe
 *   structurally true; the metrics describe the arrangement's actual
 *   operation, including the enforcement peak (357-359) and the retreat (381)
 *   that the reading's own lights experienced as persecution.
 *
 * KEY AGENTS:
 *   - homoian_episcopal_hierarchy: agenda-setter and primary beneficiary (institutional/constrained) — administers the doctrinal line and collects the sees and presidencies
 *   - constantius_imperial_establishment: enforcement arm (institutional/arbitrage) — supplies the coercive machinery and can redirect it with dynastic convenience
 *   - court_bishop_network: secondary beneficiary (powerful/mobile) — collects court access, see transfers, and rehabilitations
 *   - pro_nicene_episcopate: primary target (organized/identity_locked) — bears deposition and exile; office and confession are one commitment
 *   - pro_nicene_laity: secondary target (moderate/constrained) — bears imposed clergy, disrupted communion, second-class standing in Gothic lands
 *   - western_episcopate: secondary target (institutional/identity_locked) — pressured at Ariminum, leaders exiled, subscriptions collected under duress
 *   - gothic_royal_converts: peripheral beneficiary (organized/identity_locked) — receives scripture, literacy, and a distinct peoplehood identity
 *   - egyptian_monastic_communities: excluded voice (organized/constrained) — mobilizes decisively for the Nicene side with no seat in any synod
 *   - pagan_senatorial_observers: analytical observer (powerful/analytical) — sees the full structure from outside, holding no stake in either party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.4).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.38).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Subordinationist Ecclesiastical Settlement (Homoousios Kernel, Arian Reading)").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, 'b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf').
narrative_ontology:cs_kernel_codification('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', formalized).
narrative_ontology:cs_authority_grounding('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', extraction).
narrative_ontology:cs_interpretation_layer_present('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf').
narrative_ontology:cs_reading_relation('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', foundational, son_created_from_non_being).
narrative_ontology:cs_axiom_status(son_created_from_non_being, holdable).
narrative_ontology:cs_axiom_grounding('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', son_created_from_non_being, theological).
narrative_ontology:cs_axiom('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', foundational, father_ontological_supremacy).
narrative_ontology:cs_axiom_status(father_ontological_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', father_ontological_supremacy, theological).
narrative_ontology:cs_reference_frame('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', apostolic_subordinationist_tradition).
narrative_ontology:cs_drift_state('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', constantinopolitan_settlement, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b2ef63cc-29a8-4fc8-a2e2-eb7826f894cf', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, homoian_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, court_bishop_network).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, gothic_royal_converts).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_episcopate).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_laity).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, western_episcopate).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, subordinationist_christology).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, divine_incommunicability_principle).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, imperial_religious_uniformity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Eastern bishops — and briefly the empire-wide hierarchy — who drafted, signed, and policed the subordinationist formulas: the Dedication Council line, the Sirmium decrees, the Ariminum-Seleucia settlement. They convoked synods, deposed clergy who taught the homoousion, and administered communion boundaries. Their sees, council presidencies, and standing with the court flowed from holding the line; after 381 many surrendered their positions rather than their teaching, and others re-entered the Nicene church on its terms.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, homoian_episcopal_hierarchy, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, homoian_episcopal_hierarchy, beneficiary).

% The court and administrative apparatus of Constantius II, which made doctrinal uniformity an instrument of rule: it convened and stage-managed councils, exiled resisting bishops (Hosius under threat, Liberius, Hilary), and enforced subscriptions. Its commitment was instrumental and reversible — the dynasty changed doctrinal alliances with political convenience, and the machinery it built for the homoian line was later turned against the line's holders.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, constantius_imperial_establishment, agenda_setter,
    institutional, biographical, arbitrage, continental).

% The Eusebian circle around the imperial court: bishops holding major sees (Nicomedia, later Constantinople) who mediated between palace and episcopate. They collected rehabilitation for Arius, see transfers, and influence over appointments; their positions were portable because they rested on court favor rather than local congregational standing.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, court_bishop_network, beneficiary,
    powerful, biographical, mobile, continental).

% The Gothic communities converted through Ulfilas's mission from the 340s onward. They received scripture in their own tongue, a literate clergy, and a Christian identity deliberately distinct from the empire's Nicene Christianity — royal Arianism became a marker of Gothic peoplehood against Roman religious authority. Their descendants held the Arian confession for centuries after the empire abandoned it, at growing cost as the Nicene world closed ranks around them.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, gothic_royal_converts, beneficiary,
    organized, generational, identity_locked, regional).

% The bishops who taught the homoousion through the enforcement decades: Athanasius (five exiles), Eustathius of Antioch, Paul of Constantinople (died in exile), Marcellus of Ancyra, Liberius of Rome, Hilary of Poitiers. They lost sees, faced repeated exile, and worked through underground networks and Western alliances. Their office and their confession were the same commitment; subscribing to the formulas would have dissolved the office's meaning, so they endured rather than exited.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_episcopate, payer,
    organized, generational, identity_locked, continental).

% Congregations in Alexandria, Constantinople, Asia Minor, and later the Gothic lands whose devotional life — baptism, hymnody, loyalty to exiled bishops — was Nicene. Under homoian administration they received imposed clergy, disrupted communion, and in Gothic territory second-class religious standing. Their resistance was constant and unorganized at the imperial level, and ultimately decisive in depth: the homoian establishment never won their attachment.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_laity, payer,
    moderate, biographical, constrained, regional).

% The Latin bishops who resisted the Eastern settlement: Rome under Julius and Liberius, the Ariminum assembly that initially refused the homoian formula and was worn down by detention and attrition. They bore the exile of their leaders, forced subscriptions under duress (later retracted), and a decade of uncertainty about whether the East's formulas would be imposed on them.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, western_episcopate, payer,
    institutional, generational, identity_locked, continental).

% The ascetic movement of the Egyptian desert — Antony's circle, the Pachomian houses — which threw its weight behind Athanasius and the Nicene confession. Its monks descended on Alexandria at crisis moments and gave the Nicene cause a popular base no council could convene or control; it had no formal seat in any synod that decided the controversy.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, egyptian_monastic_communities, excluded,
    organized, biographical, constrained, regional).

% The educated pagan class — Ammianus Marcellinus its chronicler — watching the Christian dispute from outside. It saw the whole structure: emperors exiling bishops over a word, councils reversing councils, uniformity pursued by force. Its testimony is valuable precisely because it held no stake in either christological party and dismissed the whole quarrel's substance.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pagan_senatorial_observers, observer,
    powerful, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, homoian_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gave the non-Nicene churches a shared doctrinal standard — one unbegotten Father, the Son as first-created mediator — governing ordination, communion, and council membership, and (as its holders held) preserving monotheism against the collapse of Father and Son into one person. Under Constantius it additionally coordinated the Eastern episcopate with imperial governance, and through the Ulfilas mission it coordinated a new Christian civilization on the Danube frontier.
% TRANSFER_FUNCTION: Moved episcopal sees, court access, and council presidencies toward bishops who subscribed to the subordinationist formulas; moved deposition, exile, and doctrinal stigma onto clergy who taught the homoousion; moved subscription and conformity from the whole ordained body to the arrangement's enforcers. In the Gothic mission it moved literacy, liturgy, and a distinct Christian identity to Gothic communities under royal Arian auspices.
% ABSENT_VOICES: The laity had no seat at any council, though their attachment to Nicene baptism and hymnody ultimately decided how deep the contest ran; the Egyptian monks, who mobilized decisively for Athanasius, were never convened; the Gothic communities receiving the mission were its objects, not its parties. Within the enforced councils themselves, dissenting bishops were deposed before being heard — Athanasius condemned in absentia at Tyre, Liberius exiled rather than examined.
% DISAPPEARANCE_RATIONALE: At its 359 peak the arrangement held the empire's episcopal map: sees, councils, and imperial church policy were organized around the homoian line. Overnight removal would have collapsed the enforced uniformity, restored the exiled Nicene hierarchy a generation early, and cut the Gothic mission's doctrinal identity from its royal patrons — the Gothic church's separate development, which shaped frontier politics for three centuries, depended on it.
% FOUNDING_PROBLEM: Whether the Son who saves is God in the full sense or the first and greatest creature: the question on which, as the parties saw it, monotheism, the logic of redemption (a creature cannot deify creatures), and the plain sense of the subordinationist texts all hung — and on which episcopal careers and imperial unity came to hang as well.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the pro-Nicene polemicists (Athanasius, Hilary) attest the problem's liveness — they fought it for two generations at the cost of repeated exile; the pagan observer Ammianus Marcellinus attests that the dispute consumed the age, while dismissing its substance; Constantine's own letters attest it as a threat to imperial order. No seat outside the arrangement attests the Arian answer as its resolution — corroboration covers the problem's reality, not this reading's settlement of it.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).
:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.40 at endpoint) is moderate: by the reading's own lights the arrangement coordinated a real theological program — one unbegotten God, the Son as mediator of creation — and its costs fell chiefly on opponents it classified as teachers of error. Suppression (0.38 at endpoint) tracks enforcement capacity, which is the dynamic this story specifically traces, hence the suppression_requirement series: near-zero while the arrangement was a condemned dissident network (318-325), ratcheting up through the court alignment (335), the Dedication Council machinery (341), and Sirmium (351, 357), peaking at the empire-wide imposition of Ariminum-Seleucia (359, 0.62), then decaying as imperial support withdrew (366) and collapsing at the Constantinopolitan settlement (381), surviving only as Gothic-royal enforcement. Theater (0.30 at endpoint) follows the formula churn: the early councils did genuine doctrinal work, but by the Sirmium 'Blasphemy' — which banned substance-language by decree — and the double council of 359, where the formula was engineered for imperial uniformity rather than resolution and subscriptions were collected under detention, a large share of activity was performative unity-manufacturing. Accessibility_collapse (0.48): the Nicene alternative was driven underground in the East but never eliminated, never collapsed at all in the West, and returned with imperial force; the controversy's persistence is itself the evidence that alternatives survived. Resistance (0.75): five exiles of Athanasius, the Ariminum refusal, monastic mobilization, and the eventual imperial reversal — the arrangement met sustained, organized resistance for its entire life. All three series share one time grid (318, 325, 335, 341, 351, 357, 359, 366, 381). The arc is rise-peak-decay, not cyclical: the driver is imperial alignment, acquired under Constantius and lost at Theodosius's settlement. Suppression here is structural throughout — imperial and episcopal machinery, not internalized compliance — so no structural/internalized ambiguity omega is required.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from the same structure. From the homoian hierarchy's seat the arrangement is legitimate doctrinal order it built and administered: councils, formulas, and depositions are the church defending the faith, and the imperial alliance is providential support. From the pro-Nicene episcopate's seat the same structure is enforced extraction: sees transferred to subscribers, exile for teaching the homoousion, subscriptions collected under detention. Same-level actors diverge as well: the court_bishop_network and the territorial hierarchy held the same nominal rank (institutional power) but different exits — the court network's position was portable across sees on palace favor, the hierarchy's was bound to territories it had purged and congregations it had alienated. The engine computes these divergences from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the homoian hierarchy (collects sees and presidencies while administering the line — near the beneficiary end), the court bishop network (collects palace access and see transfers; mobile exit places it nearest the beneficiary end), and the Gothic royal converts (collect scripture, literacy, and a distinct identity; peripheral to the enforcement core but genuinely subsidized by it). Victims: the pro-Nicene episcopate and the Western episcopate (identity_locked — their office and confession are one commitment, so exit means dissolving the office; identity-locked targets sit near the full-target end) and the Nicene laity (constrained; bears imposed clergy and disrupted communion). The imperial establishment is an agenda-setter with an instrumental stake: it collected governance goods — uniformity, leverage over the episcopate — rather than doctrinal rents, placing it between the beneficiary and symmetric positions. The Egyptian monks are an excluded voice: they shape outcomes without a seat and feed no directionality derivation. The Gothic seat is the structurally interesting case — a beneficiary whose exit is identity_locked, which is why its descendants held the arrangement for centuries after every material incentive to leave had inverted.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabels. As pure snare, the arrangement would be misread: the coordination function was genuine — a coherent scriptural and monotheistic program, real boundary maintenance for ordination and communion, and a mission that carried literacy and identity to the Goths — not a cover story, however much court opportunism rode on it. As pure rope, it would be misread the other way: the extraction was asymmetric and enforced — depositions, exiles, forced subscriptions — and the gains concentrated in identifiable seats, above all the hierarchy that both ran the line and collected from it. Tangled_rope holds both facts. The R5 mismatch check returns no zombie flag: the founding problem is contested, not dead — the Nicene party declared it solved at 381, the Arian tradition held it live for centuries in the Gothic kingdoms, and subordinationist readings recur whenever the question is reopened. An arrangement whose founding problem is still argued is not a piton maintained by inertia; theater peaked (0.46) during the formula churn, but the arrangement was never inertial — its function was live throughout its life.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the arian_reading of kernel homoousios_christology; how would the beneficiary/victim structure and epsilon re-index under the pro_nicene_reading or the semi_arian_reading? The disagreement is located in the Son''s ontological origin — created from non-being versus eternal generation versus similar-substance generation.',
    'Cross-reading comparison of the three sibling constraint files: enforcement direction, beneficiary sets, and epsilon should flip or shift systematically (after 381 the Nicene arrangement extracts from the Arian holders instead of the reverse).',
    'Classification of this file is valid only for the arian_reading; merging readings would average over structurally distinct arrangements and violate epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is one reading of a three-reading kernel; sibling readings restructure beneficiary/victim sets and enforcement direction.').

omega_variable(
    sincere_conviction_vs_court_opportunism,
    'Was the homoian establishment''s subordinationism sincere doctrinal conviction or court opportunism riding an imperial uniformity project?',
    'Prosopographic analysis of the hierarchy''s consistency across regime changes (who held the line under Constantine''s disfavor, who flipped under Julian, who re-held under Valens), comparing pastoral writings against council politics.',
    'If largely opportunism, the coordination function thins toward cover and the arrangement drifts toward snare; if sincere, the tangled_rope reading holds — genuine coordination carrying asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_conviction_vs_court_opportunism, empirical, 'Whether the arrangement''s coordination function is genuine or a cover story.').

omega_variable(
    formula_theater_vs_doctrine,
    'Was the 357-359 formula churn (the Sirmium Blasphemy, the Ariminum-Seleucia settlement) performative uniformity-manufacturing or genuine doctrinal development?',
    'Textual comparison of the successive formulas'' theological content and the councils'' deliberation records; track whether the substance-language bans were argued or merely decreed.',
    'If performative, theater_ratio is understated at the peak and the proxy-goal-drift reading strengthens; if substantive, the arrangement''s doctrinal work was real through the enforcement peak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formula_theater_vs_doctrine, empirical, 'Whether the peak-era council activity was functional or theatrical.').

omega_variable(
    gothic_identity_fusion_durability,
    'Did Gothic Arianism persist for centuries after imperial defeat because of identity fusion (Arianism as a marker of Gothic peoplehood) or institutional inheritance (royal church structures outliving their founding logic)?',
    'Track conversion timing across the Gothic successor kingdoms against political incentives: if conversions cluster at moments of political advantage, inheritance dominates; if they resist advantage, identity fusion dominates.',
    'Identity fusion supports the identity_locked exit attribution for the Gothic seat; pure inheritance would reclassify that seat''s exit as constrained rather than identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gothic_identity_fusion_durability, empirical, 'Mechanism behind the Gothic seat''s centuries-long retention of the arrangement.').

omega_variable(
    enforcement_asymmetry_direction,
    'Across the full interval coercion ran in both directions — Arians exiled 325-337, Nicenes exiled 339-361, Arians again after 381. Does the measured extraction belong to this arrangement or to the alternating imperial settlement machinery it rode?',
    'Separate the doctrinal arrangement''s own enforcement capacity (councils, episcopal courts) from the imperial machinery''s alternating use of it, and re-measure epsilon for the arrangement alone.',
    'If the imperial machinery dominates, this reading''s epsilon is partly borrowed from a meta-arrangement (imperial religious policy), and the tangled_rope classification over-attributes agency to the doctrinal coalition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_direction, conceptual, 'Whether the arrangement''s enforcement is its own or borrowed from the alternating imperial machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 318, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t318, homoousios_christology__arian_reading, theater_ratio, 318, 0.08).
narrative_ontology:measurement_basis(homo_tr_t318, observed).
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__arian_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t335, homoousios_christology__arian_reading, theater_ratio, 335, 0.18).
narrative_ontology:measurement_basis(homo_tr_t335, observed).
narrative_ontology:measurement(homo_tr_t341, homoousios_christology__arian_reading, theater_ratio, 341, 0.24).
narrative_ontology:measurement_basis(homo_tr_t341, observed).
narrative_ontology:measurement(homo_tr_t351, homoousios_christology__arian_reading, theater_ratio, 351, 0.3).
narrative_ontology:measurement_basis(homo_tr_t351, observed).
narrative_ontology:measurement(homo_tr_t357, homoousios_christology__arian_reading, theater_ratio, 357, 0.4).
narrative_ontology:measurement_basis(homo_tr_t357, observed).
narrative_ontology:measurement(homo_tr_t359, homoousios_christology__arian_reading, theater_ratio, 359, 0.46).
narrative_ontology:measurement_basis(homo_tr_t359, observed).
narrative_ontology:measurement(homo_tr_t366, homoousios_christology__arian_reading, theater_ratio, 366, 0.4).
narrative_ontology:measurement_basis(homo_tr_t366, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__arian_reading, theater_ratio, 381, 0.3).
narrative_ontology:measurement_basis(homo_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t318, homoousios_christology__arian_reading, base_extractiveness, 318, 0.18).
narrative_ontology:measurement_basis(homo_be_t318, observed).
narrative_ontology:measurement(homo_be_t325, homoousios_christology__arian_reading, base_extractiveness, 325, 0.15).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t335, homoousios_christology__arian_reading, base_extractiveness, 335, 0.28).
narrative_ontology:measurement_basis(homo_be_t335, observed).
narrative_ontology:measurement(homo_be_t341, homoousios_christology__arian_reading, base_extractiveness, 341, 0.36).
narrative_ontology:measurement_basis(homo_be_t341, observed).
narrative_ontology:measurement(homo_be_t351, homoousios_christology__arian_reading, base_extractiveness, 351, 0.44).
narrative_ontology:measurement_basis(homo_be_t351, observed).
narrative_ontology:measurement(homo_be_t357, homoousios_christology__arian_reading, base_extractiveness, 357, 0.52).
narrative_ontology:measurement_basis(homo_be_t357, observed).
narrative_ontology:measurement(homo_be_t359, homoousios_christology__arian_reading, base_extractiveness, 359, 0.58).
narrative_ontology:measurement_basis(homo_be_t359, observed).
narrative_ontology:measurement(homo_be_t366, homoousios_christology__arian_reading, base_extractiveness, 366, 0.5).
narrative_ontology:measurement_basis(homo_be_t366, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.4).
narrative_ontology:measurement_basis(homo_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t318, homoousios_christology__arian_reading, suppression_requirement, 318, 0.1).
narrative_ontology:measurement_basis(homo_su_t318, observed).
narrative_ontology:measurement(homo_su_t325, homoousios_christology__arian_reading, suppression_requirement, 325, 0.12).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t335, homoousios_christology__arian_reading, suppression_requirement, 335, 0.22).
narrative_ontology:measurement_basis(homo_su_t335, observed).
narrative_ontology:measurement(homo_su_t341, homoousios_christology__arian_reading, suppression_requirement, 341, 0.35).
narrative_ontology:measurement_basis(homo_su_t341, observed).
narrative_ontology:measurement(homo_su_t351, homoousios_christology__arian_reading, suppression_requirement, 351, 0.48).
narrative_ontology:measurement_basis(homo_su_t351, observed).
narrative_ontology:measurement(homo_su_t357, homoousios_christology__arian_reading, suppression_requirement, 357, 0.56).
narrative_ontology:measurement_basis(homo_su_t357, observed).
narrative_ontology:measurement(homo_su_t359, homoousios_christology__arian_reading, suppression_requirement, 359, 0.62).
narrative_ontology:measurement_basis(homo_su_t359, observed).
narrative_ontology:measurement(homo_su_t366, homoousios_christology__arian_reading, suppression_requirement, 366, 0.5).
narrative_ontology:measurement_basis(homo_su_t366, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__arian_reading, suppression_requirement, 381, 0.38).
narrative_ontology:measurement_basis(homo_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Arian controversy' covers one kernel (homoousios_christology) instantiating three structurally distinct arrangements: this file (arian_reading — created and subordinate), pro_nicene_reading (consubstantial; the arrangement that won at Nicaea 325 and Constantinople 381), and semi_arian_reading (similar substance; the middle-party compromise). Each has its own epsilon over its own arrangement: by the Arian lights the enforcement counted as legitimate discipline (moderate epsilon); under the pro-Nicene sibling the same imperial machinery runs in reverse after 381 and the beneficiary/victim sets flip. The files are linked through network.affects_constraints; merging them into one story would average over structurally distinct arrangements and violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
