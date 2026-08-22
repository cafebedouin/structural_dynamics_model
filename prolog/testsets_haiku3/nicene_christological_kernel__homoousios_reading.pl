% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Christological Doctrine
 *   domain: religious/ecclesiastical/theological
 *
 * SUMMARY:
 *   The Nicene homoousios doctrine asserts that Christ possesses the
 *   identical divine substance (ousia) as God the Father, with no ontological
 *   distinction or subordination. Adopted at the Council of Nicaea (325 CE)
 *   under imperial pressure, the doctrine enforces doctrinal uniformity
 *   through anathema, exile of dissenting bishops and theologians,
 *   confiscation of property from rival communities, and legal prohibition of
 *   alternative Christologies. This constraint is authored as one reading of
 *   the contested Nicene Christological kernel. The reading instantiates high
 *   extractiveness and suppression because the doctrine's persistence depends
 *   primarily on enforcement machinery rather than on philosophical or
 *   scriptural necessity — the homoiousios alternative (Christ is of similar
 *   but distinct substance) remained logically coherent, theologically
 *   defensible, and was taught in exile. The constraint benefits
 *   institutional episcopal authority and Roman imperial religious control
 *   while harming regional theological autonomy, theological diversity, and
 *   communities that held alternative readings. The claim/metric gap is
 *   intentional: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination function + asymmetric extraction + active enforcement) while
 *   the authored metrics describe substantially extractive, heavily enforced
 *   operation. The engine measures that divergence.
 *
 * KEY AGENTS:
 *   - institutional_episcopal_authority: Bishops gathered at Nicaea and their successors; set and enforce the homoousios standard; benefit from centralized doctrinal authority
 *   - roman_imperial_establishment: Constantine and imperial successors; leverage unified doctrine for political consolidation and religious control; indirect beneficiary (no direct enforcement cost)
 *   - regional_theological_communities: Syria, North Africa, Anatolia; taught alternative Christologies; face exile, property confiscation, legal persecution
 *   - gothic_arian_populations: Frontier populations; adopted Arian Christianity; declared heretical and subject to systematic delegitimization
 *   - north_african_autonomy: African episcopates; lose regional doctrinal authority to centralized imperial-backed councils
 *   - homoiousios_reading: The excluded theological alternative; remains logically coherent but is anathematized and suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.82).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.89).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Christological Doctrine").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "religious/ecclesiastical/theological").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, 'daeb59c9-677e-4379-9199-5862ae6c1cc8').
narrative_ontology:cs_kernel_codification('daeb59c9-677e-4379-9199-5862ae6c1cc8', formalized).
narrative_ontology:cs_authority_grounding('daeb59c9-677e-4379-9199-5862ae6c1cc8', extraction).
narrative_ontology:cs_interpretation_layer_present('daeb59c9-677e-4379-9199-5862ae6c1cc8').
narrative_ontology:cs_reading_relation('daeb59c9-677e-4379-9199-5862ae6c1cc8', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('daeb59c9-677e-4379-9199-5862ae6c1cc8', foundational, christ_identical_substance_father).
narrative_ontology:cs_axiom_status(christ_identical_substance_father, holdable).
narrative_ontology:cs_axiom_grounding('daeb59c9-677e-4379-9199-5862ae6c1cc8', christ_identical_substance_father, deontological).
narrative_ontology:cs_axiom('daeb59c9-677e-4379-9199-5862ae6c1cc8', foundational, subordination_incompatible_equality).
narrative_ontology:cs_axiom_status(subordination_incompatible_equality, holdable).
narrative_ontology:cs_axiom_grounding('daeb59c9-677e-4379-9199-5862ae6c1cc8', subordination_incompatible_equality, theological).
narrative_ontology:cs_reference_frame('daeb59c9-677e-4379-9199-5862ae6c1cc8', nicene_conciliar_supremacy).
narrative_ontology:cs_drift_state('daeb59c9-677e-4379-9199-5862ae6c1cc8', constantinople_381ce, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('daeb59c9-677e-4379-9199-5862ae6c1cc8', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, institutional_episcopal_authority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, roman_imperial_establishment).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_theological_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_populations).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_episcopacy).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, conciliar_dissenting_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops gathered at Council of Nicaea (325 CE) to define orthodoxy via the homoousios formula. They enforce uniformity through anathema, exile of dissenting theologians, and sequestration of property from rival communities. The doctrine secures their monopoly on legitimate doctrinal authority and their position as mediators of salvation. They collect institutional power and resources directly from enforcement.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, institutional_episcopal_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Constantine and his successors leverage unified Christian doctrine to consolidate political control and social cohesion across a fragmenting empire. A single authorized version of Christ's nature eliminates competing theological claims that could become rallying points for regional or ethnic resistance. Imperial power benefits from ecclesiastical uniformity without bearing direct enforcement costs.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, roman_imperial_establishment, beneficiary,
    institutional, generational, arbitrage, continental).

% Long-standing communities in Syria, North Africa, and Anatolia taught distinct Christologies (Arian, Nestorian, Monophysite traditions) grounded in their own scriptural exegesis and theological councils. After Nicaea, these readings are declared anathema; communities face exile of clergy, loss of church buildings, confiscation of property, and legal persecution. They can conform (deny their theological tradition), migrate (leave the empire), or resist clandestinely (high cost).
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, regional_theological_communities, payer,
    moderate, biographical, constrained, regional).

% Gothic tribes on the empire's frontier adopted Christian faith through Arian missionaries (Ulfilas translation, 4th century). After Nicaea, they are declared heretical; their faith tradition is systematically delegitimized within imperial territories and increasingly prohibited in law. They lack voice in conciliar decisions and cannot contest the doctrinal verdict. Exit requires abandoning Christianity entirely or migrating beyond imperial reach.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_populations, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, gothic_arian_populations, excluded).

% North African episcopal councils (Augustine, Carthage) had their own theological traditions and disciplinary authority. Nicaea asserts superiority of imperial-backed councils over regional assemblies, subordinating African bishops to a centralized orthodoxy. Regional autonomy in doctrine-setting is permanently constrained; bishops must implement or risk personal exile and loss of office.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_episcopacy, payer,
    moderate, generational, identity_locked, regional).

% At Nicaea itself, bishops (Eusebius of Nicomedia, Arius) who held homoiousios positions were outvoted and then exiled. Dissenting bishops at later councils face political and personal consequences: removal from office, confiscation of bishoprics, forced subscription to the homoousios formula. They can recant (renounce their theology), accept exile (leave the Christian hierarchy), or maintain underground communities.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, conciliar_dissenting_bishops, payer,
    organized, biographical, constrained, continental).

% The broader ecosystem of Christological interpretations (Docetic, Adoptionist, Subordinationist, and many regional variants) is systematically eliminated by the enforcement machinery. Theological plurality is the victim; the constraint exists to suppress it in favor of uniformity.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theological_diversity, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, theological_diversity).

% The homoiousios reading (Christ is of similar substance, ontologically distinct) is the primary excluded theological alternative. It was a live position at Nicaea held by multiple bishops and theologians; after the vote, its defenders are exiled and the reading is anathematized. The doctrine itself persists in exile communities but is suppressed from official Christian teaching.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, homoiousios_alternative, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, homoiousios_alternative).

% Later theological scholarship and ecumenical bodies recognize that the homoousios doctrine achieved institutional dominance through coercive enforcement, not through philosophical necessity or scriptural mandate. The constraint is observable as a historical fact of ecclesiastical power consolidation.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theological_scholarship_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, institutional_episcopal_authority).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified doctrinal statement on Christ's divine nature to prevent schism within the emerging imperial Christian establishment. Solves the coordination problem of defining orthodoxy once, centrally, rather than permitting regional councils to teach incompatible Christologies that could fragment Christian identity and (from the imperial view) threaten imperial religious authority and social cohesion.
% TRANSFER_FUNCTION: Transfers ecclesiastical authority from regional and local theological traditions to the imperial-backed episcopal council system; transfers political legitimacy to bishops who enforce uniformity; transfers theological autonomy from diverse communities to a single doctrinal standard. The resource flow is hierarchical: local traditions must renounce their reading or face exile and property loss; bishops who enforce the doctrine gain institutional power; the imperial state gains consolidated religious control.
% ABSENT_VOICES: Regional Arian communities, Gothic populations, North African episcopates, and theological traditions that taught homoiousios or other Christologies are structurally excluded from the conciliar process or marginalized within it. Their representatives are exiled after voting against the measure (Eusebius of Nicomedia, Arius). They cannot contest the doctrinal verdict through the legitimate channel (council) because the verdict itself delegitimizes their position retroactively.
% DISAPPEARANCE_RATIONALE: If the homoousios doctrine and its enforcement machinery vanished overnight, the Roman Empire would lose its unified Christian doctrinal monopoly. Regional councils would revive their own Christologies; Arian communities and Gothic populations would revert to their transmitted traditions or new syntheses; the episcopal hierarchy would fragment along theological lines; imperial religious control would dissolve. The Christian world would reorganize around theological diversity and regional autonomy — the exact opposite of what the constraint enforces.
% FOUNDING_PROBLEM: Early Christian communities developed incompatible Christologies (how is Christ divine? how does divinity coexist with humanity? is subordination necessary for monotheism?). The Roman imperial establishment faced a crisis of Christian fragmentation at precisely the moment it was trying to use Christianity for political consolidation. Emperor Constantine convened Nicaea to impose doctrinal uniformity and eliminate the theological grounds for schism and regional resistance.
% FOUNDING_PROBLEM_CORROBORATION: Constantine and Eusebius of Caesarea (court theologian) attest the founding problem is genuine: theological diversity threatened imperial unity. Historians and theologians outside the benefiting establishment attest that (a) theological diversity was not intrinsically destabilizing before Nicaea, and (b) the constraint's persistence depends on enforcement rather than on genuine resolution of the underlying theological question — the homoiousios alternative remained philosophically coherent and continued to be taught in exile communities. The question of whether homoousios is philosophically superior to homoiousios, or merely politically dominant, remains disputed in scholarship: the founding problem (need for uniformity) was real; the founding solution (homoousios as the only legitimate reading) was contested even as it was enforced.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs high (0.82 at interval end) and accelerates from 0.68 to 0.82 over 56 years because enforcement intensity increases: initial conciliar decision → imperial legal codification → systematic exile and property confiscation → second council at Constantinople (381 CE) reaffirms and hardens the standard. Suppression is higher still (0.89) because the constraint's persistence depends on actively excluding alternative readings and exiling their defenders, not on participant preference or voluntary consensus. Theater ratio climbs from 0.28 to 0.41 (moderate rise) because while the security function (preventing schism) remains real, an increasing share of enforcement activity defends the doctrinal monopoly itself rather than solving the underlying coordination problem — the homoousios formula is theologically defensible but not uniquely so, and the cost of maintaining uniformity rises as regional communities resist. The shared measurement grid assigns every metric at every time point: extractiveness accelerates, suppression hardens, and theater grows over the interval as the constraint matures from doctrinal decision to enforced monopoly. Accessibility collapse is high (0.78) because once the homoousios standard is codified and enforced, alternatives are pushed underground or exiled — learning alternative Christologies becomes legally and socially dangerous. Resistance is strong (0.72) from regional communities and Gothic populations precisely because the constraint extracts from them: they mount theological counterarguments, harbor exiled bishops, preserve banned texts, and maintain clandestine communities.
 *
 * PERSPECTIVAL GAP:
 *   The institutional episcopal authority seat and the Roman imperial seat should compute as beneficiaries with low directionality (near 0.0), deriving coordination function and institutional power gain from the constraint. The regional theological communities and Gothic populations should compute as targets with high directionality (near 1.0) — their exit options are severely constrained (identity_locked for North African bishops who must abandon their tradition to conform, trapped for Gothic populations with no legal standing). From the agenda-setter's position, the arrangement solves a genuine coordination problem (theological uniformity) and they maintain it through legitimate conciliar authority and imperial law. From the payer seats' positions, the same structure operates as enforced uniformity that suppresses legitimate theological diversity. The engine computes this perspectival divergence from the structural data — beneficiaries derive low χ (effective extraction scaled downward), payers derive high χ (effective extraction scaled upward by their constrained exit and high directionality). The authored claim (tangled_rope) asserts the presence of both coordination and extraction; the metrics quantify the dominance of extraction in the realized operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional episcopal authority: beneficiary role, institutional power, arbitrage exit → directionality near 0.0 (full beneficiary; they set the rules, they enforce the rules, they collect the institutional authority). Roman imperial establishment: beneficiary role (indirect collector of political control), institutional power, arbitrage exit → d near 0.0. Regional theological communities: payer role, moderate power, constrained exit → d near 0.75 (target: they bear costs, have limited options, power is moderate but insufficient to resist the institutional apparatus). Gothic Arian populations: payer role, powerless, trapped exit → d at 1.0 (full target: they have no exit, no legal standing, no voice in the process; they bear the entire cost of the constraint). North African autonomy: payer role, moderate power, identity_locked exit (bishops cannot abandon their office and tradition without losing their identity and station) → d near 0.80 (high target, but not absolute because moderate institutional power gives some negotiation room). The directionality overrides are not needed here because the structural derivation from role, power, and exit maps cleanly to the cardinal directions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint RESOLVES mandatrophy through the tangled_rope classification: it possesses both genuine coordination (uniformity across the Christian world) AND asymmetric extraction (enforcement of one reading over others, suppression of alternatives, transfer of power to institutional hierarchy). The founding problem (theological fragmentation threatening imperial religious control) was live at Nicaea but becomes increasingly contested: regional communities and exile bishops attest that theological diversity did not inherently cause schism before the enforcement machinery existed; the coordination benefit accrues primarily to the imperial state and institutional hierarchy, not to Christians generally; alternative readings remain philosophically defensible. The theater ratio's rise (0.28 → 0.41) indicates that maintenance activity is increasingly performative: after Constantinople II (381), the homoousios standard is settled law; enforcement becomes theatrical because the underlying theological question is not resolved, merely suppressed. The mismatch consumer would read: founding_problem_status = contested (is theological uniformity necessary?), disappearance_verdict = world_rearranges (the constraint's removal would restore theological diversity), theater_ratio rising → the constraint persists by institutional inertia and enforcement, not by genuine necessity. This profile flags a zombie constraint: the founding problem is dead (uniformity is maintained by force, not by solving the underlying problem), but the arrangement persists. Mandatrophy is not yet resolved in the 325-381 interval, but the trajectory is clear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_readings_logical_exclusivity,
    'Does the homoousios axiom logically foreclose the homoiousios alternative, or do they remain coexistent live positions?',
    'Philosophical analysis of the foundational axioms: does ''Christ is of the same substance'' logically entail ''no other reading of Christ''s nature is coherent''? Or are both readings defensible within different metaphysical frameworks (e.g., Platonic substance metaphysics vs. Aristotelian hylomorphism)?',
    'If logically foreclosed: the relation between readings is forecloses (rare, high-confidence divergence). If coexistent: the relation is coexists_with (the dominant classification for theological kernel contests). The impact flows to the network topology and to foreclosure-trigger conditions in the engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_readings_logical_exclusivity, conceptual, 'Whether the homoousios axiom logically forecloses the homoiousios alternative or both remain live philosophical positions.').

omega_variable(
    enforcement_dependency_vs_philosophical_necessity,
    'Is the homoousios doctrine''s persistence attributable to its philosophical superiority, or primarily to enforcement machinery and institutional dominance?',
    'Historical analysis: (a) Did theological scholarship outside the imperial hierarchy independently converge on homoousios, or does it persist mainly in institutions backed by imperial law? (b) Do exile communities preserve homoiousios and other readings as live theological options, or do they fade? (c) Is there post-Nicene philosophical refutation of homoiousios, or does the doctrine disappear through suppression rather than refutation?',
    'If philosophical necessity: the constraint''s extractiveness is partly justified coordination cost; tangled_rope classification holds with coordination dominance. If enforcement-dependent: extractiveness is primarily institutional extraction; reclassification toward snare becomes warranted. The impact affects the piton/zombie detection path and the mandatrophy_resolved verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_dependency_vs_philosophical_necessity, empirical, 'Whether the homoousios doctrine persists due to philosophical merit or enforcement machinery.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.89) primarily structural (exile, legal prohibition, property confiscation) or partly internalized (Christian communities internalize the verdict and abandon resistance)?',
    'Post-suppression trajectory: In regions where imperial enforcement weakens (Germanic invasions, late empire fragmentation), do homoiousios and alternative readings revive, or do they remain suppressed? If revival occurs, the suppression was structural and reversed; if they remain suppressed, internalization has occurred.',
    'If structural: the constraint''s effective suppression decays as enforcement capacity decays (piton mechanism). If internalized: the suppression persists even after enforcement removes, indicating deeper identity-fusion and more durable institutional capture. The impact affects lifecycle trajectory classification and exit-option reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (external barriers) or internalized (beliefs persist after barriers remove).').

omega_variable(
    imperial_motive_theological_vs_political,
    'Was Constantine''s motive for imposing homoousios genuinely theological (believing it correct) or primarily political (unified doctrine as a control mechanism)?',
    'Historical sources: Constantine''s own theological knowledge was limited (he was baptized only on deathbed); Eusebius of Caesarea''s court theology emphasizes political benefit of uniformity; later emperors explicitly manipulated doctrine for political ends. The question admits no definitive answer from surviving sources, but the political motive is more consistently documented.',
    'If theological: the constraint is an honest doctrinal consensus achieved through a flawed process (tangled_rope with coordination dominance remains). If political: the constraint is primarily institutional extraction using theological framing (reclassification toward snare). This is a preference-class omega — the answer depends on how much weight we assign to stated vs. structural motives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_motive_theological_vs_political, preference, 'Whether the constraint''s origin is genuine theological conviction or political control consolidation.').

omega_variable(
    false_summit_homoousios_as_natural_doctrine,
    'Is homoousios presented as a self-evident or necessarily true doctrine (a false natural law), or as one defensible reading among others?',
    'Textual analysis: Does ecclesiastical doctrine present homoousios as ''discovered'' (natural law of Christology) or ''established'' (chosen by council, enforced by authority)? The institutional framing after Nicaea presents it as discovered/necessary; historical scholarship recognizes the council''s role in selection. Homoousios is a human doctrinal choice, not a natural fact.',
    'If presented-as-natural (false summit): the constraint benefits from naturality framing that conceals its institutional origin; FSM analysis should trigger. If transparently institutional: the tangled_rope classification holds without false-summit interference. The impact affects the signature-detection phase and the ''naturality certification'' pathway.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_homoousios_as_natural_doctrine, empirical, 'Whether homoousios doctrine is presented as natural/necessary (false summit) or transparently as an institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.28).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t335, nicene_christological_kernel__homoousios_reading, theater_ratio, 335, 0.32).
narrative_ontology:measurement_basis(nice_tr_t335, observed).
narrative_ontology:measurement(nice_tr_t345, nicene_christological_kernel__homoousios_reading, theater_ratio, 345, 0.36).
narrative_ontology:measurement_basis(nice_tr_t345, observed).
narrative_ontology:measurement(nice_tr_t355, nicene_christological_kernel__homoousios_reading, theater_ratio, 355, 0.38).
narrative_ontology:measurement_basis(nice_tr_t355, observed).
narrative_ontology:measurement(nice_tr_t370, nicene_christological_kernel__homoousios_reading, theater_ratio, 370, 0.4).
narrative_ontology:measurement_basis(nice_tr_t370, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.41).
narrative_ontology:measurement_basis(nice_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.68).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t335, nicene_christological_kernel__homoousios_reading, base_extractiveness, 335, 0.74).
narrative_ontology:measurement_basis(nice_be_t335, observed).
narrative_ontology:measurement(nice_be_t345, nicene_christological_kernel__homoousios_reading, base_extractiveness, 345, 0.78).
narrative_ontology:measurement_basis(nice_be_t345, observed).
narrative_ontology:measurement(nice_be_t355, nicene_christological_kernel__homoousios_reading, base_extractiveness, 355, 0.8).
narrative_ontology:measurement_basis(nice_be_t355, observed).
narrative_ontology:measurement(nice_be_t370, nicene_christological_kernel__homoousios_reading, base_extractiveness, 370, 0.82).
narrative_ontology:measurement_basis(nice_be_t370, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.82).
narrative_ontology:measurement_basis(nice_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.71).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t335, nicene_christological_kernel__homoousios_reading, suppression_requirement, 335, 0.76).
narrative_ontology:measurement_basis(nice_su_t335, observed).
narrative_ontology:measurement(nice_su_t345, nicene_christological_kernel__homoousios_reading, suppression_requirement, 345, 0.82).
narrative_ontology:measurement_basis(nice_su_t345, observed).
narrative_ontology:measurement(nice_su_t355, nicene_christological_kernel__homoousios_reading, suppression_requirement, 355, 0.86).
narrative_ontology:measurement_basis(nice_su_t355, observed).
narrative_ontology:measurement(nice_su_t370, nicene_christological_kernel__homoousios_reading, suppression_requirement, 370, 0.88).
narrative_ontology:measurement_basis(nice_su_t370, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.89).
narrative_ontology:measurement_basis(nice_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.18).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, neo_nicene_consolidation_381ce).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, imperial_religious_control_mechanism).

% DUAL FORMULATION NOTE:
% The Nicene Christological kernel constrains two distinct reading-as-constraint stories: homoousios_reading (this file, high extractiveness, enforced uniformity) and homoiousios_reading (alternative reading, lower extractiveness, preserved theological distinction). The homoousios reading INFLUENCES the homoiousios reading because imperial enforcement of homoousios pushes homoiousios reading into exile and clandestine status. The two constraints share the same kernel (the Nicene doctrinal commitment) but instantiate different ε values and beneficiary/victim structures due to their competing readings of what that kernel demands. Writers should generate both stories with explicit network links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
