% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation Extraction Cycle
 *   domain: legal/political/anthropological
 *
 * SUMMARY:
 *   Blood-feud obligations among kinship groups in medieval and early-state
 *   societies constitute, under this reading, a destructive extraction cycle.
 *   The constraint extracts productive capacity (labor, warriors, resources)
 *   from kinship groups and productive populations through cycles of
 *   retaliation that reproduce the obligation itself. As centralized
 *   authority consolidates, it benefits structurally from feud persistence:
 *   the crown uses private feuding as evidence that kinship-based justice is
 *   inherently destructive, justifying royal monopoly on violence and
 *   enabling tax extraction in exchange for peace. The reading differs
 *   fundamentally from the stateless-coordination reading (which treats feud
 *   obligations as a functional deterrent in the absence of state capacity)
 *   and from the Christianized-pacification reading (which condemns feuding
 *   as violation of divine law). This reading frames feuding as an extraction
 *   mechanism that persists because it serves the interests of consolidating
 *   authority and preserves warrior-nobility status, not because it solves a
 *   justice problem that cannot be solved otherwise.
 *
 * KEY AGENTS:
 *   - Kinship groups bound by honor obligation: bear direct resource and mortality costs; identity-locked to the feud through kinship identity fusion
 *   - Productive populations: powerless, territorially trapped, bear collateral destruction costs without participation in feud decisions
 *   - Royal consolidating authority: agenda-setter; benefits from feud persistence by using it to justify state monopoly and extract tributary legitimacy
 *   - Warrior nobility: beneficiary seat; maintains status through feud-legitimated martial prowess; constrained by dependence on the status the feud provides
 *   - Ecclesiastical authorities: excluded seat; would introduce counter-authority (divine law) that threatens feud legitimacy
 *   - Merchant networks: excluded seat; demand commercial-law substitutes; mobility and transnational networks make them exit-capable if constraint could be bypassed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.82).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.77).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal/political/anthropological").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, 'a643abc8-2f2e-4870-9651-dd5f9fdc95f4').
narrative_ontology:cs_kernel_codification('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', distributed).
narrative_ontology:cs_authority_grounding('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', lineage).
narrative_ontology:cs_interpretation_layer_present('a643abc8-2f2e-4870-9651-dd5f9fdc95f4').
narrative_ontology:cs_reading_relation('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', foundational, feud_obligation_extracts_beyond_necessity).
narrative_ontology:cs_axiom_status(feud_obligation_extracts_beyond_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', feud_obligation_extracts_beyond_necessity, empirically_contingent).
narrative_ontology:cs_axiom('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', foundational, royal_authority_benefits_from_private_justice_failure).
narrative_ontology:cs_axiom_status(royal_authority_benefits_from_private_justice_failure, holdable).
narrative_ontology:cs_axiom_grounding('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', royal_authority_benefits_from_private_justice_failure, instrumental).
narrative_ontology:cs_reference_frame('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', kinship_honor_deterrence_framework).
narrative_ontology:cs_drift_state('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', royal_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a643abc8-2f2e-4870-9651-dd5f9fdc95f4', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_consolidating_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, warrior_nobility_preserving_status).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kinship_groups_engaged_in_feuding).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, productive_populations_in_feud_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Kinship lineages bound by honor obligation to prosecute blood vengeance for slain members. They bear the direct costs: warrior mortality, productive labor diverted to feud activity, destructive raids on holdings, cycles of counter-retaliation that consume resources across generations. Exit from the obligation is perceived as family dishonor and social death; kinship identity is constituted through the feud commitment. The constraint persists because exit means ejection from the group that defines personhood.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kinship_groups_engaged_in_feuding, payer,
    moderate, generational, identity_locked, regional).

% Peasants, artisans, merchants, and smallholders in territories where feuding kinship groups operate. They bear indirect costs through destruction of productive land, interrupted trade, forced support of warrior bands, and exposure to collateral violence. Their exit options are severely constrained: migration requires abandoning holdings and social networks; the constraint operates territorially over populations with limited mobility.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, productive_populations_in_feud_zones, payer,
    powerless, biographical, trapped, regional).

% Emerging monarchical or proto-state authority seeking to consolidate territorial monopoly on violence and legitimate taxation. The persistence of blood-feud obligations fragments authority and prevents collection of stable tribute. The royal authority benefits structurally: feud obligations demonstrate why private kinship-based justice is destructive, justifying state monopoly on violence as legitimate; the crown can extract tax revenue by promising to suppress feuding and provide royal justice in its place. The authority actively enforces the framing of feuding as illegitimate while consolidating its own monopoly.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_consolidating_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Military aristocracy whose social position and landholding depend on demonstrated prowess in feud prosecution and defense. The feud obligation sustains their raison d'être: it justifies warrior retainers, prestigious martial activity, and the narrative that nobility's role is indispensable for group protection. They benefit from the constraint because it maintains the structural need for their services; subordination to royal justice would eliminate their independent authority. Their exit is constrained by dependence on feud-legitimated status, even though the long-term trajectory favors royal consolidation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, warrior_nobility_preserving_status, beneficiary,
    powerful, generational, constrained, regional).

% Church institutions and clerical advocates for pacification who would condemn blood feuds as violations of divine law and champion penance-based restitution or royal justice as alternatives. They are systematically excluded from feud enforcement because their presence would introduce counter-authority claiming moral superiority to kinship obligation. Their exclusion is what suppression enforces.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authorities, excluded,
    institutional, generational, analytical, national).

% Long-distance trading communities whose economic survival depends on predictable property rights and commercial routes unconstrained by feuding cycles. They would advocate for suppression of feud obligations and substitution of merchant-law or royal-backed commerce law. Their exclusion from the feud framework is active: they are confined to diaspora communities, taxed as outsiders, and their property claims denied kinship-group protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, merchant_networks_and_trading_diasporas, excluded,
    organized, biographical, mobile, continental).

% Anthropological, historical, and comparative-political analyst examining the constraint from outside the participating communities, measuring resource depletion, mortality rates, territorial consolidation patterns, and the structural relationship between feud persistence and royal authority emergence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_consolidating_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates private justice and deterrence: establishes that harm to any kinship member triggers organized retaliatory response, providing both justice restoration and predation deterrence in the absence of centralized court authority.
% TRANSFER_FUNCTION: Moves productive capacity (warrior labor, material resources, territorial consolidation capability) from kinship groups and productive populations into cycles of destruction—raids, fortifications, warrior maintenance—that reproduce the obligation itself. Secondary transfer: moves legitimacy claim to emerging royal authority by demonstrating private kinship-based justice is inherently destructive.
% ABSENT_VOICES: Ecclesiastical authorities would condemn feuding as violation of divine law and advocate penance-based restitution or royal justice. Merchant networks would demand suppression of feuding to enable commerce and predictable property rights. Productive populations themselves have no voice in feud-obligation setting despite bearing the most severe costs; they are passive subjects to the feud constraint.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished, territorial consolidation would accelerate, productive investment in holdings would increase, warrior nobility would lose their raison d'être and status-justification, royal authority would lose a foundational legitimacy claim (that private justice is inherently destructive), and long-distance commerce would expand into previously feuding zones. The political economy of the region would reorganize around productive consolidation rather than feud cycles.
% FOUNDING_PROBLEM: In stateless or weakly-centralized societies, kinship-based violent retaliation answers the problem: how is justice achieved and predation deterred when no centralized authority exists to punish offenders or provide neutral arbitration?
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and consolidating-state advocates attest the founding problem is obsolete: centralized courts, enforcement capacity, and royal justice now provide superior solutions. Independent anthropological and historical analysis confirms that the founding problem IS live in genuinely stateless contexts, but that feud obligations persist in consolidating kingdoms BEYOND functional necessity—they persist because suppression of alternatives (ecclesiastical counter-authority, merchant-law frameworks) and elite benefit (warrior nobility status, royal legitimacy) sustain them. The persistence-beyond-necessity is the mark of extraction.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.82 reflects the permanent drain on productive capacity: the constraint creates cycles of destruction that consume resources faster than they can be replaced, preventing wealth accumulation and territorial consolidation. Early in the interval (T=0, value 0.58) the extraction is moderate because state authority is not yet consolidated enough to benefit from feud persistence; as royal consolidation proceeds (T=40, value 0.82), extraction increases because the crown can more effectively extract tax revenue by positioning itself as the alternative to destructive feuding. Suppression at 0.77 is high because the constraint's persistence depends on actively suppressing ecclesiastical authority's counter-narrative (divine law condemning vengeance) and merchant-network alternatives. Theater rises from 0.18 to 0.42 over the interval, indicating that as royal authority consolidates, an increasing share of feud-related activity becomes performative: maintaining the narrative that private justice is destructive (rather than actually prosecuting disputes) becomes more important than the original deterrent function. This pattern marks a Piton trajectory: the foundational coordination function (private deterrence in absence of state capacity) atrophies as state capacity emerges, but the obligation persists through theatrical maintenance (warrior prowess displays, honor narratives) and royal benefit extraction (monopoly-on-violence legitimacy). The measurements share one grid so every metric is authored at every time point (shared alignment per OQ-56 rider).
 *
 * PERSPECTIVAL GAP:
 *   From the kinship-group seat, the constraint is an honor obligation maintained by collective belief and enforced through shame/exclusion—a coordination mechanism that provides justice when no other option exists. From the royal-authority seat, the same constraint is a demonstration of private-justice failure that justifies state monopoly. From the productive-population seat, it is pure destruction with no consultation and no exit. The engine computes these as different directionalities (kinship groups and productive populations as targets, royal authority as beneficiary, merchant networks and ecclesiastical bodies as excluded) stemming from the same structural arrangement. The authored claim (snare: pure extraction with suppression of alternatives) is consistent with the directionality-derived reading (high d for victims, low d for beneficiaries, trapped/identity-locked exit modulation), but the claim and metrics diverge from the stateless-coordination reading's alternative framing of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Kinship groups: d approaches 1.0 (full target). They pay the material and mortality costs; their exit is identity-locked (kinship identity is fused to the feud obligation, making exit unthinkable within the group's own frame); this produces high effective extraction (χ) for them even though they do not see themselves as targets but as honorable agents. Productive populations: d = 1.0 (full target). They have no voice in the obligation, bear collateral costs, and are territorially trapped with no exit option. Royal consolidating authority: d approaches 0.0 (full beneficiary). The constraint serves their consolidation agenda by providing legitimacy for monopoly on violence and justifying tax extraction; their exit options are broad (arbitrage-capable: they can move toward other legitimacy sources). Warrior nobility: d near 0.5-0.3 (complex). They benefit from status preservation through feud-legitimated prowess, but they are constrained by dependence on the feud for their identity and position. As royal authority consolidates, their actual position degrades (they lose independence), but the feud obligation keeps them nominally in the beneficiary set until subordination to royal justice is completed. Ecclesiastical authorities and merchant networks: excluded entirely from the directionality computation because they are structurally outside the constraint (not beneficiaries, not victims, but suppressed alternatives). The directionality derivation shows feud-participants as targets and royal authority as beneficiary, consistent with the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (private deterrence and justice in the absence of state capacity) is live in stateless contexts and was genuinely functional when this constraint emerged. As royal authority consolidates and state courts develop, the founding problem shifts to contested status: does the constraint still solve a necessary problem (some argue yes—kinship honor still provides deterrence; some argue no—state courts are now superior)? By the end-state of the interval (T=40), the founding problem is substantively dead in the consolidating realm: royal courts exist, royal justice is available, and the constraint persists through institutional inertia and elite benefit (warrior nobility status preservation, royal authority legitimacy). The disappearance verdict (world_rearranges) is robust: the constraint's removal would accelerate territorial consolidation, redirect productive capacity, and eliminate the crown's founding-problem legitimacy narrative. The constraint does not meet Piton criteria cleanly because suppression is still high and extraction is still extracting value (not purely performative), but the measurement trajectory and theater-ratio rise suggest Piton emergence: the coordination function is atrophying, and persistence rides increasingly on inertia and benefit capture rather than on solving an unsolved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_locked_vs_choice,
    'Is kinship-obligation participation genuinely identity-locked (agents cannot exit without self-conception collapse), or is the perceived identity-lock a contingent cultural belief that could shift with external authority and incentive changes?',
    'Historical observation of societies undergoing state consolidation: do kinship groups voluntarily embrace royal justice once it becomes available, or do they maintain feud obligations against material incentives to exit? Post-consolidation anthropological study of kinship-group identity in pacified regions.',
    'If identity-locked is contingent and belief-driven rather than structural, the suppression mechanisms (ecclesiastical exclusion, merchant-network exclusion) become MORE important to explaining persistence—the constraint depends on controlling the narrative frame. If genuinely structural to kinship identity, the constraint persists through the identity fusion even absent active suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_choice, empirical, 'Whether identity-lock is structural or narrative-contingent').

omega_variable(
    royal_benefit_vs_unintended_consequence,
    'Does royal authority deliberately sustain blood-feud obligations to justify monopoly on violence, or does the crown benefit incidentally from feud persistence while genuinely attempting suppression?',
    'Archival evidence of royal policy: do royal courts actively suppress feuding or selectively enforce? Do they criminalize feud prosecution or merely privilege royal justice? Historical comparison of state consolidation rates in regions where feud was actively suppressed vs. tolerated.',
    'If deliberate, the constraint is more purely extractive and royal authority should be classified as full beneficiary (d near 0.0). If incidental benefit, the beneficiary classification remains but reflects secondary effect rather than primary strategy, potentially shifting toward snare->tangled-rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_benefit_vs_unintended_consequence, empirical, 'Whether royal benefit is strategic or incidental to feud persistence').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high measured suppression (0.77) structural—external barriers, legal punishment, military enforcement—or internalized—kinship groups believe the feud is illegitimate and suppress themselves?',
    'Post-consolidation ethnographic study: when external enforcement pressure ceases (weak central authority, collapse of state), do kinship groups resume feuding, or has the belief-internalization persisted? Anthropological interview data on whether participants frame suppression as external coercion or moral conviction.',
    'If suppression is structural, the constraint''s breakdown depends on maintaining enforcement capacity; if internalized, the constraint persists even when enforcement capacity degrades. High internalization would suggest the constraint has shifted toward Piton (inertial maintenance through belief rather than force) earlier in the trajectory than measurements show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    alternative_readings_kernel_framing,
    'Is the kernel (blood-feud obligation practice) genuinely stable across the three readings, or does the practice shift its meaning so fundamentally under different authority frames that each reading refers to different instantiations?',
    'Linguistic and conceptual analysis: does ''blood feud'' mean the same practice under royal justice versus kinship honor versus divine law? Are the behavioral patterns identical or do they diverge?',
    'If the readings refer to genuinely different practices masquerading under one label, this is not one kernel with three readings but three separate constraints—the decomposition would apply per ε-invariance principle. If the kernel is stable (same behavioral pattern, different value frames), the reading-relations and axiom-distinction model holds as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_kernel_framing, conceptual, 'Whether the kernel is observationally stable across readings or decomposable into multiple constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(feud_tr_t5, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(feud_tr_t15, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(feud_tr_t30, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(feud_tr_t35, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(feud_be_t5, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(feud_be_t15, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(feud_be_t30, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(feud_be_t35, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(feud_su_t5, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(feud_su_t10, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(feud_su_t15, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(feud_su_t30, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(feud_su_t35, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 35, 0.77).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.18).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the feud_obligation_kernel. The extraction_cycle_reading (this constraint) differs fundamentally in its framing of the same kernel practice: it emphasizes productive destruction and elite benefit-capture, where the stateless_coordination_reading emphasizes functional justice provision and the Christianized reading emphasizes divine-law violation. The three readings form a constraint family linked by the shared kernel. ε values diverge substantially across readings: the extraction_cycle reading authors high ε (0.82) because it frames the obligation's operation as resource destruction; the stateless_coordination reading would author lower ε (likely 0.3-0.4) because it frames the obligation as solving a necessary problem; the Christianized reading would author ε as applied to the standing practice under contest, diverging depending on whether the reading is abolitionist or reformist. Decomposition is justified by OQ-26 / ε-invariance: different readings produce different ε values for the same kernel practice, so they instantiate different constraints, not observations of one constraint from multiple seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
