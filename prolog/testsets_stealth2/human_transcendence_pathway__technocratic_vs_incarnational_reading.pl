% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__technocratic_vs_incarnational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__technocratic_vs_incarnational_reading, []).

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
 *   constraint_id: human_transcendence_pathway__technocratic_vs_incarnational_reading
 *   human_readable: Technocratic Transcendence Pathway (Optimization Regime Read Through Incarnational Lights)
 *   domain: political_theology/technology_ethics/bioethics
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel 'how is human
 *   transcendence attained?' — the technocratic_vs_incarnational_reading,
 *   which holds the question as a fork between transcendence achieved through
 *   technological optimization and elimination of limits (the ascendant
 *   transhumanist/posthumanist program) and transcendence received as gift in
 *   vulnerability (the Incarnational pattern). Per the fixed epsilon-referent
 *   rule, the referent of the authored metrics is the STANDING arrangement
 *   under contest: the technocratic optimization regime as it actually
 *   operates in biomedical research, clinical priority-setting, and the
 *   cultural politics of aging and disability — assessed by this reading's
 *   own Incarnational lights. The Incarnational pole is the reading's
 *   endorsed alternative and is deliberately NOT the referent; a companion
 *   story would be needed to classify it. The regime coordinates real
 *   therapeutic success while transferring standing, attention, and resources
 *   toward the optimizable and away from those it recasts as problems or
 *   failures; enforcement runs through funding gatekeeping, screening
 *   regimes, professional norms, and cultural grading rather than statute
 *   alone. KEY AGENTS (by structural relationship): -
 *   biotech_longevity_industry: Agenda-setter (institutional/arbitrage) —
 *   defines which limits count as solvable, collects revenue and prestige -
 *   enhancement_capable_elites: Primary beneficiary (powerful/arbitrage) —
 *   purchases capability extension, funds the agenda -
 *   elderly_and_care_dependent: Primary target (powerless/trapped) — bears
 *   reclassification as problem or failure - disabled_communities: Organized
 *   target (organized/constrained) — bears prevention-and-elimination logic
 *   inside systems they cannot leave - chronically_ill_uninsured: Target
 *   (powerless/trapped) — absorbs care deprioritization under
 *   return-on-investment reasoning - care_workforce: Target
 *   (organized/constrained) — bears status and wage discounting of custodial
 *   vocation - severely_cognitively_impaired: Excluded seat
 *   (powerless/trapped) — cannot object; the arrangement's deepest test case
 *   - catholic_bioethical_teaching_office: Analytical observer
 *   (institutional/analytical) — sees the full fork, articulates the
 *   Incarnational counter-position
 *
 * KEY AGENTS:
 *   - biotech_longevity_industry: Agenda-setter (institutional/arbitrage) — defines which limits count as solvable, collects revenue and prestige
 *   - enhancement_capable_elites: Primary beneficiary (powerful/arbitrage) — purchases capability extension, funds the agenda
 *   - elderly_and_care_dependent: Primary target (powerless/trapped) — bears reclassification as problem or failure
 *   - disabled_communities: Organized target (organized/constrained) — bears prevention-and-elimination logic inside systems they cannot leave
 *   - chronically_ill_uninsured: Target (powerless/trapped) — absorbs care deprioritization under return-on-investment reasoning
 *   - care_workforce: Target (organized/constrained) — bears status and wage discounting of custodial vocation
 *   - severely_cognitively_impaired: Excluded seat (powerless/trapped) — cannot object; the arrangement's deepest test case
 *   - catholic_bioethical_teaching_office: Analytical observer (institutional/analytical) — sees the full fork, articulates the Incarnational counter-position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__technocratic_vs_incarnational_reading, 0.72).
domain_priors:suppression_score(human_transcendence_pathway__technocratic_vs_incarnational_reading, 0.68).
domain_priors:theater_ratio(human_transcendence_pathway__technocratic_vs_incarnational_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__technocratic_vs_incarnational_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__technocratic_vs_incarnational_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(human_transcendence_pathway__technocratic_vs_incarnational_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__technocratic_vs_incarnational_reading, tangled_rope).
narrative_ontology:human_readable(human_transcendence_pathway__technocratic_vs_incarnational_reading, "Technocratic Transcendence Pathway (Optimization Regime Read Through Incarnational Lights)").
narrative_ontology:topic_domain(human_transcendence_pathway__technocratic_vs_incarnational_reading, "political_theology/technology_ethics/bioethics").

domain_priors:requires_active_enforcement(human_transcendence_pathway__technocratic_vs_incarnational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__technocratic_vs_incarnational_reading, '760d544e-121a-466b-b5f5-3a2f592965da').
narrative_ontology:cs_kernel_codification('760d544e-121a-466b-b5f5-3a2f592965da', distributed).
narrative_ontology:cs_authority_grounding('760d544e-121a-466b-b5f5-3a2f592965da', expertise).
narrative_ontology:cs_interpretation_layer_present('760d544e-121a-466b-b5f5-3a2f592965da').
narrative_ontology:cs_reading_relation('760d544e-121a-466b-b5f5-3a2f592965da', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('760d544e-121a-466b-b5f5-3a2f592965da', human_transcendence_pathway__jerusalem_reading, coexists_with).
narrative_ontology:cs_axiom('760d544e-121a-466b-b5f5-3a2f592965da', foundational, transcendence_fork_optimization_or_grace).
narrative_ontology:cs_axiom_status(transcendence_fork_optimization_or_grace, holdable).
narrative_ontology:cs_axiom_grounding('760d544e-121a-466b-b5f5-3a2f592965da', transcendence_fork_optimization_or_grace, theological).
narrative_ontology:cs_axiom('760d544e-121a-466b-b5f5-3a2f592965da', secondary, vulnerability_as_site_of_grace).
narrative_ontology:cs_axiom_status(vulnerability_as_site_of_grace, holdable).
narrative_ontology:cs_axiom_grounding('760d544e-121a-466b-b5f5-3a2f592965da', vulnerability_as_site_of_grace, theological).
narrative_ontology:cs_reference_frame('760d544e-121a-466b-b5f5-3a2f592965da', grace_received_in_vulnerability).
narrative_ontology:cs_drift_state('760d544e-121a-466b-b5f5-3a2f592965da', contemporary_optimization_paradigm, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('760d544e-121a-466b-b5f5-3a2f592965da', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__technocratic_vs_incarnational_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__technocratic_vs_incarnational_reading, enhancement_capable_elites).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__technocratic_vs_incarnational_reading, biotech_longevity_industry).
narrative_ontology:constraint_victim(human_transcendence_pathway__technocratic_vs_incarnational_reading, elderly_and_care_dependent).
narrative_ontology:constraint_victim(human_transcendence_pathway__technocratic_vs_incarnational_reading, disabled_communities).
narrative_ontology:constraint_victim(human_transcendence_pathway__technocratic_vs_incarnational_reading, chronically_ill_uninsured).
narrative_ontology:constraint_victim(human_transcendence_pathway__technocratic_vs_incarnational_reading, care_workforce).
narrative_ontology:constraint_victim(human_transcendence_pathway__technocratic_vs_incarnational_reading, severely_cognitively_impaired).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Funds and directs the research pipeline that determines which forms of human limitation count as solvable engineering problems — longevity, cognitive enhancement, reproductive selection, performance extension. Sets clinical and investment priorities through grant-making, venture capital, publication venues, and regulatory lobbying, and collects revenue, prestige, and talent concentration from the optimization economy. Exit is easy: capital and teams relocate to whichever jurisdiction offers the friendliest regime.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, biotech_longevity_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Wealthy individuals, executives, and professionals who purchase or access the frontier of enhancement — advanced therapeutics, longevity protocols, genomic screening, cognitive and physical optimization. They gain years of capability and standing unavailable to others, and their demand finances the industry's agenda. They can travel for treatment and are insulated from the care-rationing consequences their preferred priorities impose elsewhere in the system.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, enhancement_capable_elites, beneficiary,
    powerful, generational, arbitrage, global).

% Aging and dependent people whose condition the optimization frame recasts as a problem to be solved or a failure to be managed. They bear the costs: care systems organized around cure-or-decline narratives, diminished social standing once productivity ends, and pressure to accept escalating intervention as the price of being treated as worth treating. They cannot exit aging, and they cannot opt out of the cultural frame that grades their worth by trajectory.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, elderly_and_care_dependent, payer,
    powerless, biographical, trapped, global).

% Disability advocates and communities living inside systems — healthcare, benefits, prenatal-screening regimes — increasingly structured by prevention-and-elimination logic. They bear the cost of a culture that reads their existence as preventable error: selective-termination pressure, research funding that flows past accommodation toward cure, and public rhetoric that treats dependence as indignity. They resist through organized advocacy but cannot leave the systems that grade them.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, disabled_communities, payer,
    organized, generational, constrained, global).

% People with chronic conditions and thin coverage whose treatment decisions are made under return-on-investment reasoning. When research dollars and clinical attention chase enhancement frontiers, maintenance care for conditions that cannot be won is deprioritized; they absorb the resulting gaps in coverage and attention directly, with no exit from their bodies or from the payment structures that ration around them.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, chronically_ill_uninsured, payer,
    powerless, immediate, trapped, national).

% Hospice nurses, home-health aides, and caregivers whose vocation is custodial presence with people who will not be optimized. The optimization economy concentrates prestige and pay at the frontier and treats custodial care as residual, so they bear wage stagnation and status discounting even as demand for their work grows. Many experience the work as calling rather than job, which makes exit costly in a way wages do not capture.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, care_workforce, payer,
    organized, biographical, constrained, global).

% People with advanced dementia, severe intellectual disability, and pre-verbal brain injury who cannot articulate any position at all. Their interests enter the conversation only through proxies — families, guardians, ethicists — whose standing the optimization frame discounts as sentimental. Whatever the arrangement decides about them reveals what it believes human worth rests on; they are present in the system and absent from every table that governs it.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, severely_cognitively_impaired, excluded,
    powerless, immediate, trapped, global).

% The magisterial and bioethical apparatus of the Catholic Church — congregations, pontifical academies, hospital-system ethics boards — articulating the Incarnational counter-position: transcendence as gift received in vulnerability, with solidarity toward the least as the measure of a society's health. It observes the full structure of the optimization economy, publishes sustained critique, and forms conscience within the health systems it sponsors, but sets no part of the research agenda it examines.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__technocratic_vs_incarnational_reading, catholic_bioethical_teaching_office, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__technocratic_vs_incarnational_reading, biotech_longevity_industry).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__technocratic_vs_incarnational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates biomedical research, capital, clinical training, and regulatory approval around a shared definition of health as expanded capability — pooling investment and standardizing science so that disease, injury, and some effects of aging are addressed once, at scale, rather than suffered separately.
% TRANSFER_FUNCTION: Moves research funding, clinical attention, and cultural prestige toward limit-elimination projects serving the enhancement-capable, and away from custodial care of those who cannot be optimized; correspondingly moves the costs of obsolescence — deprioritized care, discounted standing, screening pressure — onto the elderly, disabled, chronically ill, and cognitively impaired.
% ABSENT_VOICES: The severely cognitively impaired and the pre-verbal cannot object at all; their interests arrive only through proxies whose standing the frame discounts. Care workers and disability advocates object but sit outside the funding and agenda-setting tables where health priorities are actually set. The dying — whom the Incarnational pole centers — lose their seat entirely once death is coded as engineering failure rather than a human moment to be accompanied.
% DISAPPEARANCE_RATIONALE: If the optimization imperative vanished overnight, research portfolios, capital allocation, clinical priority-setting, prenatal-screening defaults, and the cultural meaning of aging and disability would all reorganize around different questions — what is owed to the vulnerable rather than what can be eliminated — and the enhancement economy's revenue and prestige streams would dry up or redirect.
% FOUNDING_PROBLEM: Disease, injury, aging, and physical limitation cause immense suffering and premature death; the arrangement was built to relieve that suffering through the systematic application of technology to human limits.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: WHO Global Burden of Disease epidemiology and the palliative-medicine literature attest independently that illness, aging, and mortality remain mass sources of suffering, and the Catholic teaching office — an observer seat with no stake in enhancement revenue — attests the same from its own tradition. None of these sources depends on the industry's self-description; what they corroborate is the founding problem, not the arrangement's solution to it.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__technocratic_vs_incarnational_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__technocratic_vs_incarnational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__technocratic_vs_incarnational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__technocratic_vs_incarnational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__technocratic_vs_incarnational_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__technocratic_vs_incarnational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__technocratic_vs_incarnational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__technocratic_vs_incarnational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because resource flows are decoupled from need: research dollars, clinical attention, and prestige track optimizability rather than suffering, so the seats least able to benefit from enhancement bear the largest share of deprioritization. Suppression (0.68) is structural and rising: the regime persists not by participant preference but by funding gatekeeping, prenatal-screening defaults, professional norms that read limit-acceptance as clinical defeat, and platform-amplified futurist narrative. Theater ratio (0.30) is moderate-low and climbing slowly — the therapeutic core is real (anesthesia, oncology, vaccines are not performance), but a growing share of activity is promotional spectacle (longevity summits, investor narratives, escape-death rhetoric) that substitutes for delivery. Accessibility_collapse (0.45) is mid-range: genuine alternatives persist — hospice and palliative traditions, disability-justice scholarship, monastic medicine, the Catholic hospital system — but they are marginalized rather than eliminated. Resistance (0.60) is substantial and organized: disability-led critiques of cure ideology, Catholic bioethics, and bioconservative coalitions actively contest the frame. The measurement series run on one shared time grid (t=0,8,16,24,32,40) with all three tracked metrics authored at every point; trajectories show extraction accumulation and enforcement hardening over the interval, consistent with rent layering onto a functioning coordination core. Suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat should compute a very different type from the payer seats: from inside the industry the arrangement is the coordination it built and honestly maintains — therapies work, capabilities expand, suffering is relieved. From the trapped payer seats the same structure operates as a grading machine that prices their continued existence by optimized trajectory. The observer seat (catholic_bioethical_teaching_office) sees the fork entire: it grants the therapeutic good while locating the extraction in what the good is made to mean. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: enhancement_capable_elites combine beneficiary position with arbitrage-grade exit (jurisdictional shopping for treatment), pushing derived directionality furthest toward d=0; biotech_longevity_industry collects revenue directly as agenda-setter. Targets cluster near the full-target end: elderly_and_care_dependent and chronically_ill_uninsured are trapped (no exit from bodies, payment structures, or the cultural frame), so their effective extraction approaches the unscaled maximum; disabled_communities are constrained rather than trapped — organized resistance moderates but does not remove their exposure; care_workforce is dual-positioned (wages flow to them, status and wage discounting flow from them) and derives a net-target directionality. The excluded seat (severely_cognitively_impaired) sits at maximal exposure with zero voice. Global spatial scope raises verification difficulty and modestly amplifies effective extraction for the target seats. Directionality overrides were considered for care_workforce (partial wage subsidy) but the structural derivation from declared roles plus exit options already yields the correct ordering, so no overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that disease, aging, and limitation cause immense suffering — remains live and is corroborated from outside the benefiting parties (burden-of-disease epidemiology, palliative-medicine literature, the Catholic teaching office), so no mandatrophy is declared and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The tangled_rope classification guards against both symmetrical errors. A pure-snare verdict would erase the regime's real coordination achievement — anesthesia, vaccines, and functional restoration are not cover stories — and license indiscriminate rejection of therapeutic capacity. A pure-rope verdict would launder the extraction riding on that capacity: the grading of worth by optimized trajectory, the deprioritization of custodial care, and the suppression of limit-acceptance as a live human possibility. Holding both faces in one classification keeps the reform question precise: not whether to abandon the therapeutic project, but whether its coordination function can be separated from the extraction layered onto it (see the therapy_enhancement_separability omega).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'Is the technocratic-versus-incarnational fork the correct decomposition of the human_transcendence_pathway kernel, or do the babel_reading (collective self-sufficiency through unified systems) and jerusalem_reading (communion through patient labor under blessing) axes cut the space more accurately?',
    'Comparative classification across the three sibling stories: if the technocratic and babel readings converge on identical victim sets and enforcement structures, the fork collapses into the babel axis; if this reading''s Incarnational pole converges with jerusalem, the pair reduces to one axis.',
    'If the fork is redundant, this story''s victim set and epsilon merge with a sibling''s and the family shrinks to two constraints; if the fork is real, the three-way family stands and per-seat classifications diverge across all three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether this reading''s optimization/grace fork is independent of the babel/jerusalem axes.').

omega_variable(
    epsilon_referent_fix,
    'Is epsilon correctly authored for the standing technocratic optimization arrangement (assessed by this reading''s Incarnational lights), rather than for the Incarnational discipline the reading endorses?',
    'Re-read the story''s referent declaration against the fixed epsilon-referent rule: the endorsed alternative must never serve as referent; a separate companion story would be required if the Incarnational discipline itself is to be classified.',
    'If the referent were flipped to the Incarnational arrangement, epsilon would fall toward coordination-cost levels and the classification would invert; keeping the referent fixed preserves reading-indexed high extraction over a stable object.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_fix, conceptual, 'Fixes what epsilon is about: the technocratic arrangement, never the endorsed alternative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding gatekeeping, screening regimes, professional norms, platform amplification) or internalized (internalized ableism, gerontophobia, the optimization subject''s habit of grading self-worth by trajectory)?',
    'Post-concession trajectory analysis: communities that win structural protections (disability-rights law, parity mandates) but show persistent self-worth collapse indicate internalized residue; parallel improvement in standing and self-assessment indicates structural dominance.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the frame with them after barriers fall, and purely structural reform would under-deliver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized split of the suppression scalar.').

omega_variable(
    constructed_vs_constitutive_striving,
    'Is the drive to eliminate human limits a constitutive feature of human striving (emerging wherever capability grows, mountain-like in character) or a culturally constructed program sustained by identifiable funding, credentialing, and enforcement?',
    'Cross-cultural and historical comparison of societies with equivalent technical capacity but different transcendent framings; funding-flow and institution-building history of the enhancement economy.',
    'If constitutive, suppression aimed at the drive itself is futile and only the extractive layer is addressable; if constructed, the whole arrangement is reformable and its persistence reflects enforcement rather than nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_constitutive_striving, empirical, 'Naturality of the limit-elimination drive.').

omega_variable(
    therapy_enhancement_separability,
    'Are genuine therapy (restoring function, relieving suffering) and the optimization economy''s grading of worth by enhanced trajectory structurally separable within the same research and clinical apparatus?',
    'Natural experiments in systems that ring-fence custodial-care funding from enhancement research and development: if therapeutic outcomes hold while the grading dynamic recedes, the functions are separable.',
    'If separable, this tangled-rope reading decomposes into a coordination story plus an extraction story (two files, linked); if inseparable, part of the measured extraction is the unavoidable price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapy_enhancement_separability, empirical, 'Separability of the coordination and extraction components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__technocratic_vs_incarnational_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(huma_tr_t8, human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(huma_tr_t16, human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(huma_tr_t24, human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(huma_tr_t32, human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__technocratic_vs_incarnational_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__technocratic_vs_incarnational_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(huma_be_t8, human_transcendence_pathway__technocratic_vs_incarnational_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(huma_be_t16, human_transcendence_pathway__technocratic_vs_incarnational_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(huma_be_t24, human_transcendence_pathway__technocratic_vs_incarnational_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(huma_be_t32, human_transcendence_pathway__technocratic_vs_incarnational_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__technocratic_vs_incarnational_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(huma_su_t8, human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(huma_su_t16, human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(huma_su_t24, human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(huma_su_t32, human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__technocratic_vs_incarnational_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__technocratic_vs_incarnational_reading, resource_allocation).
narrative_ontology:affects_constraint(human_transcendence_pathway__technocratic_vs_incarnational_reading, babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__technocratic_vs_incarnational_reading, jerusalem_reading).

% DUAL FORMULATION NOTE:
% Constraint family of the human_transcendence_pathway kernel: babel_reading (collective self-sufficiency), jerusalem_reading (communion through blessed labor), and this technocratic_vs_incarnational_reading (individual optimization versus gifted grace) instantiate different victim sets and different epsilon sources from one contested kernel. This story's epsilon is indexed to the technocratic arrangement read through Incarnational lights; each sibling indexes its own to its own standing arrangement. Edges run to both siblings because the readings compete for the same institutional territory — research-ethics governance, bioethics curricula, health-policy legitimacy — without any one logically ruling the others out for the parties that hold them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
