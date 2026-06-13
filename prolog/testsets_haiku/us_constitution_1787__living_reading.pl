% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitution Reading: Evolutionary Constitutional Meaning
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   The living Constitution reading holds that constitutional meaning evolves
 *   with society, allowing courts to recognize modern rights and adapt the
 *   Constitution's meaning to contemporary values without formal amendment.
 *   This constraint instantiates the LIVING READING of the contested 1787
 *   Constitution kernel—one of three major interpretive readings (living,
 *   originalist, positivist) that compete for authority over constitutional
 *   meaning. The living reading is the dominant reading in American federal
 *   courts from roughly 1965–2020, though originalism has gained
 *   institutional position afterward. This story models the living reading as
 *   a tangled rope: it genuinely solves a coordination problem (adapting
 *   constitutional law to social change without supermajority amendment
 *   requirements), but it asymmetrically extracts interpretive authority from
 *   the amendment process and from originalist/textualist traditions, and it
 *   persists through active enforcement (judicial gatekeeping of which claims
 *   count as 'evolved norms').
 *
 * KEY AGENTS:
 *   - Progressive judicial coalitions: set and enforce the living reading; interpret novel rights claims; control the agenda.
 *   - Rights claimants (modern era): benefit from interpretive authority that recognizes privacy, dignity, equality without amendment.
 *   - Originalist interpreters: structurally excluded from agenda-setting when living reading dominates; bear the cost of being ruled outside legitimate constitutional discourse.
 *   - Textual literalists: argue the reading violates the rule of law and the written Constitution's constraints.
 *   - States and jurisdictions: face unpredictability as 'evolved' constitutional meaning overrides their policies.
 *   - Constitutional scholars: develop and legitimize the framework; benefit from its authority and platform.
 *   - Originalist movement: observer position increasingly mobilized as a competing institutional force (post-2020s appointments).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.68).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.55).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitution Reading: Evolutionary Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '7d437acc-ce0b-407e-9aa4-88749d65df43').
narrative_ontology:cs_kernel_codification('7d437acc-ce0b-407e-9aa4-88749d65df43', fixed_text).
narrative_ontology:cs_authority_grounding('7d437acc-ce0b-407e-9aa4-88749d65df43', lineage).
narrative_ontology:cs_interpretation_layer_present('7d437acc-ce0b-407e-9aa4-88749d65df43').
narrative_ontology:cs_reading_relation('7d437acc-ce0b-407e-9aa4-88749d65df43', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d437acc-ce0b-407e-9aa4-88749d65df43', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('7d437acc-ce0b-407e-9aa4-88749d65df43', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('7d437acc-ce0b-407e-9aa4-88749d65df43', constitutional_meaning_evolves, deontological).
narrative_ontology:cs_axiom('7d437acc-ce0b-407e-9aa4-88749d65df43', foundational, aspirational_framework_doctrine).
narrative_ontology:cs_axiom_status(aspirational_framework_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7d437acc-ce0b-407e-9aa4-88749d65df43', aspirational_framework_doctrine, conventional).
narrative_ontology:cs_reference_frame('7d437acc-ce0b-407e-9aa4-88749d65df43', flexible_constitutional_adaptation).
narrative_ontology:cs_drift_state('7d437acc-ce0b-407e-9aa4-88749d65df43', originalist_institutional_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d437acc-ce0b-407e-9aa4-88749d65df43', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, progressive_judicial_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, rights_claimants_modern_era).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_interpreters).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, textual_literalists).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, jurisdictions_seeking_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, social_movements).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, constitutional_scholars).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, constitutional_fluidity_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, judicial_modernization_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution as responding to evolving societal norms and contemporary values. Adjudicates novel rights claims (privacy, dignity, equal protection for previously excluded groups) by reading them into the constitutional text through the living-reading frame. Controls the authority to declare what the Constitution 'now means' and frames that authority as fidelity to the Constitution's aspirational purpose. Benefits from interpretive flexibility and from the legitimacy that attaches to courts that are perceived as adapting law to modern conditions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, progressive_judicial_coalitions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, progressive_judicial_coalitions, beneficiary).

% Seek recognition of rights not explicitly enumerated in the 1787 text (reproductive autonomy, privacy, dignity, marriage equality, racial and gender equality beyond explicit text). The living reading provides the interpretive pathway to success: if meaning evolves, courts can recognize new rights as the Constitution's commitment to human dignity unfolds. Without this reading, their claims lack constitutional basis and must proceed through amendment (vastly harder) or legislative grace.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, rights_claimants_modern_era, beneficiary,
    organized, biographical, constrained, national).

% Argue the Constitution's meaning is fixed at ratification; judges who reinterpret the text to match modern values are usurping the amendment power and imposing their own policy preferences under the guise of constitutional law. They bear the cost of being structurally excluded from the agenda-setting authority when living-reading coalitions control the courts; their counter-interpretations are ruled outside legitimate constitutional discourse when the living reading dominates.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_interpreters, payer,
    institutional, generational, constrained, national).

% Insist constitutional authority derives only from the written text as adopted; judicial innovation beyond the text's plain meaning violates the rule of law and substitutes judges' intuitions about progress for the actual Constitution. They argue the living reading collapses predictability and constitutional constraint itself — the text means what it says, not what judges wish it to mean in the current era.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, textual_literalists, payer,
    powerful, generational, constrained, national).

% States and political subdivisions attempting to maintain policies grounded in traditional constitutional interpretation (e.g., restricting abortion, limiting commerce-clause reach, maintaining federalism boundaries) find their authority overridden when courts declare the Constitution now means something different. The living reading generates unpredictability in constitutional law — a state passes a policy believing it constitutional under settled law, and a later court declares the Constitution has evolved to forbid it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, jurisdictions_seeking_stability, payer,
    organized, generational, trapped, national).

% Mobilize constituencies around rights claims — LGBTQ+ equality, racial justice, gender equity, bodily autonomy — and seek judicial recognition through the living-reading frame. They provide the political movement that legitimates 'evolving norms' and generates the societal consensus the reading claims to reflect. Their successes in the courts validate the living reading and reinforce the interpretive authority of courts that adopt it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, social_movements, beneficiary,
    organized, biographical, mobile, national).

% Academic interpreters who develop and legitimize the living-reading framework. Shape judicial doctrine through law review articles, amicus briefs, and judicial opinions citing scholarly work. Their interpretive authority derives from articulating 'evolving norms' and showing how the Constitution accommodates modern demands. Benefit from the reading's legitimacy and from the platform it provides for legal philosophy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_scholars, beneficiary,
    analytical, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, constitutional_scholars, agenda_setter).

% The amendment process and ordinary legislation are structurally excluded from the agenda when courts claim interpretive authority to evolve constitutional meaning. A legislature seeking to establish a right through amendment faces an entrenched living-reading coalition claiming the Constitution already guarantees it; a legislature seeking to restrict a right finds courts declaring an unamendable constitutional principle. The living reading narrows the space for democratic constitutional change.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legislative_alternatives, excluded,
    institutional, generational, trapped, national).

% Organized intellectual and political movement opposing the living reading, developing originalist scholarship and seeking to place originalist judges on the bench. Monitors the constraint's application and produces counter-interpretations. Currently (post-2020s appointments) increasingly gains institutional position, creating live contestation over which reading controls judicial authority.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_movement, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for constitutional interpretation that allows courts to recognize rights not explicitly in the 1787 text, adapting the Constitution's meaning to accommodate modern understandings of human dignity, equality, and justice. Coordinates the legal system's response to social change without requiring the supermajority consensus for amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority from the amendment process (a high-friction democratic mechanism requiring broad consensus) to the courts (an institutional actor subject to organized political pressure and elite preferences). Moves the power to define what rights citizens have from the legislature and supermajority amendment power to the federal judiciary, and specifically to judicial coalitions aligned with progressive social movements.
% ABSENT_VOICES: Originalist and textual-literalist interpreters would argue the reading illegitimately usurps the amendment power and imposes judges' preferences as constitutional law. Legislative bodies seeking to maintain policies against court-mandated evolution would object to having their enactments overridden by 'evolved' constitutional meaning. Future generations with different social values would object to having today's 'evolving norms' enshrined as unamendable constitutional principle. Conservative jurisdictions and traditional legal communities, while present in the discourse, are systematically excluded from the agenda-setting authority when living-reading coalitions control the courts.
% DISAPPEARANCE_RATIONALE: If the living reading evaporated overnight and only originalist/textualist readings remained, constitutional law would contract dramatically: the judicially-recognized rights to privacy, marriage equality, reproductive autonomy, and expansive equal protection would lose their constitutional anchor and revert to legislative grace or state-level constitutional protection. The political economy of rights claims would reorganize around amendment processes and state constitutions rather than federal judicial reinterpretation. The courts' role in legitimizing social movements would shrink.
% FOUNDING_PROBLEM: The 1787 Constitution was written without explicit protection for rights that modern society recognizes as fundamental (privacy, dignity, equal protection for women and LGBTQ+ persons). The amendment process is too rigid to adapt to evolving understandings of constitutional values. Courts have authority to interpret the Constitution; that authority should extend to recognizing rights the framers did not foresee but that flow from the Constitution's core commitments to human dignity and equality.
% FOUNDING_PROBLEM_CORROBORATION: Progressive judges and constitutional scholars, civil-rights organizations, and social movements attested the founding problem as live throughout the 20th century, producing major decisions (Griswold, Loving, Roe, Obergefell) recognizing unenumerated rights. Originalist judges and scholars, conservative legal organizations, and textual-literalist traditions contest that the founding problem justifies the living reading, arguing the amendment process—not judicial reinterpretation—is the constitutional remedy. Legislative testimony from states and constitutional amendment advocates provides corroboration from outside the judicial beneficiaries: the amendment process IS too rigid for rights recognition at the speed courts have managed through living reading.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the reading transfers interpretive authority from the amendment process to courts, and courts aligned with progressive movements gain the power to declare what rights exist. The trajectory rises from 0.42 to 0.68 over the first 30 years (the period 1960s–1990s of major rights-recognition decisions) then plateaus, indicating the living reading consolidated its institutional position—extraction stabilized once the interpretive authority was settled. Suppression is moderate (0.55) because the reading is actively defended against originalist challenge through judicial gatekeeping, law review dominance, and legitimacy claims about 'evolving norms'—but suppression is lower than full snare because originalist voices remain intellectually live (resistance = 0.72). Theater is moderate (0.42) because the reading's claim to be discovering 'evolving norms' is partly genuine (courts do respond to social change) and partly performance (the norms are selected through a filter of judicial preferences and elite social movements, not discovered neutrally). The measurement series all share one time grid: every metric is authored at every examined point, preventing OQ-105-style misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting progressive coalitions experience the living reading as legitimate constitutional interpretation responding to social change—a genuine coordination benefit. The originalist/textualist payers experience it as usurpation of the amendment power and imposition of judicial policy preferences. The rights-claimant beneficiaries experience it as opening constitutional pathways to rights recognition. States and jurisdictions experience it as unpredictable constraint-shifting that overrides their policies. The engine should compute sharply divergent types from these different seats: the judicial coalitions' seat is low d (beneficiary, mobile exit, institutional power); the originalists' seat is high d (victim of exclusion, constrained by interpretive authority loss, institutional power); the rights claimants' seat is low d (beneficiary); the states' seat is moderate-to-high d (payer of unpredictability). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive judicial coalitions: d ≈ 0.15–0.25 (beneficiary: control the interpretive authority, set the agenda, gain legitimacy from rights-recognition decisions; mobile exit because their interpretation can adjust over time). Rights claimants: d ≈ 0.20–0.30 (beneficiary: pathways to rights recognition without amendment; constrained exit because they depend on courts; but organized power gives them some leverage). Originalist interpreters: d ≈ 0.70–0.80 (victim: structurally excluded when living reading dominates, their counter-interpretations ruled outside legitimate discourse; constrained exit because they remain bound to the constitution they interpret; institutional power moderates but doesn't eliminate the targeting). Textual literalists: d ≈ 0.65–0.75 (victim: same mechanism as originalists). States and jurisdictions: d ≈ 0.55–0.65 (payer: unpredictability in constitutional meaning creates policy instability; organized exit because some can seek amendment or federalism carve-outs, but trapped in federal constitutional system). Constitutional scholars: d ≈ 0.10–0.20 (beneficiary: platform and authority; arbitrage exit because scholars can shift positions).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the rigidity of the amendment process—genuine issue. But by the 2000s, the problem is partially obsolete: major rights have been recognized, and the amendment process, while slow, has not proven to be the binding constraint on rights evolution. The reading persists not because the amendment bottleneck still blocks necessary change, but because the judicial authority to reinterpret is now entrenched and beneficiary coalitions profit from it. A true mandatrophy reading would note: the living reading solves a real coordination problem (avoiding the amendment supermajority requirement) but the beneficiaries are now narrowly concentrated (progressive judges and aligned scholars/movements), and the costs are distributed (unpredictability for states, interpretive exclusion for originalists). This is not rope (broad-based coordination benefit). It is tangled rope verging on snare: genuine coordination function, but heavily asymmetric extraction and active enforcement against competing readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_norms_capture,
    'Who determines what counts as ''evolving norms'' that the Constitution now accommodates? Is this determination genuinely responsive to societal consensus, or filtered through elite judicial and scholarly preferences?',
    'Empirical analysis of the correlation between major living-reading decisions and measurable public opinion / social movement success. Discourse analysis of how ''evolving norms'' are justified in opinions (citation patterns, which voices are cited as corroboration). Comparison of rights recognized by courts vs. rights articulated by grassroots movements.',
    'If ''evolving norms'' are elite-filtered (judges and scholars selecting which movements'' claims count as constitutional evolution), the reading is substantially more extractive than the beneficiary narrative claims—it is using constitutional authority to amplify some voices while suppressing others. This would elevate extractiveness and suppress the coordination-function defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evolving_norms_capture, empirical, 'Whether evolving norms reflect genuine societal consensus or elite judicial selection.').

omega_variable(
    amendment_process_fungibility,
    'Is the amendment process genuinely so rigid that living reading is the only feasible mechanism for constitutional evolution? Or would focused reform of the amendment process (lower supermajority threshold, sunset clauses, state ratification alternatives) achieve the same rights recognition without judicial reinterpretation?',
    'Comparative constitutional law: examine whether other democracies with lower amendment thresholds show faster rights evolution and whether they rely less on judicial reinterpretation. Counterfactual: would lowering the amendment supermajority to 3/5 or 2/3 have produced the same rights outcomes without living reading?',
    'If amendment-process reform would produce similar rights evolution, the living reading''s justification (the amendment process is too rigid) is undermined. The reading would appear as a path-dependent institutional choice rather than a structural necessity. This would reframe the constraint as pure extraction (judicial authority aggrandizement) rather than necessary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_fungibility, empirical, 'Whether amendment-process reform could substitute for living reading''s function.').

omega_variable(
    originalist_reading_foreclosure,
    'Does the living reading''s core axiom (''constitutional_meaning_evolves'') logically foreclose the originalist reading''s axiom (''constitutional_meaning_fixed_at_ratification'') within a single committed framework, or do these represent genuinely coexisting positions held by different parties?',
    'Jurisprudential analysis: can a single judge, legal system, or interpreting community hold both ''meaning evolves'' and ''meaning is fixed'' without logical contradiction? Or are these mutually exclusive commitments?',
    'If they foreclose each other, the reading_relations in cs_structure should be ''forecloses'' rather than ''coexists_with''. This would model the kernel contest as zero-sum (one reading must lose) rather than as multiple live positions. Type: if foreclosure, the living reading is more extractive because it requires the complete displacement of originalism rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_reading_foreclosure, conceptual, 'Whether living and originalist readings can coexist or must foreclose.').

omega_variable(
    rights_boundary_expansion,
    'Does the living reading contain an internal principle limiting which claimed rights count as constitutional evolution, or does the framework admit indefinite expansion to any right that social movements claim?',
    'Jurisprudential analysis of living-reading opinions: is there a stated criterion (e.g. ''deeply rooted in tradition'', ''essential to ordered liberty'', ''dignity'') that constrains which rights are recognized? How often are novel claimed rights rejected as inconsistent with constitutional evolution? If rejected, what is the justification?',
    'If no principled boundary exists, the reading is vulnerable to indefinite elite capture (any interest that mobilizes courts can claim constitutional status). This would elevate extractiveness and theater_ratio. If a boundary exists but is unevenly applied, this is evidence of elite capture. The reading would shift from tangled_rope (genuine coordination + asymmetric extraction) toward snare (mostly elite authority capture dressed in constitutional language).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rights_boundary_expansion, empirical, 'Whether the living reading has principled boundaries on which rights qualify as constitutional evolution.').

omega_variable(
    interpretation_layer_stability,
    'The living reading operates through an interpretation layer (federal courts, constitutional scholars, law review discourse) below the kernel (the 1787 text). How stable is this interpretation layer? If the institutional composition changes (e.g., originalist judges gain majority), does the reading collapse or survive?',
    'Observe the reading''s persistence as institutional composition shifts. Analyze whether scholars and judges adjust their interpretation-of-evolution claims in response to originalist pressure, or whether they maintain a fixed understanding of constitutional evolution regardless of court composition.',
    'If the reading is fully dependent on a particular judicial coalition''s dominance, interpretation_layer_present should be false—there is no stable interpretive layer, just a coalition controlling the courts. If the reading survives institutional change by adjusting its framing of ''evolved norms'', the interpretation layer is real but may be performing rather than constraining. This affects whether the reading is sustainable as a coordinate frame or is merely an artifact of a particular period''s institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_layer_stability, empirical, 'Whether the interpretation layer is stable across institutional-composition changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__living_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_1787__living_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t5, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__living_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_1787__living_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__living_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_1787__living_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t25, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__living_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__living_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__living_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t5, us_constitution_1787__living_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(us_c_be_t5, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__living_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_1787__living_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__living_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t25, us_constitution_1787__living_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(us_c_be_t25, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__living_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__living_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(us_c_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__living_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t5, us_constitution_1787__living_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(us_c_su_t5, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__living_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_1787__living_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__living_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t25, us_constitution_1787__living_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(us_c_su_t25, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__living_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__living_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(us_c_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, american_abortion_access_constraint).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, lgbtq_legal_equality_constraint).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, privacy_tort_constraint).

% DUAL FORMULATION NOTE:
% The 1787 Constitution kernel is contested across three major readings: living (this story, meaning evolves), originalist (meaning fixed at ratification), and positivist (meaning is text + formal amendment). Each reading is a separate constraint with its own ε, beneficiary/victim structure, and classification. They are linked via network.affects_constraints because the upstream reading (originalist, more established epistemic certainty) influences downstream readings (living, more contestable). The family structure models how competing readings of a single kernel generate different constraints. Any change in the authoritative reading cascades to affect the others' operative meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__living_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
