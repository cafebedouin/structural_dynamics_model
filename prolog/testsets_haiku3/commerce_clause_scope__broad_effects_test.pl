% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test (Federal Regulatory Scope)
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The Commerce Clause broad effects test is one reading of a contested
 *   constitutional kernel. This reading interprets the constitutional text
 *   'Congress shall have power to regulate commerce among the several states'
 *   as extending to any intrastate economic activity that substantially
 *   affects interstate commerce in the aggregate. Under this interpretation,
 *   the federal regulatory domain encompasses virtually all economic activity
 *   once aggregated national effects are demonstrated. This reading benefits
 *   federal regulatory agencies, national interest coalitions, and civil
 *   rights enforcement constituencies by enabling uniform national policies
 *   across domains from labor rights to environmental protection. It extracts
 *   from state legislative autonomy and local economic experimentation by
 *   subordinating state police powers to federal preemption whenever federal
 *   agencies can establish aggregate economic effects. This constraint story
 *   instantiates THIS READING ONLY — it does not adjudicate between the broad
 *   effects test, the narrow originalist reading, or the
 *   intermediate-channels reading; it assesses the structural impact of the
 *   broad effects reading on federalism as a constraint on federal power.
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: institutional beneficiary (expanded jurisdiction, uniform national authority)
 *   - states_as_legislatures: victim (constrained autonomy, preempted police powers, self-censorship)
 *   - national_advocacy_coalitions: beneficiary (uniform policy across jurisdictions enables national coordination)
 *   - local_economic_actors: victim (subject to federal regulation via aggregation even when activity is local)
 *   - judiciary_federal_appellate: agenda_setter (interprets the kernel text; current doctrine favors the broad reading)
 *   - originalist_scholarship_community: excluded (articulates the narrow reading but has no institutional authority to enforce it)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.78).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.64).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.78).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test (Federal Regulatory Scope)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'b187caf1-8349-4f9a-ba78-bdd61661dbf3').
narrative_ontology:cs_kernel_codification('b187caf1-8349-4f9a-ba78-bdd61661dbf3', fixed_text).
narrative_ontology:cs_authority_grounding('b187caf1-8349-4f9a-ba78-bdd61661dbf3', lineage).
narrative_ontology:cs_interpretation_layer_present('b187caf1-8349-4f9a-ba78-bdd61661dbf3').
narrative_ontology:cs_reading_relation('b187caf1-8349-4f9a-ba78-bdd61661dbf3', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('b187caf1-8349-4f9a-ba78-bdd61661dbf3', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('b187caf1-8349-4f9a-ba78-bdd61661dbf3', foundational, aggregation_doctrine_intrastate_reach).
narrative_ontology:cs_axiom_status(aggregation_doctrine_intrastate_reach, holdable).
narrative_ontology:cs_axiom_grounding('b187caf1-8349-4f9a-ba78-bdd61661dbf3', aggregation_doctrine_intrastate_reach, empirically_contingent).
narrative_ontology:cs_axiom('b187caf1-8349-4f9a-ba78-bdd61661dbf3', secondary, federal_police_power_via_commerce).
narrative_ontology:cs_axiom_status(federal_police_power_via_commerce, holdable).
narrative_ontology:cs_axiom_grounding('b187caf1-8349-4f9a-ba78-bdd61661dbf3', federal_police_power_via_commerce, instrumental).
narrative_ontology:cs_reference_frame('b187caf1-8349-4f9a-ba78-bdd61661dbf3', enumerated_federal_powers_doctrine).
narrative_ontology:cs_drift_state('b187caf1-8349-4f9a-ba78-bdd61661dbf3', contemporary_administrative_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b187caf1-8349-4f9a-ba78-bdd61661dbf3', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_advocacy_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_coalitions).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_legislative_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, non_interstate_economic_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, state_executive_agencies).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_executive_agencies).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal agencies (EPA, NLRB, OSHA, HHS, DOJ Civil Rights Division) gain expanded jurisdiction over economic activities previously thought to be intrastate and non-regulable. The broad effects test allows them to assert authority over any activity with demonstrated or claimed national economic aggregate effects. They set the agenda through rulemaking, enforcement, and interpretation of the doctrine. Their mandate expands with the doctrine, and they collect the institutional power and budgetary expansion that comes with jurisdiction growth.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary,
    institutional, generational, arbitrage, global).

% Labor unions, environmental organizations, civil rights coalitions, and consumer advocacy groups benefit from the broad effects test because it enables federal-level uniform policy they cannot achieve through state-by-state negotiation. They lobby federal agencies and support litigation that extends the doctrine. They do not set the agenda (federal agencies do) but they are aligned beneficiaries.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_advocacy_groups, beneficiary,
    organized, generational, arbitrage, global).

% Civil rights enforcement depends on federal authority to override state and local discrimination. The broad effects test (applied in Heart of Atlanta, Katzenbach v. McClung) enables federal reach into local economic activity (hotels, restaurants) to enforce civil rights. Coalitions seeking national non-discrimination norms benefit from this federal power.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_coalitions, beneficiary,
    organized, generational, arbitrage, global).

% State legislatures experience the broad effects test as loss of autonomy over intrastate economic regulation. They cannot credibly maintain that local economic activities fall outside federal jurisdiction because the doctrine permits federal courts to aggregate national effects and assert authority. They bear the cost of preemption, regulatory duplication where federal and state rules clash, and the frustration of their own police powers. Their exit options are: (1) anticipate federal preemption and regulate only in complementary domains (constrained by federal regulatory landscape), (2) litigate to narrow the doctrine (difficult, requires Supreme Court willingness to overturn precedent), (3) opt for federal-floor regulation while state-level experimentation remains legally risky. States cannot exit the federal system; the constraint is binding.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_legislatures, payer,
    organized, generational, constrained, national).

% State environmental, labor, and health agencies experience dual positioning: they must enforce federal regulations under preemption (cost of compliance, loss of independent authority) but also benefit where federal regulation sets a floor that prevents races-to-the-bottom. States like California that want stronger environmental rules can use federal authority as a political tool, but states that want looser rules experience it as a constraint. On balance, state executive agencies are partially captured by federal regulatory frameworks and experience loss of policy autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_executive_agencies, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, state_executive_agencies, beneficiary).

% Small manufacturers, local agricultural operations, family businesses, and local service providers experience the broad effects test as subjection to federal regulation even when their activity is wholly local. Once federal courts adopt the broad effects test, these actors cannot argue their activity is beyond federal reach — the doctrine permits federal regulators to assert jurisdiction via aggregation. Their exit options are: (1) comply with federal regulation in addition to any state/local rules, (2) exit the regulated sector, (3) litigate to challenge federal jurisdiction (expensive, low success rate under current doctrine). They are trapped within the federal regulatory system.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_actors, payer,
    powerless, biographical, trapped, local).

% Federalism advocates (originalist scholars, conservative judges, states' rights organizations) oppose the broad effects test as violating the constitutional division of powers. They seek to narrow the doctrine through litigation (Lopez, Morrison, NFIB v. Sebelius) and scholarship. They bear the cost of institutional subordination: their preferred reading of the Commerce Clause is not the operative doctrine, and they must litigate uphill against precedent and institutional inertia. Their exit is constrained by judicial supremacy and the difficulty of constitutional amendment.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federalism_advocates, payer,
    moderate, generational, constrained, national).

% Federal appellate courts (especially the Supreme Court) established and maintain the broad effects doctrine through interpretation of the Commerce Clause. They set the agenda by deciding which constitutional readings are legitimate. Current doctrine (post-1942 Wickard through current period) favors the broad effects test, though some justices (Thomas, Gorsuch in Lopez dissent) advocate for the narrower originalist reading. The judiciary's agenda-setting power is substantial but not absolute — it is constrained by precedent, constitutional amendment possibility, and political pressure.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_judiciary_appellate, agenda_setter,
    institutional, civilizational, analytical, national).

% Originalist judges dissent from the broad effects test and articulate alternative readings of the Commerce Clause. They occupy a minority position on the Supreme Court and in the federal judiciary. They have institutional authority to write dissenting opinions but not to set the operative doctrine. They observe the constraint from an analytical position outside the dominant institutional framework.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, originalist_judges, observer,
    institutional, generational, analytical, national).

% Legal scholars articulating originalist and federalism-constraining readings of the Commerce Clause are excluded from the institutional framework that applies the doctrine. They write about the narrow originalist reading and the limitations of the broad effects test, but federal courts do not adopt their arguments as operative doctrine. They would reshape the constraint if they had institutional authority, but they operate outside the judicial system.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, originalist_legal_scholarship, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The broad effects test solves the coordination problem of regulating interstate commerce: without federal power to reach intrastate activity that aggregates to national economic effects, states could erect barriers to interstate commerce, enact discriminatory local rules, or create races-to-the-bottom in labor standards, environmental protection, and civil rights. The doctrine enables uniform national regulation across domains where local fragmentation creates inefficiency or harm. It also enables federal enforcement of national civil rights norms (anti-discrimination, labor rights) that cut across state lines.
% TRANSFER_FUNCTION: The doctrine transfers regulatory authority from state legislatures and local jurisdictions to the federal government (agencies, Congress, courts). It moves the power to decide what intrastate economic activities can be regulated, setting rules that bind all states. It transfers the power to enforce uniform national norms across jurisdictions. In civil rights domains, it transfers power from state authorities that might tolerate discrimination to federal authorities mandated to enforce non-discrimination.
% ABSENT_VOICES: State legislatures and local governments are structured to be victims rather than participants in the doctrine's development. Their voices appear in litigation (state attorneys general defending state regulations) but they cannot set the agenda. Originalist and federalism advocates are excluded from institutional authority to enforce their reading of the Commerce Clause, though they publish critiques. States that would prefer looser regulation (and would be regulated more stringently) have no structural voice in the judiciary's interpretation of the doctrine.
% DISAPPEARANCE_RATIONALE: If the broad effects test disappeared overnight (replaced by the narrow originalist reading or a genuine limiting principle), the regulatory landscape would fundamentally rearrange. Federal agencies would lose jurisdiction over activities lacking a clear channel to interstate commerce. Environmental, labor, and civil rights enforcement would revert to state control, creating regulatory fragmentation and races-to-the-bottom in protective standards. States would regain autonomy to experiment with local economic rules. National interest coalitions would need to negotiate state-by-state instead of lobbying federal agencies. The world would not revert to an alternative stable equilibrium — it would face a period of regulatory uncertainty and the need to rebuild federal/state cooperation structures around explicit interstate commerce channels rather than aggregation doctrine.
% FOUNDING_PROBLEM: The founding problem was the Articles of Confederation: states erected tariff barriers against each other's commerce, engaging in mutual economic warfare. The Commerce Clause was designed to give Congress power to prevent state barriers to interstate trade and to establish uniform national commercial rules. Early applications (Gibbons v. Ogden, 1824) enforced this mandate by striking down state barriers to interstate navigation and commerce.
% FOUNDING_PROBLEM_CORROBORATION: Federal regulatory agencies, national interest coalitions, and federal courts attest that the founding problem remains live: states could still erect local barriers and fragment national commerce if federal authority did not extend to aggregate intrastate effects. Federalism advocates and originalist scholars (sources outside the benefiting parties) attest that the founding problem is substantially solved: explicit state tariff barriers are constitutionally prohibited under dormant Commerce Clause doctrine, and federal power to regulate interstate commerce channels is uncontested. They argue the broad effects test exceeds the founding mandate by reaching wholly intrastate activity unrelated to interstate commerce. Neutral analysts (constitutional historians, legal scholars) confirm that the founding problem concerned state barriers to interstate trade, not the regulation of intrastate activity, and note that the test has substantially expanded beyond the founding rationale.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the broad effects test grants federal regulatory power over activities that would otherwise be reserved to the states under the Tenth Amendment. The extraction is real: the test extracts state sovereignty by subsuming intrastate activity into federal jurisdiction whenever cumulative economic effects can be demonstrated. The measurement series traces extraction accumulation from Wickard (agricultural production, 1942, nominally intrastate but regulated as 'substantially affecting' interstate commerce) through Gonzales v. Raich (medical marijuana, 2005, purely intrastate activity regulated under aggregation). Suppression is moderate-to-high (0.64) because the doctrine actively constrains state experimentation: states cannot credibly maintain that an economic activity is insulated from federal regulation if federal regulators can construct an aggregation argument. Theater ratio rises over the interval (0.18 → 0.41) because the interpretive machinery of the test becomes increasingly detached from its coordination function: early applications of 'substantially affects' involved genuine interstate commerce channels (Wickard — wheat could cross state lines; Heart of Atlanta — racial discrimination in interstate commerce). Later applications extend to purely local activity via aggregation (Gonzales — individual intrastate cannabis cultivation has no channel to interstate commerce; the federal claim rests entirely on aggregated national demand). Theater rises as the test's coordination rationale (unifying interstate commerce rules) is succeeded by its extractive function (expanding federal jurisdiction). Accessibility collapse is high (0.72) because once federal courts adopt the broad effects test as the operative doctrine, states face near-total foreclosure of autonomy: challenging federal regulation requires arguing the regulated activity has NO national economic effects — a nearly impossible burden after decades of administrative agency fact-finding. Resistance is moderate (0.58) because state governments and federalism advocates actively resist the doctrine, but resistance is constrained by judicial supremacy and the difficulty of amending the Constitution.
 *
 * PERSPECTIVAL GAP:
 *   From the federal institutional seat (agencies, courts), the broad effects test solves a genuine coordination problem: without it, interstate commerce regulation fractures across state lines, creating races-to-the-bottom for labor standards, environmental rules, and civil rights protections. National interest coalitions (labor unions, civil rights groups, environmental organizations) perceive the test as enabling nationwide coordination on issues that individual state action cannot solve. From the state legislative seat, the same test operates as a mechanism of federal extraction: it removes from state hands the power to experiment with local economic regulation, even when the activity is genuinely local and the claimed interstate effects are attenuated. From the local economic actor's seat (a small manufacturer, a local agricultural operation), the constraint appears as pure extraction with performative justification: the federal government claims its jurisdiction over local activity rests on aggregated national effects, but the local actor experiences it as unilateral federal override of local regulatory choice. The engine computes these seat-specific divergences from the structural data (beneficiary/victim declarations, power atoms, exit options) — the authored claim (tangled_rope: genuine coordination + asymmetric extraction) reflects this perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies are structural beneficiaries (d → 0.0): they collect expanded jurisdiction and power to regulate without the political friction of state-by-state negotiation. Their exit from this arrangement is not available without constitutional amendment. States as collective legislative actors are structural targets (d → 1.0): they bear the cost of subordinated autonomy and preempted police powers. Individually mobile states might escape via internal regulatory choice (opt for federal compliance, preempt the field locally), but this is false exit — it still leaves them subject to federal jurisdiction. The federal judiciary (especially the Supreme Court) is the agenda-setter (d → 0.5 to favorable to beneficiary): it established and maintains the broad effects doctrine through interpretation. National advocacy coalitions are secondary beneficiaries (d → 0.1 to 0.2): they benefit from uniform national policy but do not set the agenda; their power is dependent on federal regulatory institutions. Local economic actors are secondary targets (d → 0.8): they experience regulation but do not set policy; they lack the state-level political power to resist. The originalist scholarly community is excluded (role=observer): they articulate alternative readings but have no institutional authority to enforce them. The analytical measure reflects this structure: the federal seat experiences the constraint as coordination (unifying national commerce rules, enabling civil rights enforcement nationwide) while the state seat experiences it as extraction (subjection to federal override whenever aggregation can be demonstrated).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — a constraint whose founding mandate has outlived its function — is plausible here but contested. The founding mandate of the Commerce Clause power is to prevent states from erecting barriers to interstate commerce and to enable uniform national commercial rules. Under the broad effects test, the doctrine now extends far beyond this original mandate: it reaches wholly intrastate, non-commercial activity (Gonzales — marijuana cultivation for personal medical use) via aggregation logic. The test persists because it serves new mandates unrelated to interstate commerce coordination: federal civil rights enforcement, labor regulation, environmental protection. The classification as Tangled Rope rather than Piton reflects that the constraint retains a real coordination function (national commerce does benefit from uniform federal rules, civil rights enforcement does benefit from federal authority) alongside its extractive operation. If the founding mandate were completely dead (if no one credibly argued the test coordinates interstate commerce anymore, only that it enables federal power expansion), the type would shift to Piton. The measurement series shows the theater_ratio rising, which is a Piton symptom, but the underlying extractiveness also rises — in Piton, extractiveness typically plateaus or declines as the constraint becomes pure performance. Here, extractiveness rises because the test is BOTH coordinating (new mandates) and extracting (federal power growth). This places it firmly in Tangled Rope territory: both functions active, both measurable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_doctrine_empirical_threshold,
    'At what cumulative national economic impact does intrastate activity cross the ''substantially affects'' threshold? Is there a principled, measurable boundary, or is the standard inherently indeterminate?',
    'Comparative constitutional law analysis: jurisdictions with explicit economic-impact thresholds (EU subsidiarity tests, Swiss canton-federal balancing) versus the doctrinal record of U.S. cases (Wickard, Gonzales, Morrison) to assess whether a stable, predictable threshold exists across domains.',
    'If a threshold is identifiable and consistently applied, the broad effects test becomes constraining on federal power (narrower extractiveness, higher accessibility to state challenge). If the threshold is fundamentally indeterminate (case-by-case, result-oriented), federal power is effectively unbounded by the Commerce Clause text (higher extractiveness, victim set is nearly all intrastate activity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_empirical_threshold, empirical, 'Whether the ''substantially affects'' standard is determinate or result-oriented.').

omega_variable(
    reading_kernel_decomposition,
    'Is this constraint one reading of the Commerce Clause kernel, or is it a substantive constitutional claim about federal power that stands independent of textual interpretation?',
    'This omega documents the committer-frame structure: the broad effects test is INSTANTIATED as a reading of the fixed-text kernel ''Commerce among the several States'' — the reading interprets ''substantially affects interstate commerce'' as embracing aggregated intrastate effects. Sibling readings (narrow_originalist, intermediate_channels) interpret the same kernel text differently. The three readings are not three separate constitutional claims about federal power; they are three competing interpretations of ONE constitutional text (the kernel).',
    'The constraint is a kernel reading, not a free-standing doctrine. Its classification is assessment of THIS READING''s structural impact on state sovereignty, not a general assessment of federal commerce power. Alternative readings of the same kernel would have different extractiveness, different victim sets, and different classifications. The engine computes per-seat type differences from the structural asymmetries; authority-grounding differences between readings are recorded in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_decomposition, conceptual, 'Constraint identity as a kernel reading versus a free-standing doctrine.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of state autonomy structural (legal doctrine bars state action; federal preemption removes de facto state options) or internalized (states anticipate federal override and self-censor, even where federal action is uncertain)?',
    'Post-doctrinal-shift measurement: if federal appellate courts explicitly narrow the aggregation doctrine (e.g., via limiting Lopez/Morrison principles or textualist reinterpretation), do states significantly expand their legislative agendas in previously self-censored domains? Or does suppression persist due to institutional path-dependence and risk-averse state administrative cultures?',
    'If suppression is primarily structural (legal barriers), remedying the doctrine''s text resolves it. If suppression is primarily internalized (states have internalized the federal power model as legitimate constraint on their authority), the actual suppression persists after doctrinal change — the victim set experiences the constraint as internalized norm rather than external legal rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression of state legislative autonomy.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the broad effects test FORECLOSE the narrow originalist reading, or do the two coexist as competing interpretive frameworks held by different judicial factions?',
    'Appellate jurisprudence and circuit splits: if courts systematically reject originalist Commerce Clause arguments on the merits (rather than on procedural standing/justiciability grounds), and originalist scholarship is engaged as a legitimate alternative framework (even if rejected), the readings coexist. If originalist arguments are treated as foreclosed by settled precedent and not entertained on the merits, the broad effects test has displaced the originalist reading within the authoritative institutional framework (the judiciary).',
    'Coexistence implies both readings remain active in different seats (judiciary vs. legal academy, federal vs. state) — the kernel has multiple simultaneous readings and neither is dominant everywhere. Foreclosure implies the broad effects test has become the institutional authority''s canonical reading, and the narrow originalist reading survives only outside the institutional framework. Foreclosure would be reflected in the engine''s computation as axis dominance (federal institutional seat, high power, subsumes lower-power originalist seats).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, empirical, 'Logical relationship between broad effects and narrow originalist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__broad_effects_test, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__broad_effects_test, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__broad_effects_test, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(comm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__broad_effects_test, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__broad_effects_test, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__broad_effects_test, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(comm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__broad_effects_test, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__broad_effects_test, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__broad_effects_test, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(comm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, resource_allocation).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__broad_effects_test, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, interstate_commerce_channels_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, state_police_power_preemption).

% DUAL FORMULATION NOTE:
% This constraint is one reading (broad effects test) of the commerce_clause_scope kernel. The narrow_originalist and intermediate_channels constraints are sibling readings of the same kernel, each with its own ε, beneficiary/victim structure, and classification. All three readings are linked via network.affects_constraints to document the constraint family. The broad effects test is the upstream reading (institutional authority — federal courts adopt it as the operative doctrine); it influences the intermediate_channels reading (creates structural pressure to adopt limiting principles) and forecloses the narrow originalist reading (at the institutional appellate level, originalist arguments are not entertained on the merits). In the scholarly/political sphere, all three remain live.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
