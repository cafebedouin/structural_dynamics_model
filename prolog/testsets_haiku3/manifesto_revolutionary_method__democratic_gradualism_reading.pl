% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Electoral Path to Socialism via Institutional Gradation
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   The democratic gradualist reading of the Marxist project claims that
 *   socialism is achievable through electoral majorities within existing
 *   liberal democratic institutions, without seizure of state power or
 *   violent rupture. This constraint governs how working-class political
 *   effort is organized, strategically framed, and disciplined. It benefits
 *   social democratic parties, labor union bureaucracies, and
 *   parliamentary-left intellectuals by positioning them as the authorized
 *   representatives and strategic directors of working-class interests. It
 *   imposes costs on revolutionary militant cadres, council communists, and
 *   direct-action organizers by suppressing their approaches as adventurist,
 *   sectarian, or counterproductive. The constraint is NOT a rope — it
 *   carries substantial asymmetric extraction (those suppressed bear costs
 *   without compensation) and requires active enforcement (expulsions,
 *   deplatforming, legal harassment of militants). It is a tangled rope
 *   because it DOES solve a real coordination problem (channeling diffuse
 *   working-class energies into a shared strategic framework) while ALSO
 *   extracting disproportionately from those whose alternative organizational
 *   forms are suppressed. The claim/metric independence is deliberate: social
 *   democratic parties themselves CLAIM this is a rope (authentic
 *   coordination around shared liberation goals); the authored metrics
 *   describe higher extraction and suppression. The engine computes that
 *   divergence — that is the measurement the framework exists to take.
 *
 * KEY AGENTS:
 *   - Social democratic parties: institutional agenda-setters, high power, high exit arbitrage (can switch to centrist positions), geographically national scope
 *   - Labor union leadership: organized beneficiaries with constrained exit, biographical horizon, material reliance on state legal frameworks
 *   - Revolutionary militant cadres: powerless targets, identity-locked (cannot exit without dissolving their political identity), biographical horizon, subject to active suppression
 *   - Council communist movements: powerless targets, local scope, identity-locked, excluded from the parliamentary-left coalition
 *   - Parliamentary-left intellectuals: moderate-power beneficiaries with mobile exit, provide legitimating theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Electoral Path to Socialism via Institutional Gradation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'cc655d43-0b07-499e-a8e4-f294b65c17f9').
narrative_ontology:cs_kernel_codification('cc655d43-0b07-499e-a8e4-f294b65c17f9', distributed).
narrative_ontology:cs_authority_grounding('cc655d43-0b07-499e-a8e4-f294b65c17f9', distributed).
narrative_ontology:cs_reading_relation('cc655d43-0b07-499e-a8e4-f294b65c17f9', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc655d43-0b07-499e-a8e4-f294b65c17f9', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('cc655d43-0b07-499e-a8e4-f294b65c17f9', foundational, capitalist_institutional_cooptability).
narrative_ontology:cs_axiom_status(capitalist_institutional_cooptability, holdable).
narrative_ontology:cs_axiom_grounding('cc655d43-0b07-499e-a8e4-f294b65c17f9', capitalist_institutional_cooptability, empirically_contingent).
narrative_ontology:cs_axiom('cc655d43-0b07-499e-a8e4-f294b65c17f9', foundational, electoral_majority_state_power_sufficiency).
narrative_ontology:cs_axiom_status(electoral_majority_state_power_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('cc655d43-0b07-499e-a8e4-f294b65c17f9', electoral_majority_state_power_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('cc655d43-0b07-499e-a8e4-f294b65c17f9', foundational, revolutionary_seizure_dictatorship_inevitability).
narrative_ontology:cs_axiom_status(revolutionary_seizure_dictatorship_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('cc655d43-0b07-499e-a8e4-f294b65c17f9', revolutionary_seizure_dictatorship_inevitability, empirically_contingent).
narrative_ontology:cs_reference_frame('cc655d43-0b07-499e-a8e4-f294b65c17f9', liberal_democratic_institutions_reformable_socialist).
narrative_ontology:cs_drift_state('cc655d43-0b07-499e-a8e4-f294b65c17f9', neoliberal_erosion_and_capitalist_structural_adaptation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cc655d43-0b07-499e-a8e4-f294b65c17f9', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, labor_unions_bureaucratic).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_left_intellectuals).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militant_cadres).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_movements).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, direct_action_organizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, labor_unions_bureaucratic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and maintains the strategic framework that socialism is achievable through electoral competition within existing liberal democratic institutions. Controls the narrative of reform trajectory, defines what counts as legitimate working-class organization, and channels class struggle into parliamentary and union processes. Benefits by sustaining institutional positions, funding, and electoral leverage without rupture. The constraint's persistence depends on their capacity to suppress alternatives and claim the exclusive representation of working-class interests.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from integration into state-mediated collective bargaining, legal recognition, and partnership with social democratic parties. Constrained by reliance on institutional channels; cannot exit without losing negotiating legitimacy. Gradually materially compromise through acceptance of productivity increases, wage restraint justified by electoral cycles, and suppression of shop-floor militancy. Their structural role is to translate working-class discontent into managed demand for reforms that preserve capitalist property relations.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, labor_unions_bureaucratic, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, labor_unions_bureaucratic, payer).

% Scholars, journalists, and public figures who articulate the democratic gradualist reading to educated audiences. Benefit from platform, institutional affiliation, publishing access, and the social prestige of being taken seriously by state and parliamentary institutions. Their work legitimizes the constraint by providing theoretical justification for electoral gradualism and critiques of 'utopianism' in direct revolutionary approaches.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_left_intellectuals, beneficiary,
    moderate, biographical, mobile, national).

% Organized networks committed to rapid transformation and worker seizure of state power. Systematically suppressed through legal harassment, party expulsion, deplatforming, and characterization as 'adventurist' or 'dogmatist' by social democratic gatekeepers. Exit from the working-class movement means exit from their identity and worldview; they cannot accept the gradualist frame without abandoning their analysis of capitalism's structural resistance to peaceful transformation. The constraint's enforcement actively costs them resources, safety, and hearing.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militant_cadres, payer,
    powerless, biographical, identity_locked, national).

% Organizers and theorists advocating direct democratic councils (factory committees, neighborhood assemblies) as the authentic form of working-class power, with no need for state mediation or party vanguard. Structurally excluded from coalition-building within the parliamentary-left framework; their alternative organizational forms are dismissed as naive or federalist deviationism. Cannot exit without renouncing their understanding of workers' self-management.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_movements, payer,
    powerless, biographical, identity_locked, local).

% Shop stewards, wildcat strikers, and localized organizers who attempt to build power outside party/union channels or in advance of parliamentary strategy. Constrained by legal liability, workplace retaliation, and union apparatus suppression of their initiatives. The constraint frames their efforts as reckless, sectarian, or counterproductive to the 'real' work of building electoral majorities.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, direct_action_organizers, payer,
    powerless, biographical, constrained, local).

% Witness the constraint from their position as those whose property and power is ostensibly targeted for transformation. From their seat, the constraint operates as a reassurance mechanism: it promises transformation through legal channels that preserve property relations, rule of law, and ultimately capitalist production. They remain effectively outside the constraint's direct governance but benefit from its operation as a stabilization mechanism.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_class_representatives, observer,
    powerful, generational, analytical, national).

% The state administrative structure and constitutional framework within which the constraint operates. The democratic gradualist reading requires continuous negotiation with state institutional logic and legal frameworks. State enforcement of property law, contract law, and restrictions on revolutionary organizing are structural conditions the constraint depends on; they are not authored as beneficiaries but their institutional continuity is presupposed.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state_apparatus, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the working-class political effort into a single coherent electoral and institutional strategy rather than multiple competing forms of class organization (revolutionary parties, councils, syndicalist cells, spontaneous uprisings). Coordinates labor unions, socialist political parties, and sympathetic intellectuals into a shared temporal horizon (the next election cycle) and shared conception of legitimate action (parliamentary negotiation, strikes authorized by union leadership, petitions, peaceful assembly).
% TRANSFER_FUNCTION: Redirects working-class energies, organizational capacity, and strategic attention from direct expropriation and seizure of production to electoral campaigns, union bureaucratic offices, and institutional reform negotiation. Transfers control over revolutionary narrative and strategic direction from grassroots militant cadres to party intellectuals and union officials with state-institutional access. The material transfer is diffuse: working-class members pay through acceptance of slower transformation pace, suppression of their own tactical innovations, and subordination of their autonomous organizational forms to party/union discipline.
% ABSENT_VOICES: Revolutionary cadres and council communists are structurally excluded from the consensus-building process: they are characterized as outside the legitimate left rather than as contesting voices within it. Unorganized workers, precariat, women in reproductive labor, and colonized peoples whose liberation cannot fit into national electoral cycles are also structurally absent from the democratic gradualist framework, which assumes a nationally-bounded working class and linear progress through state institutional reform.
% DISAPPEARANCE_RATIONALE: Social democratic parties and unions claim its disappearance would mean loss of hard-won working-class institutional power and reversion to capitalist domination. Revolutionary cadres argue that if the gradualist frame were abandoned, working-class autonomous organization and rapid expropriation could proceed. The parties dispute whether the constraint is a necessary stabilization mechanism or an obstacle to liberation.
% FOUNDING_PROBLEM: The founding problem, from the gradualist reading's own perspective: how to avoid both capitalist restoration (as happened in 1848, 1871) and authoritarian party dictatorship (as emerged in 1917-1921) while achieving socialized production. The answer: gradual institutional reform through legal channels, with working-class majorities voting for socialist parties and co-opting capitalist institutions for socialist purposes.
% FOUNDING_PROBLEM_CORROBORATION: Social democratic parties and labor union leadership attest the founding problem is live: authoritarian revolutions do lead to party dictatorship and capitalist restoration remains a risk. Revolutionary theorists and historians (Eric Hobsbawm, Arno Mayer, Perry Anderson) attest the founding problem is a false framing: it presupposes that capitalist classes will accept electoral defeat and that institutions built for bourgeois rule can be 'co-opted' rather than needing destruction and replacement. Academic economic historians document the failure of social-democratic transformation paths (20th century Western European experience of stalled or reversed nationalization, 2015 Greek capital controls circumvention of democratic mandate). Anticapitalist movements worldwide attest the founding problem is still unresolved: no path to socialism has succeeded without either authoritarian dictatorship (USSR, China, Vietnam) or capitalist integration (Sweden, Germany post-1989). The corroboration split follows factional lines — no external arbiter has settled it.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, contested).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.40) reflects the reading's claim to be moderate-extraction: it genuinely coordinates working-class organizational effort into coherent strategy, which has non-trivial cost (some inefficiency from centralization, some delay from electoral cycle timing). However, the asymmetry is real: militants pay costs (suppression, organizational exclusion) while receiving no countervailing benefit — they remain committed to working-class liberation but are prevented from pursuing their understanding of the fastest path. The suppression score (0.45) is moderate rather than high because suppression operates primarily through institutional exclusion and characterological attacks ('adventurist', 'sectarian') rather than direct state violence in most periods and places (though police/state violence against militants does occur). The theater ratio (0.42) is moderate-high: a substantial portion of the constraint's operation is devoted to defending the gradualist frame itself (theory production, electoral posturing, union bureaucratic performance) rather than actual transformation toward socialism. The measurement series shows extractiveness rising from 0.25 to 0.40 over 50 time periods, with suppression rising from 0.30 to 0.45, indicating that as the constraint aged and capitalist societies adapted (neoliberalism, post-industrial labor displacement), the social-democratic parties increasingly operated as suppressors of militant alternatives rather than coordinators of transformation — the founding problem (how to transform without dictatorship) persists unsolved, but the constraint now operates primarily to prevent rapid transformation rather than to coordinate it. Accessibility collapse is low (0.38) because alternatives — council organization, vanguard parties, syndicalism, spontaneous uprising — remain logically and organizationally available to workers, even if heavily suppressed. Resistance is high (0.72) because revolutionary movements, council communists, and wildcat organizers continuously mount real resistance to the gradualist frame, refusing to accept the suppression and persisting in their organizational forms despite institutional pressure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between social-democratic agenda-setters and revolutionary payers is the entire point: from the SP/union leadership perspective, the constraint is coordination around a shared goal (working-class power) with disagreement only about method and timeline. From the revolutionary perspective, the constraint is institutional co-optation of working-class energies into a system fundamentally opposed to their liberation. The engine's per-seat computation should show this as a clear type divergence: rope or even high-cooperation outcome for the agenda-setter, snare or tangled-rope capture outcome for the payers. This divergence is not an error in the JSON — it is exactly the measurement the system is built to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Social-democratic parties: d ≈ 0.1-0.2 (beneficiary, institutional power, arbitrage-level exit options — can shift toward centrist positions without abandonment of institutional seat). Labor unions: d ≈ 0.35-0.45 (secondary beneficiary/payer split — benefit from legal recognition and wage negotiation power, but constrained exit means they are partly locked into defending the constraint even as it erodes their members' transformative power). Revolutionary cadres: d ≈ 0.8-0.9 (target, powerless, identity-locked — suppression operates directly against them, exit means dissolution of political identity). Council communists: d ≈ 0.85 (target, powerless, local scope, identity-locked, structurally excluded from coalition). Direct-action organizers: d ≈ 0.75-0.80 (target, constrained exit via legal/workplace retaliation, powerless). The beneficiary seats (intellectual legitimators) sit around d ≈ 0.2. No directionality override is needed; the structural data (beneficiary/victim declarations + exit + power) produces appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The democratic gradualist reading does NOT suffer mandatrophy in the technical sense because its founding problem (how to achieve socialism without capitalist restoration or revolutionary dictatorship) remains live and contested. However, there is substantial ambiguity about whether the founding problem is still being addressed by the constraint or whether the constraint now operates primarily to prevent transformation altogether. The measurement series showing rising theater ratio and suppression with flat-to-declining actual transformative output suggests that the constraint is increasingly performing its coordination function rather than delivering it — this is piton-adjacent rather than mandatrophy in the technical sense, but it indicates that the constraint is aging into a maintenance-and-suppression role rather than a transformation role. The analysis should note this trajectory clearly: the constraint began as a genuine attempt to solve the coordination problem of how-to-transform-without-catastrophe, but as capitalist societies proved resistant to electoral-path socialism, the constraint increasingly operated to prevent the alternative approaches (revolution, councils) from gaining traction. This is a captured constraint hiding inside a coordination claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capitalist_state_co_optation_possibility,
    'Can liberal democratic state institutions genuinely be co-opted for socialist transformation, or are they structurally designed to protect capitalist property relations irreversibly?',
    'Historical evidence from actually-existing social-democratic governments when they attempted large-scale expropriation (Sweden 1970s-80s capital taxation, France 1981-86 Mitterrand nationalization, Venezuela 1999-2010 Chavez land reform, Bolivia 2006-19 Morales indigenous land and resource redistribution). Success = institutions can be co-opted; state reversal or capitalist adaptation = institutions are structurally constrained.',
    'If co-optation is possible, the democratic gradualist reading''s strategy is sound and the constraint is justifiable coordination. If institutions are structurally constrained, the constraint operates as a pacification mechanism — history of failed attempts would establish it as snare, not rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capitalist_state_co_optation_possibility, empirical, 'Whether liberal-democratic state institutions can be co-opted for socialist transformation or are fundamentally capitalist.').

omega_variable(
    vanguard_dictatorship_unavoidability,
    'Is authoritarian party dictatorship an inevitable outcome of revolutionary seizure of state power, as the gradualist reading claims, or is it a contingent outcome of specific Leninist organizational forms?',
    'Comparative historical analysis of revolutionary parties that did NOT produce dictatorship (anarchist collectives in Ukraine, Spanish Civil War, Paris Commune, Yugoslav workers'' councils 1950-1990 as partial counter-example); analysis of whether dictatorship followed from the logic of holding state power or from choice of centralized party organization.',
    'If dictatorship is inevitable from state seizure, the gradualist reading''s avoidance of revolution is justified. If dictatorship is contingent on party form, the constraint''s suppression of non-vanguard revolutionary approaches is not justified by necessity but by competing interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vanguard_dictatorship_unavoidability, conceptual, 'Whether revolutionary dictatorship is inevitable or contingent on organizational form.').

omega_variable(
    electoral_majority_sufficiency_for_expropriation,
    'Is an electoral majority sufficient to enact expropriation and decommodification of production, or do capitalist classes have extra-electoral power (capital flight, investment strike, military coup) that can veto electoral outcomes?',
    'Analysis of the structural power of capital (control of investment, employment, media, military loyalty) compared to the structural power of electoral majorities (control of lawmaking and executive). Historical case studies where elected left governments faced capital strike (Chile 1970-73, Greece 2015, UK 1974-79 ''Winter of Discontent'').',
    'If electoral majorities are sufficient, gradualism is strategically sound. If capital retains veto power, electoral majorities are structurally constrained and the constraint operates to channel class struggle into an institutionally powerless form — establishing it as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_majority_sufficiency_for_expropriation, empirical, 'Whether electoral majorities have sufficient structural power to enact expropriation against capitalist resistance.').

omega_variable(
    identity_lock_mechanism_in_militants,
    'Is the revolutionary militant''s commitment to rapid transformation identity-locked (fused with their self-concept and worldview such that exit means dissolution of identity), or is it strategically chosen and thus mobile?',
    'Post-transition studies: how many revolutionary activists remain committed to their reading if state power shifts or historical circumstances change (e.g., communists post-1989, Maoists after Deng reforms)? If commitment persists despite contrary evidence, identity-lock is confirmed; if activists abandon the reading when circumstances change, it is strategic choice rather than identity-lock.',
    'If identity-locked, the suppression cost is maximal (target cannot exit without self-dissolution) and the constraint''s asymmetry is severe. If strategic, the suppression is constrained (activists can shift orientations) and the constraint''s asymmetry is moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_militants, empirical, 'Whether revolutionary commitment is identity-fused or strategically mobile.').

omega_variable(
    reading_sibling_foreclosure,
    'Does the democratic-gradualist reading logically foreclose the vanguard-rupture reading, or do they coexist as live strategic options within the same working-class movement?',
    'Can a single working-class organization (party, union, council) coherently hold both readings simultaneously — both pursuing electoral strategy AND preparing for revolutionary rupture? Historical answer: most major parties (Communist Parties, Trotskyist tendencies) attempted to hold both; the contradiction between them created recurring factional splits.',
    'If foreclosure: one reading must be abandoned for the other. If coexistence: both remain live and the constraint''s operation is to suppress one coexistent reading in favor of another, making it snare-like (institutional power exercised to eliminate a live alternative). The commission rules to mark this as ''coexists_with'' rather than ''forecloses'' — the two readings cannot actually be held in a single framework at the strategic level, but they do coexist as competing claims held by different organizations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Logical relationship between democratic-gradualist and vanguard-rupture readings: foreclosure vs. coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mani_tr_t8, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(mani_tr_t16, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(mani_tr_t25, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(mani_tr_t35, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 35, 0.45).
narrative_ontology:measurement(mani_tr_t50, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mani_be_t8, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(mani_be_t16, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(mani_be_t25, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(mani_be_t35, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement(mani_be_t50, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mani_su_t8, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(mani_su_t16, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(mani_su_t25, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement(mani_su_t35, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 35, 0.48).
narrative_ontology:measurement(mani_su_t50, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.18).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% The democratic_gradualism_reading is one of three constraint stories decomposed from the contested kernel 'how should the working class organize to achieve socialism?' Each reading instantiates a different organizational form and strategic claim. This reading claims socialism is achievable through electoral majorities and institutional reform within liberal-democratic structures. The vanguard_rupture_reading claims revolutionary seizure of state power is necessary. The council_communist_reading claims direct democratic councils should replace both state and party. These are structurally distinct constraints with different beneficiaries, victims, and epsilon values. The democratic-gradualist reading influences the other two by establishing the default institutional frame that alternatives must argue against. All three share the founding kernel (the commitment to working-class liberation) but interpret it through incompatible institutional logics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
