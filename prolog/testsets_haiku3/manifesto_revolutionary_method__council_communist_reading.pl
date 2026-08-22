% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_council_communist_soviets, []).

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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Reading: Workers' Soviets as Direct Democratic Authority
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   The council communist reading instantiates workers' soviets (workplace
 *   assemblies) as terminal authority structures that replace both capitalist
 *   state apparatus and vanguard party apparatus. Authority is federated
 *   upward from shop-floor assemblies through sectoral and territorial
 *   councils, without permanent delegated executives or privileged
 *   administrative class. The reading claims low internal extractiveness
 *   (0.25 measured): councils coordinate production and conflict resolution
 *   through consensus deliberation among equals, with rotating representation
 *   and immediate recallability of delegates. Suppression is minimal
 *   internally (0.15: conflicts require only the persuasion needed to
 *   maintain federation, not coercive machinery). The constraint is contested
 *   by two sibling readings instantiating the same kernel
 *   (manifesto_revolutionary_method): the vanguard_rupture reading, which
 *   argues the party must seize state power as a prerequisite transition
 *   stage, and the democratic_gradualism reading, which argues existing
 *   democratic institutions can be reformed to worker control. This story
 *   generates the council_communist_reading alone, not the contested
 *   alternatives.
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Workplace assemblies with direct control over production decisions; hold authority through continuous participation; face pressure from both state and party to submit to external coordination
 *   - federated_delegates: Rotating representatives from workplace councils sent to sectoral and territorial coordination bodies; must maintain accountability to their base assemblies through recall rights; structurally positioned to resist permanent bureaucratization
 *   - state_bureaucratic_apparatus: Administrative structure maintaining commodity relations and private property; victims of council displacement because councils' authority claims make state enforcement of commodity law unnecessary
 *   - vanguard_party_officials: Communist party apparatus claiming historical mission and theoretical understanding; victims of council displacement because councils' direct democracy bypasses the party's claimed revolutionary leadership
 *   - rival_socialist_reading_holders: Advocates of vanguard party path and electoral gradualism; excluded from council decision-making; would object that councils cannot sustain revolutionary discipline or defend against counter-revolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.15).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Reading: Workers' Soviets as Direct Democratic Authority").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '1f8c23e6-b8d8-4f9f-a88e-61b63acd645a').
narrative_ontology:cs_kernel_codification('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', fixed_text).
narrative_ontology:cs_authority_grounding('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', lineage).
narrative_ontology:cs_interpretation_layer_present('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a').
narrative_ontology:cs_reading_relation('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', foundational, worker_collective_capability_direct_authority).
narrative_ontology:cs_axiom_status(worker_collective_capability_direct_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', worker_collective_capability_direct_authority, empirically_contingent).
narrative_ontology:cs_axiom('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', foundational, federated_coordination_without_reclassification).
narrative_ontology:cs_axiom_status(federated_coordination_without_reclassification, holdable).
narrative_ontology:cs_axiom_grounding('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', federated_coordination_without_reclassification, instrumental).
narrative_ontology:cs_axiom('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', secondary, terminal_authority_councils_not_transitional).
narrative_ontology:cs_axiom_status(terminal_authority_councils_not_transitional, holdable).
narrative_ontology:cs_axiom_grounding('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', terminal_authority_councils_not_transitional, deontological).
narrative_ontology:cs_reference_frame('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', worker_direct_democracy_terminal).
narrative_ontology:cs_drift_state('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', post_leninism_failure_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1f8c23e6-b8d8-4f9f-a88e-61b63acd645a', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucratic_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, federated_delegates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workplace assemblies of production workers with direct control over production decisions, work allocation, and distribution of output. They participate in sectoral and territorial councils through elected rotating delegates. They benefit from authority and autonomy; they also bear the cost of deliberation and coordination labor. Exit would mean abandoning collective control and returning to wage labor or joining a vanguard party structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, identity_locked, global).

% Elected representatives rotating through sectoral and territorial council coordination roles. They translate base assembly preferences into federation-wide decisions and coordinate inter-sectoral production. They bear the burden of consensus-building and are immediately recallable if decisions diverge from base preference. They set the agenda within the federation but must maintain accountability to assembly mandates.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, federated_delegates, agenda_setter,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, federated_delegates, payer).

% Administrative structures maintaining commodity relations, private property enforcement, and state monopoly on legitimate violence. Under council authority, the state apparatus loses its function: councils coordinate production without commodity markets, enforce collective property rights without private courts, and manage internal conflicts through assembly deliberation rather than police power. The apparatus cannot adapt; it must be replaced.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucratic_apparatus, payer,
    institutional, generational, trapped, global).

% Communist party cadres claiming historical understanding and revolutionary leadership. Under council authority, the party's claim to leadership and its function as vanguard are negated: workers decide directly, bypassing the party's mediation. The party cannot adapt to council authority; it must either seize state power and subordinate councils to party discipline, or lose its organizational purpose. The constraint eliminates the party's structural position.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, generational, trapped, global).

% Advocates of vanguard party path (Leninism, Maoism) and electoral democratic socialism would argue that councils cannot sustain revolutionary discipline, cannot defend against counter-revolution, and require party guidance. They are structurally excluded from council decision-making by the council reading's own logic (direct democracy admits no privileged vanguard). Their objections would not be heard in council deliberation; they would only become relevant if councils were militarily defeated or politically displaced by a vanguard seizure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rival_political_readings, excluded,
    institutional, generational, trapped, global).

% Historians, political theorists, economists who study the council reading as a lived political project and as a theoretical claim about coordination mechanisms. They assess the council reading's feasibility at scale, its resilience under external pressure, and its relationship to rival readings. They are external to the constraint structure and neither benefit nor pay, but their analysis shapes understanding of the constraint's viability.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordination of production, distribution, conflict resolution, and collective decision-making through federated workplace assemblies. Solves the problem of how to organize complex production and distribute resources without either commodity markets (capitalism) or hierarchical command (statism).
% TRANSFER_FUNCTION: Transfers authority over production decisions from both state bureaucrats and capitalists (capitalism) and from vanguard party officials (statism) to autonomous worker collectives. No fixed transfer of value or goods; the redistribution is of power and autonomy, not of extracted surplus.
% ABSENT_VOICES: Vanguard party officials and state administrators are not present in council deliberation by the council reading's own logic; they are structurally excluded because the reading denies their legitimacy as mediators. They would argue that councils cannot defend revolutionary gains, that direct democracy is inefficient, and that the party must guide production. They are also materially prevented from participation because councils displace their power; their presence would represent a return to hierarchical structure.
% DISAPPEARANCE_RATIONALE: If councils disappeared overnight, production would not be commanded by workers but by either capitalist property relations (if capitalism reasserted) or by vanguard party hierarchy (if Leninism reasserted). The entire structure of who makes decisions, who benefits from production, and how conflicts are resolved would reorganize. The world does not return to a baseline; it reorganizes under an alternative authority structure.
% FOUNDING_PROBLEM: Production under capitalism requires workers to sell their labor to capitalists who control decisions; production under statism requires workers to obey party-appointed administrators. Both deny workers autonomy and control. The founding problem is: how to organize production collectively, without both commodity exchange and hierarchical command?
% FOUNDING_PROBLEM_CORROBORATION: The council communist reading itself affirms the founding problem is live. Historical advocates (Pannekoek, Gorter, autonomist Marxists, Council Communists of Germany and the Netherlands) attest the problem. Historical counter-testimony: Leninist accounts argue the founding problem is solved through party leadership (not by councils); social-democratic gradualists argue it is solved through electoral democracy and regulatory reform. Independent economic analysis from outside the revolution (modern heterodox economics) contests whether councils can match capitalist efficiency or whether planned federation requires technical cadre (subtle form of party re-emergence). Historiographical consensus: councils in practice faced either military defeat or infiltration by vanguard party cadres; no sustained, large-scale council system proved stable against external pressure or internal pressure toward re-centralization.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.25) because councils distribute decision authority horizontally among workers; no permanent class of administrators collects rents from production. The measurement series shows slight upward drift (0.18→0.25 over interval 0-10, then stable) reflecting: historical pressure toward delegation and professionalization of council functions, initial idealism giving way to coordination cost absorption, but no dramatic re-stratification into the 0.5+ range typical of snares or tangled ropes. Suppression requirement is similarly low-to-moderate (0.08→0.15) because internal compliance is sustained by participation and consensus, not by dedicated coercive apparatus. Theater ratio is minimal (0.05→0.08) because the functional work of councils (production coordination, conflict resolution, resource distribution) IS the legitimate operation — there is no significant gap between performance for display and actual coordination. Accessibility collapse is moderate (0.35) because while the council reading offers workers real decision authority, the material costs of defection (losing livelihood outside capitalist firms, invasion of the councils by state/party forces) are substantial; alternatives (going back to wage labor, joining vanguard party) are available but costly. Resistance is high (0.72) because council implementation faces continuous pressure from both state police power and party discipline; the measurement endpoint reflects historical cases where councils were crushed by vanguard parties (Russia 1918-1921, Germany 1919) and by counter-revolutionary states (Spain 1937, Hungary 1956). Stub this: the story measures the constraint UNDER THIS READING, not the total field pressure; external suppression from rival readings is modeled as a separate empirical fact, not attributed to the council constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   From a worker collective's seat, the council constraint is pure coordination and beneficial autonomy; extraction is nearly zero because workers decide what to do with what they produce. From a state bureaucrat's seat, the same constraint is catastrophic dispossession: the entire administrative apparatus whose authority rested on controlling production decisions is eliminated. From a vanguard party official's seat, councils represent an unwanted rival authority structure that prevents the party from guiding production according to historical understanding; the party experiences the councils as a constraint limiting its power. From the analytical seat, the constraint is a genuine attempt at coordination without hierarchy, but its stability under pressure and its scalability are open empirical questions. The engine should compute these divergences directly from the authored power atoms and roles; I have declared the structural asymmetry in base_properties beneficiaries/victims and stakeholder roles; the engine derives the per-seat directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Worker collectives are beneficiaries (d near 0.0): they receive authority, participate in decisions affecting their work, and capture the surplus generated. Their exit options are constrained (identity_locked and mobile both apply: they depend on the collective for livelihood, but they could theoretically join rival political movements or accept wage labor) — moderate power but structured as agents of the constraint rather than targets. State bureaucrats and party officials are victims (d near 1.0): they lose the privilege of commanding workers from outside. They are institutional power but face material elimination if councils displace their authority; their time horizon is generational because state/party apparatus renewal requires ongoing recruitment of people willing to manage for capital or vanguard. Federated delegates are structurally near-symmetric (d~0.5): they receive authority and participate, but they also bear the weight of maintaining federation stability and face immediate recall if their decisions diverge from base assembly preference — the constraint both empowers and constrains them. The analytical observer seat (sociologists, historians, political theorists) has arbitrage-grade exit and remains neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was: production coordination without either commodity exchange (capitalism) or hierarchical command (statism). This is claimed as the constraint's continued justification — councils solve this problem through federated deliberation. The mandatrophy frame: if councils persist but stop actually coordinating production (become theater), or if they are captured by party cadres who turn them into administrative arms of the vanguard, the original mandate has died but the structure persists. The measurement series at 0.08 theater ratio suggests the internal operation is still mostly functional coordination rather than ritual; if theater_ratio climbed above 0.5, that would indicate mandate drift. Mandatrophy_resolved: declared false because the constraint is continuously contested and justified by reference to its founding problem; if conditions changed and councils became ritualized or party-captured, the classification would shift to piton. Current stance: rope (coordination without extraction), with live questions about scalability and durability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scale_coordination_feasibility,
    'Can federated workplace councils coordinate production at the scale of a modern industrial economy without degrading into either hierarchical reconstruction or coordination failure?',
    'Historical observation from attempted council implementations (early Soviet Russia 1917-1918, Paris Commune, Yugosalv works councils, Barcelona 1936-1937, Rojava cantons post-2011) measuring: duration of coordination stability, transaction costs of consensus, response speed to supply disruption, degree of re-centralization pressure.',
    'If feasible at scale, the reading''s core claim (councils as workable authority structure) holds; if scale-dependent failures drive re-centralization, the constraint may degrade into vanguard_rupture_reading''s instantiation. If coordination cost is prohibitive but efficiency acceptable at small scale, the reading fragments into local-only validity (constraints the scope, does not refute the reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scale_coordination_feasibility, empirical, 'Whether workers'' councils can coordinate industrial-scale production without re-stratification').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Do the council_communist and vanguard_rupture readings logically foreclose each other, or do they coexist as live strategic options held by different revolutionary factions within the same historical moment?',
    'Textual and historical analysis: (a) do the readings share a common foundational premise they diverge on (suggests coexistence — different strategies within shared theory) or do they start from contradictory premises (suggests foreclosure)? (b) Did historical actors holding one reading attempt to refute the other as logically impossible, or did they contest the practical viability / effectiveness? (c) Do the readings map to different class fractions or organizational contexts (coexistence signal) or to universal strategic claims (foreclosure signal)?',
    'If foreclosed: one reading''s adoption within a framework eliminates the other''s logical coherence — framework coherence is the constraint; one reading''s political victory would constitute a logical refutation of the other. If coexist: both remain options for different actors / contexts; political defeat does not equal logical refutation — opening for recurrence. Determines the cs_structure.reading_relations atom for vanguard_rupture_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'The logical/strategic structure of the council vs. vanguard contest within revolutionary Marxism').

omega_variable(
    external_suppression_source,
    'Is the measured suppression of council authority (0.15 internal, but implied high external by ''high external suppression by rival readings'') a property of the council constraint itself or a property of surrounding vanguard and state structures that contest it?',
    'Distinguish: (a) internal suppression = a council structure''s own requirement to enforce compliance (how much coercion the councils themselves deploy to maintain order); (b) external suppression = pressure FROM rival structures (party discipline, state force, rival councils with different authority claims). Measure by observing which enforcement apparatus operates and who operates it.',
    'If the suppression is external (rival structures enforce against councils), the constraint''s own extractiveness and suppression scores capture only the within-council operation — the full picture requires modeling the rival readings'' constraints in parallel. If internal, the councils'' authority requires substantial coercion even over their own constituent members. Distribution affects classification: high external/low internal suggests rope under internal view, snare/tangled_rope under external-pressure view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_suppression_source, empirical, 'Locus of suppression: councils'' own requirement or external rivalry''s pressure').

omega_variable(
    identity_lock_council_participation,
    'For a worker in a federated council system, is the constraint''s persistence dependent on identity-fusion (workers have become constituted as self-governing collectives; exit is identity-dissolving) or on structural coordination benefit (exit is available but costly)?',
    'Ethnographic/historical observation: post-exit, do workers retain autonomous decision-making capacity and organizational identity, or does exit dissolve the worker identity itself? Can a worker leave a council without identity rupture? Do councils require continuous buy-in or are they maintained by path dependence after formation?',
    'If identity-locked: the constraint''s persistence is robust against external pressure but vulnerable to identity-rupturing events (defeat, infiltration, cooptation by vanguard that turns councils into administrative arms). If structurally coordinate-benefit: exit cost is real but the constraint is theoretically replaceable and workers retain autonomous identity outside councils. Affects directionality computation for the worker seat and stability predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_council_participation, empirical, 'Whether council participation is identity-constitutive or coordinatively beneficial').

omega_variable(
    historical_reading_instantiation,
    'Which historical actors and texts instantiate the council_communist reading as a distinct position, vs. merely adopting council forms as a tactical stage before vanguard centralization or state apparatus takeover?',
    'Historiography: identify authors, movements, and organizations that explicitly advocated councils as TERMINAL authority structure (not transitional): council communists (Germany/Netherlands, Pannekoek, Gorter), autonomous Marxists, Italian autonomia, contemporary autonomist traditions. Distinguish from: Leninist use of soviets as revolutionary tool before state centralization; anarcho-syndicalists who rejected councils as state-building; social democrats who treated councils as extended bargaining forums.',
    'If the reading is instantiated clearly in identifiable historical tradition, its axioms and reading relations are grounded in actual dispute; if the reading is primarily an analytical reconstruction with minimal historical instantiation, the constraint models a live possibility that was historically minoritarian (affects confidence and status assessments).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_reading_instantiation, empirical, 'Historiographical grounding of the council communist reading as a distinct lived position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(mani_tr_t15, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(mani_tr_t25, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 5, 0.21).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(mani_be_t15, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(mani_be_t25, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 25, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(mani_su_t15, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(mani_su_t25, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 25, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% The manifesto_revolutionary_method kernel admits three distinct constraint readings. This story (council_communist_reading) models the coordination structure of federated workplace democracy. The vanguard_rupture_reading models the party-mediated state seizure as an alternative revolutionary path. The democratic_gradualism_reading models electoral institutional reform. Each reading has distinct ε (council: 0.25 internal, vanguard: higher, gradualism: lower), distinct beneficiary/victim sets, and distinct typology. All three are linked as members of the same constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
