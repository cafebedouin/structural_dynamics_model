% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Revolutionary Seizure and Proletarian Dictatorship
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates the vanguard rupture reading of the
 *   manifesto revolutionary method kernel — the commitment that revolutionary
 *   transformation requires organized party seizure of state power and that
 *   the dictatorship of the proletariat functions as a necessary transitional
 *   state form under party guidance. The reading claims that capitalism
 *   cannot be reformed through democratic gradualism and that autonomous
 *   worker organizations lack the discipline and theory to prevent
 *   counter-revolutionary restoration. The party vanguard is positioned as
 *   the agent with superior theoretical understanding (scientific socialism)
 *   and organizational capacity to navigate the transition. This reading is
 *   contested: council communists argue that workers' councils (soviets)
 *   should replace both capitalist state AND vanguard party; democratic
 *   gradualists argue transformation can occur through electoral majorities
 *   and democratic institutions. The vanguard reading suppresses both
 *   alternatives through organizational monopoly and state enforcement,
 *   making this a high-extraction, highly-enforced constraint.
 *
 * KEY AGENTS:
 *   - Party vanguard: organized revolutionary leadership, identity-locked to party mission, controls state apparatus and suppresses alternatives
 *   - Central planning apparatus: state bureaucracy that collects rents through authority over production allocation and priority goods distribution
 *   - Industrial working class: theoretical beneficiary but practically subject to labor discipline, wage suppression, and suppression of autonomous organization
 *   - Political pluralists and autonomous worker organizations: victims trapped within the national scope, denied voice in decision-making, systematically suppressed
 *   - Independent peasantry: victims subject to forced collectivization, requisitions, and material deprivation during transition
 *   - International socialist movement: observers debating vanguard strategy but lacking binding authority over national implementations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.79).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Revolutionary Seizure and Proletarian Dictatorship").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '461f29ac-0624-45fc-a7c8-1533834b11d7').
narrative_ontology:cs_kernel_codification('461f29ac-0624-45fc-a7c8-1533834b11d7', formalized).
narrative_ontology:cs_authority_grounding('461f29ac-0624-45fc-a7c8-1533834b11d7', lineage).
narrative_ontology:cs_interpretation_layer_present('461f29ac-0624-45fc-a7c8-1533834b11d7').
narrative_ontology:cs_reading_relation('461f29ac-0624-45fc-a7c8-1533834b11d7', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('461f29ac-0624-45fc-a7c8-1533834b11d7', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('461f29ac-0624-45fc-a7c8-1533834b11d7', foundational, vanguard_party_scientific_superiority).
narrative_ontology:cs_axiom_status(vanguard_party_scientific_superiority, holdable).
narrative_ontology:cs_axiom_grounding('461f29ac-0624-45fc-a7c8-1533834b11d7', vanguard_party_scientific_superiority, deontological).
narrative_ontology:cs_axiom('461f29ac-0624-45fc-a7c8-1533834b11d7', foundational, capitalist_restoration_permanent_threat).
narrative_ontology:cs_axiom_status(capitalist_restoration_permanent_threat, holdable).
narrative_ontology:cs_axiom_grounding('461f29ac-0624-45fc-a7c8-1533834b11d7', capitalist_restoration_permanent_threat, empirically_contingent).
narrative_ontology:cs_axiom('461f29ac-0624-45fc-a7c8-1533834b11d7', secondary, autonomous_worker_organization_insufficient_for_transition).
narrative_ontology:cs_axiom_status(autonomous_worker_organization_insufficient_for_transition, overridden).
narrative_ontology:cs_axiom_grounding('461f29ac-0624-45fc-a7c8-1533834b11d7', autonomous_worker_organization_insufficient_for_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('461f29ac-0624-45fc-a7c8-1533834b11d7', scientific_socialism_vanguard_necessity).
narrative_ontology:cs_drift_state('461f29ac-0624-45fc-a7c8-1533834b11d7', late_vanguard_regime_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('461f29ac-0624-45fc-a7c8-1533834b11d7', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_vanguard).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, central_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, independent_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, intellectual_cadres_of_marxism).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The disciplined revolutionary organization that seizes state power and exercises dictatorship of the proletariat on behalf of the working class. Members have fused their identities with the party's historical mission; cadres occupy all key decision positions and control the state apparatus. The vanguard justifies its monopoly on power as necessary to prevent counter-revolutionary infiltration and to guide the working class through the transition to communism. Exit from the party means abandoning revolutionary identity and career.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_vanguard, agenda_setter,
    organized, generational, identity_locked, national).

% The state bureaucracy coordinating socialist production through centralized planning. Its members gain stable employment, advancement through party loyalty, and authority over production allocation. The apparatus operates under party guidance and receives rents in the form of priority allocation for administrative goods and services. Its perpetuation depends on maintaining the vanguard's organizational supremacy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, central_planning_apparatus, beneficiary,
    institutional, generational, constrained, national).

% In theory the ultimate beneficiary of the dictatorship of the proletariat — the class on whose behalf power is seized and the transition is managed. In practice subject to party discipline over labor allocation, wage suppression in the name of capital accumulation for transition, and suppression of independent trade union organization. They cannot exit the spatial-national scope and have no alternative power structure to join.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class, payer).

% Parties, movements, and intellectuals advocating democratic participation, multi-party competition, or constitutional limits on state power. Systematically excluded from decision-making, suppressed through party enforcement machinery, and discredited as counter-revolutionary or petty-bourgeois. Their exit requires fleeing the national territory or abandoning their political commitments entirely.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Worker councils, independent trade unions, factory committees, and soviets that arise from working-class self-organization during revolutionary upheaval. The vanguard party subordinates these to party discipline, converts them into transmission belts for party directives, or dissolves them entirely when they resist centralization. Workers in these organizations cannot independently organize and face suppression if they attempt autonomous action.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    powerless, biographical, trapped, national).

% Agricultural producers who initially supported or were neutral to revolutionary transformation. Subject to forced collectivization, grain requisitions, and party control over production. Cannot exit agricultural production or the territorial scope. Bear severe material costs through requisitions and starvation during transition.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, independent_peasantry, payer,
    powerless, biographical, trapped, national).

% Intellectuals, theorists, and technical specialists who staff the central planning apparatus and party ideology apparatus. Gain status, secure employment, and the opportunity to implement their understanding of historical materialism. Career advancement and security depend on conformity with party line and vanguard leadership's theoretical authority.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, intellectual_cadres_of_marxism, beneficiary,
    organized, generational, identity_locked, national).

% Capitalist class remnants, landlords, imperialist powers, and their agents. Structurally excluded from the political order the vanguard is constructing. Their exclusion is the entire point of the dictatorship — they would object vociferously if in the room and their suppression is justified as prerequisite to transition. They either flee or are liquidated.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, counter_revolutionary_forces, excluded,
    powerful, biographical, trapped, national).

% Other communist parties, socialist movements, and international revolutionary organizations that observe and debate the vanguard strategy. Some advocate for council communism or democratic gradualism; others endorse the vanguard model. The vanguard's organizational success in one nation influences their standing in global revolutionary politics, but the international movement lacks binding authority over national vanguards.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, international_socialist_movement, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, party_vanguard).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the transition from capitalist to communist production by centralizing control of the means of production under a unified planning authority, preventing atomized decisions by individual capitalists or workers from fragmenting revolutionary gains, and coordinating the disciplined allocation of labor and resources according to a unified development program.
% TRANSFER_FUNCTION: Moves decision-making authority from plural actors (capitalists, workers' councils, individual producers) to the party vanguard and central planning apparatus. Moves material surplus from workers and peasants to the state apparatus and the party's reproduction as an organization. Moves political voice away from independent organizations toward party-mediated channels.
% ABSENT_VOICES: Autonomous worker organizations, pluralist political movements, and independent peasant associations — all of which are explicitly excluded from the decision-making structure because the vanguard reading treats them as sources of counter-revolutionary or reformist pressure. These excluded parties would argue for decentralized worker control, democratic participation, or gradual institutional reform but are kept out by the vanguard's organizational monopoly.
% DISAPPEARANCE_RATIONALE: If the vanguard party's monopoly on power and the dictatorial state apparatus disappeared overnight, the entire structure of centralized socialist planning would collapse. Capital accumulation, production allocation, and class relations would reorganize — either reverting to capitalist markets, devolving to worker councils, or fragmenting into competing power centers. The political landscape would immediately fill with the suppressed pluralist and autonomous organizational forces.
% FOUNDING_PROBLEM: Capitalist production creates anarchic competition, periodic crises, and exploitation of labor. Revolutionary seizure requires overcoming organized capitalist resistance, coordinating millions of workers across industries, preventing counter-revolutionary sabotage during transition, and managing the technical complexity of planned production at national scale. A disciplined revolutionary party is necessary to accomplish what uncoordinated masses cannot.
% FOUNDING_PROBLEM_CORROBORATION: The vanguard party claims the founding problem remains live — transition is incomplete, capitalist restoration threatens, counter-revolutionary forces persist. Advocates of council communism and democratic gradualism argue the founding problem is substantially resolved or was mis-stated: they claim that worker self-organization, not party dictatorship, solves coordination; that vanguard suppression of autonomous organization prevents rather than enables workers' liberation; and that historical experience shows vanguard parties become permanent bureaucratic elites rather than transitional organs. Historical-materialist scholars outside the benefiting parties (council communist theorists, democratic socialist historians, post-communist analysts) offer corroboration for the contested reading.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval: early in the revolutionary period, when threat of counter-revolution is highest and party control is not yet total, extraction is moderate — the party must still negotiate with autonomous organizations and retain working-class support. As the state consolidates and autonomous alternatives are suppressed (10–30 year mark), extractiveness rises as the party's grip tightens and the planning apparatus becomes the sole allocative mechanism. It plateaus at 0.68 because indefinite extraction beyond this level triggers open resistance and organizational breakdown (peasant flight, worker dissatisfaction, defection of cadres). Suppression rises sharply early and plateaus: the vanguard party's monopoly must be continuously defended through enforcement machinery — secret police, party discipline, control of information — because the suppressed constituencies (pluralists, autonomous worker organizations) remain structural threats. Theater rises from 0.25 to 0.42: early revolutionary periods have genuine coordination function (preventing capitalist counter-revolution, coordinating emergency responses), but as the transition deepens and the founding problem recedes (transition is supposedly temporary), increasing share of party activity becomes performative — public trials of counter-revolutionaries, mass rallies affirming party infallibility, theoretical debates about dialectical materialism — rather than functional necessity. The measurement series tracks extractiveness and suppression separately because they respond to different dynamics: extraction is pulled by the beneficiary accumulation incentive; suppression is driven by the need to prevent alternatives from reorganizing.
 *
 * PERSPECTIVAL GAP:
 *   The party vanguard experiences this constraint as genuine coordination necessity: without organizational monopoly and dictatorial state power, capitalist forces would sabotage the transition and restore exploitation. Their reading is that suppression of alternatives is temporary, justified by the existential threat to revolution itself. Pluralists experience it as pure extraction and suppression masked by revolutionary rhetoric: the 'dictatorship of the proletariat' becomes dictatorship by a party claiming to speak for the proletariat. Autonomous worker organizations experience it as betrayal: they fought the revolution expecting workers' councils to hold power, not a new bureaucratic elite. The industrial working class experiences it as a two-way seat: they gain security from capitalist competition and unemployment (genuine coordination benefit) but lose the right to organize independently and see wages and living standards suppressed in the name of capital accumulation for transition (extraction and suppression). The peasantry experiences it as near-pure coercion: if they resisted forced collectivization, they faced starvation or execution; if they complied, they faced starvation through requisitions. The engine's per-seat classification should capture these divergences: tangled rope from the vanguard and planning-apparatus seats (genuine coordination function + asymmetric extraction), snare from the pluralist and autonomous-worker seats (pure suppression), complex hybrid from the working-class seat (coordination + extraction in tension). None of these divergences mean the constraint itself is 'really' any type — they mean each seat perceives and experiences it differently, and the engine computes the type from that seat's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The party vanguard (organized, identity-locked) should compute near-pure-beneficiary: it sets the rules, controls the machinery, reproduces itself, and faces no meaningful exit pressure. The central planning apparatus (institutional, constrained exit) is a beneficiary with some payer elements — it gains rents and security but is subordinate to party directives and could theoretically be dissolved. The industrial working class (powerless, trapped, generational horizon) bears asymmetric extraction through labor discipline and wage suppression, despite being theoretically the beneficiary class on whose behalf the dictatorship is supposedly exercised — the theory/practice gap is precisely what makes this tangled rope rather than rope. Political pluralists and autonomous worker organizations (moderate-to-powerless, trapped, biographical horizon) are pure targets — they pay suppression costs and have zero decision power. The peasantry (powerless, trapped, biographical) are pure targets during forced collectivization. The international socialist movement (organized, analytical exit) observes but is excluded from this particular constraint's decision space. This divergence of directionalities between seats is the seat divergence the engine computes: from the vanguard's seat, this is rope (coordination under party guidance for historical transition); from the pluralists' or autonomous workers' seat, this is snare (suppression without genuine benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The vanguard rupture reading claims the founding problem (preventing capitalist counter-revolution and managing transition to communism) is live and permanent justification for the vanguard dictatorship. Historical outcomes suggest mandatrophy: by the later decades of vanguard regimes (USSR by the 1970s–80s, China by the 1980s–2000s), the counter-revolutionary threat had substantially receded, productive forces had developed, and the 'transitional' state was persisting indefinitely as a permanent bureaucratic apparatus. Yet the justification — 'transition is not yet complete' — persisted as pure theater and institutional self-maintenance. This is the classical mandatrophy pattern: the founding problem is no longer the driver of the arrangement; instead, the arrangement persists because the beneficiary (party and planning apparatus) has no incentive to resolve the founding problem (if transition 'completed,' their power would disappear). The theater ratio rising from 0.25 to 0.42 models this: early revolutionary periods have high functional necessity; later periods show increasing share of party activity devoted to maintaining its own authority rather than solving the original founding problem. The vanguard reading itself does NOT acknowledge this mandatrophy — it asserts perpetual necessity of transition and counter-revolutionary threat — which is exactly the cover story mandatrophy generates. A council communist or democratic gradualist reading would diagnose this more clearly: they would say the founding problem was either mis-stated (transition doesn't require vanguard dictatorship) or has already been overcome, and the constraint's persistence is pure inertia and power-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vanguard_authenticity_ambiguity,
    'Does the party vanguard genuinely represent the proletariat''s interests, or does it constitute a new exploiting class separated from the workers it claims to speak for?',
    'Historical trajectory analysis: if party bureaucrats become a self-reproducing elite with interests structurally opposed to working-class interests (as Trotskyist and council communist analyses argue), then the vanguard is not authentically representing the proletariat but rather extracting from it. Post-regime analysis of cadre mobility, material privilege, and intergenerational reproduction of party elites provides evidence.',
    'If the vanguard is found to be a new exploiting class, the entire classification shifts: this is not tangled rope (coordination + extraction for the working class) but snare (pure extraction by party cadres using proletarian ideology as cover). The ''dictatorship of the proletariat'' becomes dictatorship by a party elite, not by the class itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vanguard_authenticity_ambiguity, empirical, 'Whether the vanguard party represents proletarian interests or constitutes a new bureaucratic class.').

omega_variable(
    transition_temporality_ambiguity,
    'Is the dictatorship of the proletariat genuinely transitional (as the vanguard reading claims), or is it structurally permanent under any vanguard organizational model?',
    'Comparative analysis of vanguard regimes: have any successfully ''withered away'' the state and planning apparatus as Marx and Lenin theorized, or do all vanguard regimes tend toward indefinite perpetuation? Do alternative models (council communism, democratic socialism) show different trajectories?',
    'If vanguard dictatorships structurally perpetuate rather than transition, the foundational claim of the vanguard reading is false — the arrangement is permanent extraction, not temporary coordination. This would support mandatrophy diagnosis and might support reclassification to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_temporality_ambiguity, empirical, 'Whether vanguard dictatorship is genuinely transitional or structurally permanent.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of autonomous worker organizations and pluralist parties primarily structural (external barriers, coercive force) or internalized (workers and cadres have adopted party ideology and no longer seek alternatives)?',
    'Post-regime analysis of defector testimony, archival evidence of dissent suppression, and worker behavior when regime constraints are removed. If suppression persists after regime collapse, it indicates internalization; if alternatives emerge immediately, it indicates structurally enforced suppression.',
    'If suppression is primarily structural, the constraint''s extractiveness might be lower if enforcement were withdrawn — alternatives would immediately organize. If suppression is internalized, the constraint has created deeper identity fusion and is more stable even after formal structures weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternatives is structural or internalized.').

omega_variable(
    foundational_problem_misstatement,
    'Is the vanguard reading''s foundational problem (anarchic capitalism requires coordinated revolutionary transformation through party dictatorship) accurately stated, or do democratic gradualism and council communism identify a different problem that the vanguard reading obscures?',
    'Comparative historical-materialist analysis: can the actual coordination problems of capitalist transition be solved through gradualist or council-communist routes without vanguard dictatorship? Do vanguard regimes solve the stated problems better or worse than alternatives?',
    'If the foundational problem is mis-stated and alternatives can solve the genuine problems better, the vanguard reading is not just one reading among equals — it would be structurally false and the classification would shift toward snare (suppression without genuine coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_problem_misstatement, conceptual, 'Whether the vanguard reading''s foundational problem is accurately identified or obscures the real coordination challenge.').

omega_variable(
    kernel_identity_over_readings,
    'Is the manifesto revolutionary method kernel a single commitment with multiple defensible readings, or are the vanguard, council communist, and democratic gradualist models so structurally distinct that they should be treated as separate kernels with incompatible axioms?',
    'Textual analysis of Marx and Lenin: can a single interpretive tradition hold all three readings without incoherence, or does commitment to one reading logically foreclose the others? Does the framing of a unified kernel obscure the depth of the disagreement?',
    'If the readings cannot share a unified kernel, the committer frame''s assumption of shared contested commitment collapses — we have three separate constraint families (vanguard, council communist, gradualist), not three readings of one kernel. This would change how the corpus treats the relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_over_readings, conceptual, 'Whether the revolutionary method kernel is genuinely unifiable or whether the readings are structurally distinct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(mani_tr_t50, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(mani_be_t50, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(mani_su_t50, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 50, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the manifesto revolutionary method kernel family. All three readings (vanguard_rupture_reading, council_communist_reading, democratic_gradualism_reading) are structurally distinct constraints with different ε values, beneficiary/victim structures, and suppression mechanisms. They share a kernel (contested commitment about how revolutionary transformation occurs and what state forms transition requires) but instantiate contradictory implementations. Each reading is a separate constraint story linked by affects_constraints. The vanguard reading influences both siblings by establishing organizational suppression machinery that blocks their implementation; the council communist and democratic gradualist readings coexist as alternatives held by different parties to the historical dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, powerless, 0.95).
constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
