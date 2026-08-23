% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism: The Parliamentary Road as the Standing Method of Working-Class Strategy
 *   domain: political philosophy / revolutionary theory / historical materialism
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the manifesto_revolutionary_method
 *   kernel — the democratic_gradualism_reading: the commitment that socialism
 *   is achievable through a democratic electoral majority and gradual
 *   institutional reform, with working-class power exercised through existing
 *   democratic structures. The constraint under classification is the
 *   standing arrangement that commitment constitutes: the channeling of
 *   working-class political capacity through electoral, parliamentary, and
 *   collective-bargaining institutions, enforced by party discipline, union
 *   rulebooks, and the 'adventurist' censure of extra-parliamentary currents.
 *   Per the fixed kernel-reading rule, ε's referent is this standing
 *   arrangement itself, assessed by this reading's own lights: the reading
 *   inhabits and endorses the channel, yet the channel's own structural
 *   record names real costs borne by real parties — revolutionary militants
 *   expelled, council experiments in 1918-19 dissolved into parliamentary
 *   channels, transformation deferred to receding electoral horizons — so ε
 *   is moderate (0.40), neither the near-zero value the reading's endorsement
 *   would suggest nor the high value a sibling reading would author for the
 *   same history. The sibling readings (vanguard_rupture_reading,
 *   council_communist_reading) are separate constraint files with their own
 *   ε, beneficiary/victim structures, and enforcement machinery; their deltas
 *   are routed to the omega variables, not folded into this one. KEY AGENTS
 *   (by structural relationship): social_democratic_party_leaderships —
 *   agenda-setter and primary beneficiary (institutional/identity_locked);
 *   trade_union_bureaucracies — secondary beneficiary and co-enforcer
 *   (institutional/constrained); working_class_electorate — dual-positioned
 *   class base (organized/constrained); revolutionary_militants — primary
 *   target (moderate/constrained); dual_power_organizers — crisis-moment
 *   target (powerless/trapped); revolutionary_opposition_factions — excluded
 *   currents (moderate/mobile); liberal_constitutional_institutions —
 *   incidental beneficiary (institutional/constrained); movement_historians —
 *   analytical observer attesting the record from outside the benefiting
 *   parties.
 *
 * KEY AGENTS:
 *   - social_democratic_party_leaderships: agenda-setter and primary beneficiary (institutional/identity_locked) — sets strategy, enforces the electoral-road commitment, collects votes, subsidies, and offices
 *   - trade_union_bureaucracies: secondary beneficiary and co-enforcer (institutional/constrained) — bargains inside the legal frameworks the channel maintains, enforces industrial peace
 *   - working_class_electorate: dual-positioned class base (organized/constrained) — receives reforms, defers transformation, supplies votes and dues
 *   - revolutionary_militants: primary target (moderate/constrained) — expelled and censured as 'adventurist' for contesting the electoral-road commitment
 *   - dual_power_organizers: crisis-moment target (powerless/trapped) — build councils and strike committees the channel declines to defend
 *   - revolutionary_opposition_factions: excluded currents (moderate/mobile) — vanguard, council, and syndicalist currents outside the platform
 *   - liberal_constitutional_institutions: incidental beneficiary (institutional/constrained) — receives legitimacy and stability from absorbed revolutionary pressure
 *   - movement_historians: analytical observer — attests the founding problem's status and the program-practice gap from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.55).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism: The Parliamentary Road as the Standing Method of Working-Class Strategy").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political philosophy / revolutionary theory / historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'fc29a394-5b47-4d39-b737-baa86a4f6398').
narrative_ontology:cs_kernel_codification('fc29a394-5b47-4d39-b737-baa86a4f6398', fixed_text).
narrative_ontology:cs_authority_grounding('fc29a394-5b47-4d39-b737-baa86a4f6398', lineage).
narrative_ontology:cs_interpretation_layer_present('fc29a394-5b47-4d39-b737-baa86a4f6398').
narrative_ontology:cs_reading_relation('fc29a394-5b47-4d39-b737-baa86a4f6398', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('fc29a394-5b47-4d39-b737-baa86a4f6398', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('fc29a394-5b47-4d39-b737-baa86a4f6398', foundational, existing_state_adequate_vehicle).
narrative_ontology:cs_axiom_status(existing_state_adequate_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('fc29a394-5b47-4d39-b737-baa86a4f6398', existing_state_adequate_vehicle, empirically_contingent).
narrative_ontology:cs_axiom('fc29a394-5b47-4d39-b737-baa86a4f6398', foundational, democratic_mandate_necessity).
narrative_ontology:cs_axiom_status(democratic_mandate_necessity, holdable).
narrative_ontology:cs_axiom_grounding('fc29a394-5b47-4d39-b737-baa86a4f6398', democratic_mandate_necessity, deontological).
narrative_ontology:cs_axiom('fc29a394-5b47-4d39-b737-baa86a4f6398', secondary, cumulative_reform_over_rupture).
narrative_ontology:cs_axiom_status(cumulative_reform_over_rupture, holdable).
narrative_ontology:cs_axiom_grounding('fc29a394-5b47-4d39-b737-baa86a4f6398', cumulative_reform_over_rupture, instrumental).
narrative_ontology:cs_reference_frame('fc29a394-5b47-4d39-b737-baa86a4f6398', manifesto_democratic_minimum_program).
narrative_ontology:cs_drift_state('fc29a394-5b47-4d39-b737-baa86a4f6398', contemporary_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc29a394-5b47-4d39-b737-baa86a4f6398', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_party_leaderships).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_constitutional_institutions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, dual_power_organizers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_opposition_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_road_doctrine).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, constitutional_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets strategy through party congresses, executive committees, and parliamentary groups; controls candidate lists, the party press, and the disciplinary machinery; enforces the electoral-road commitment by censure and expulsion of currents that organize outside it. Collects votes, membership dues, state party subsidies, and ministerial offices. Its authority within the movement rests on being the vehicle of the parliamentary road; abandoning that commitment would dissolve the electoral coalition, funding, and legal standing the organization is built on.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_party_leaderships, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_party_leaderships, beneficiary).

% Negotiates wages and conditions inside collective-bargaining frameworks that the electoral channel helped legislate and defends; enforces industrial peace during campaign periods through no-strike pledges and arbitration agreements. Depends on legal recognition and access to the state's industrial-relations machinery, and gains membership, check-off dues, and bargaining-table standing from the channel's dominance over the movement's strategy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, agenda_setter).

% Votes, pays dues, and staffs the parties' and unions' ranks. Receives material gains when the channel wins — welfare protections, labor law, an expanded franchise — and supplies the legitimacy that sustains the organizations. Its transformative aspirations are deferred to electoral horizons that recede; in crisis moments its strike committees and workplace assemblies are redirected back toward the ballot box. Leaving the organizations means losing the reforms' institutional defenders.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, payer).

% Organizes study circles, strike support, anti-war agitation, and factory newspapers inside the mass organizations. When it contests the electoral-road commitment it faces censure, denial of platform and press, and expulsion under the charge of 'adventurism.' Exiting means abandoning the mass audience, printing infrastructure, and institutional resources built over decades, and starting rival organizations at a fraction of the scale.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    moderate, biographical, constrained, national).

% Emerge in crisis moments — military defeat, occupation, mutiny, general strike — building strike committees, soldiers' and workers' councils, and workplace assemblies that begin administering local life. The mass gradualist organizations decline to defend these organs or vote to dissolve them into parliamentary and constitutional channels. When the crisis passes, the organizers face isolation, employer blacklists, and prosecution without institutional protection.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, dual_power_organizers, payer,
    powerless, immediate, trapped, regional).

% Vanguard, council-communist, and syndicalist currents excluded from party platforms, press boards, and congress agendas. They would contest the deferral of transformation and the crisis conduct of the channel's leaderships; their exclusion is maintained through disciplinary machinery and the 'adventurist' label. They persist by forming separate organizations outside the channel, at the cost of scale and mass reach.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_opposition_factions, excluded,
    moderate, generational, mobile, continental).

% The parliamentary state, courts, and electoral machinery receive mass participation, fiscal legitimacy, and social peace from the channel: disaffection among the class is converted into electoral contention that reproduces the constitutional order. The institutions' stability through the crises of the twentieth century depended on the channel continuing to absorb revolutionary pressure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_constitutional_institutions, beneficiary,
    institutional, generational, constrained, national).

% Comparative scholars of the labor movement who trace the channel's record across countries and crises — war votes, council episodes, welfare settlements, austerity administrations — and attest the founding problem's status and the program-practice gap from outside the benefiting parties.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, movement_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_party_leaderships).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed working-class political capacity into a durable electoral majority: solves the collective-action problem of converting millions of individual votes, dues payments, and strikes into parliamentary power, permanent organizations, and a legally protected strategy that survives electoral cycles.
% TRANSFER_FUNCTION: Moves votes, dues, and militant labor from the working class into the party and union apparatuses; moves the movement's strategic initiative from extra-parliamentary currents to the parliamentary group; and, when majorities are won, moves legal reforms — welfare protections, labor law, franchise expansion — from the state to the class. In crisis moments it moves dual-power experiments out of existence: councils and strike committees are dissolved into electoral and constitutional channels.
% ABSENT_VOICES: Revolutionary militants and council organizers — expelled, censured, or denied platform as 'adventurist' — would contest the electoral-road commitment and the indefinite deferral of transformation; they are outside the congress agendas, press boards, and candidate lists where strategy is set. The unenfranchised of the channel's founding era — women, the colonized, the propertyless without the vote — were absent from the founding settlements that defined the road.
% DISAPPEARANCE_RATIONALE: If the electoral-road commitment vanished overnight, the mass organizations would split immediately along the strategic fault line the channel currently holds together; the liberal constitutional order would lose its absorber of revolutionary pressure, and crisis moments would find no machinery to redirect councils and strike committees into parliamentary channels — the 1918-19 pattern suggests ungoverned extra-parliamentary contention. Party funding, union bargaining frameworks, and the constitutional settlement all depend on the channel's continued operation.
% FOUNDING_PROBLEM: How a growing, legally organized workers' movement in consolidated states with expanding suffrage can convert its numbers into state power without confronting the state's coercive monopoly in street battle — the question posed by the repression following 1848 and 1871 and by the extension of the franchise in Imperial Germany.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the contemporary testimony of the revolutionary opposition (Luxemburg's Reform or Revolution against the revisionist current; the anti-war minority's record against the 1914 war credits) and by independent historiography and comparative political science on social democratic incorporation, which attest both that the founding problem was real and that its resolution remains disputed. The parties' own programs attest the problem only as self-account; the corroboration that counts is oppositional and scholarly, and it disputes the status rather than confirming it.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends at 0.40 because the channel genuinely delivers — welfare settlements, labor law, durable mass organizations — while imposing real asymmetric costs: suppressed currents, crisis capitulations, deferred transformation. Suppression is 0.55 as a raw structural property (unscaled by power or scope, per the framework rule): the enforcement machinery is intra-movement — discipline, expulsions, platform denial, the 'adventurist' charge — real but non-violent at the core case. Theater is 0.35: the retained socialist goal in party programs has become increasingly ritual while practice is administrative, the classic minimum-program/maximum-program gap; the 1989-2008 rise tracks Goodhart drift as the goal survived only ceremonially. Accessibility_collapse is 0.40: alternatives (insurrectionary, councilist) are partly collapsed inside the mass organizations but persist as live positions outside them. Resistance is 0.65: the entire sibling-reading tradition is organized resistance — factional wars, the 1917 USPD split, the 1920 KPD-USPD fusion, continuous oppositional presses. The measurement series shows a cyclical, crisis-driven pattern on one shared time grid: extraction and enforcement spike at crisis moments (1914, when the channel delivered the movement to the national state; 1919, when it demobilized the councils), dip at settlement moments (1945, when reforms actually flowed), and rise when alternatives collapse (1989-2008, when the Soviet model's discrediting left the channel as the only game in town while it administered austerity). The oscillation is partly the extraction mechanism itself: crisis moments are precisely when the cost of deferral is collected. Boltzmann coordination type is resource_allocation: the channel's primary function is aggregating dispersed political capacity into collective electoral power. Coalition note: the payer seats have repeatedly coalesced (the 1918-19 council movement was a coalition of the excluded currents) and were broken each time — the channel's enforcement is strongest exactly where coalition power forms.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently, and the structural data is what carries the divergence. From the leaderships' position the channel is the movement's greatest achievement: legality, durability, the welfare state, an unbroken organizational continuity no insurrectionary current has matched. From the militants' and dual-power organizers' position the same machinery is what delivered August 1914, dissolved the workers' and soldiers' councils in January 1919, and has never converted an electoral majority into transformation of property relations. The class base straddles both experiences: real reforms received in settlement periods, real deferral collected in crisis periods. The engine computes per-seat classifications from power, exit, and directionality; the authored claim does not adjudicate between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The leaderships sit near the beneficiary end (d low): they collect the votes, subsidies, and offices and set the rules that enforce the channel. The union bureaucracies sit low but not zero: they collect standing and dues yet bear discipline obligations to the state's industrial-relations framework. The liberal constitutional institutions are incidental beneficiaries: they collect stability without administering the channel. The revolutionary militants and opposition factions sit near the target end (d high): the channel's enforcement operates directly on them. The dual-power organizers sit at the extreme target position: the channel's crisis-moment conduct acts on them with no protection and no exit. One override is declared: the organized power atom (held in this story only by working_class_electorate) is set to d = 0.5. The derivation from the electorate's beneficiary declaration would understate its cost-bearing; the class base is genuinely dual-positioned — it receives real material coordination and surrenders real transformative capacity, including the crisis-moment redirection of its own councils — so the honest structural relationship is symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both mislabels. A pure-extraction reading — the vanguardist polemical account of gradualism as counter-revolutionary capitulation — erases the genuine collective-action solve: dispersed workers cannot aggregate into state power without durable organizations, and the welfare settlements and labor law were real transfers, not theater. A pure-coordination reading — the parties' own self-account — erases the asymmetric costs: the same machinery that aggregates votes expelled the anti-war left, dissolved the councils, and has administered austerity under a retained socialist vocabulary. The tangled_rope structure holds both: a real coordination function, active enforcement, identifiable beneficiaries, and identifiable payers through the same structure. On the genealogy: the founding problem (converting numbers into power without catastrophic confrontation) is disputed rather than dead — the channel persists while its destination has been quietly redefined (Godesberg-style redefinitions absorbed drift without formally adjudicating the adequacy claim), which is the drift signature the theater series and the delivery omega track. Because founding_problem_status is contested rather than dead, the status-by-verdict mismatch consumer should read no zombie flag here; the open question is routed to the electoral_road_delivery_record omega instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (democratic_gradualism_reading) of the manifesto_revolutionary_method kernel; what would the sibling readings (vanguard_rupture_reading, council_communist_reading) change structurally, and where exactly is the disagreement located?',
    'Author the sibling files and compare their structural deltas: the vanguard reading makes the party the agent and seizure the method (militants become beneficiaries; the liberal institutions become targets); the council reading makes workplace assemblies the vehicle (both party apparatuses and the parliamentary state become redundant). The disagreement is located in the vehicle question: whether existing democratic state institutions are the adequate vehicle of transformation.',
    'A sibling file would author a different ε for the same historical arrangement — the vanguard reading authors the gradualist channel as counter-revolutionary (high ε); this reading authors it as moderate (0.40). Cross-reading comparison is valid only with the reading-indexed ε kept explicit per file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Reading-indexed structure: which kernel, which reading, what siblings would change, where the dispute sits.').

omega_variable(
    electoral_road_delivery_record,
    'Has the electoral road ever transformed property relations in a consolidated capitalist state, or has it only administered and reformed capitalism while the transformation horizon receded?',
    'Comparative analysis of gradualist governments in power (structural reforms achieved versus capitalist continuity maintained): post-1945 welfare settlements, Mitterrand''s 1983 turn, Agenda 2010, Syriza''s capitulation, the Nordic social-democratic settlement''s limits.',
    'If the road has never delivered transformation, the channel''s coordination function is reform-delivery rather than transition, the founding problem reads as dead-with-arrangement-persisting, and classification shifts toward the extraction-heavy types; if partial transitions count, the coordination function stands and the tangled_rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_road_delivery_record, empirical, 'Whether the channel coordinates transition or only administers capitalism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of revolutionary currents structural (expulsion machinery, platform denial, career sanctions inside the mass organizations) or internalized (self-censorship, acceptance of the ''adventurist'' frame as political realism)?',
    'Post-exit trajectory of expelled militants: if self-censorship and the gradualist framing of ''realism'' persist after leaving the channel, the suppression is partially internalized; if expelled currents immediately reorganize openly, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the enforcement with them after exit, and the channel''s hold exceeds what its disciplinary machinery alone explains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the channel''s enforcement.').

omega_variable(
    crisis_extraction_attribution,
    'Are the extraction spikes at 1914 and 1919 features of the channel itself, or of war and counter-revolutionary context that would have constrained any strategy the movement adopted?',
    'Compare crisis-moment conduct of gradualist versus non-gradualist working-class organizations facing the same contexts (the anti-war minority inside the channel, the council movements, the vanguard organizations) — did the channel''s structure cause the capitulation and demobilization, or merely transmit external pressure?',
    'If context-driven, the base trajectory is flatter and the end-state ε overstates the channel''s intrinsic cost-imposition; if structural, the crisis spikes are the channel''s characteristic failure mode and the moderate ε rests on the settlement-period record alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_extraction_attribution, empirical, 'Attribution of the crisis-moment extraction spikes to the channel versus external context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1889, 2019).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1889, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1889, 0.15).
narrative_ontology:measurement_basis(mani_tr_t1889, observed).
narrative_ontology:measurement(mani_tr_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1914, 0.3).
narrative_ontology:measurement_basis(mani_tr_t1914, observed).
narrative_ontology:measurement(mani_tr_t1919, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1919, 0.35).
narrative_ontology:measurement_basis(mani_tr_t1919, observed).
narrative_ontology:measurement(mani_tr_t1933, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1933, 0.4).
narrative_ontology:measurement_basis(mani_tr_t1933, observed).
narrative_ontology:measurement(mani_tr_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(mani_tr_t1945, observed).
narrative_ontology:measurement(mani_tr_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement_basis(mani_tr_t1968, observed).
narrative_ontology:measurement(mani_tr_t1989, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1989, 0.45).
narrative_ontology:measurement_basis(mani_tr_t1989, observed).
narrative_ontology:measurement(mani_tr_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2008, 0.5).
narrative_ontology:measurement_basis(mani_tr_t2008, observed).
narrative_ontology:measurement(mani_tr_t2019, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement_basis(mani_tr_t2019, observed).

% Extraction over time
narrative_ontology:measurement(mani_be_t1889, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1889, 0.3).
narrative_ontology:measurement_basis(mani_be_t1889, observed).
narrative_ontology:measurement(mani_be_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1914, 0.55).
narrative_ontology:measurement_basis(mani_be_t1914, observed).
narrative_ontology:measurement(mani_be_t1919, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1919, 0.62).
narrative_ontology:measurement_basis(mani_be_t1919, observed).
narrative_ontology:measurement(mani_be_t1933, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1933, 0.58).
narrative_ontology:measurement_basis(mani_be_t1933, observed).
narrative_ontology:measurement(mani_be_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement_basis(mani_be_t1945, observed).
narrative_ontology:measurement(mani_be_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1968, 0.48).
narrative_ontology:measurement_basis(mani_be_t1968, observed).
narrative_ontology:measurement(mani_be_t1989, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1989, 0.52).
narrative_ontology:measurement_basis(mani_be_t1989, observed).
narrative_ontology:measurement(mani_be_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement_basis(mani_be_t2008, observed).
narrative_ontology:measurement(mani_be_t2019, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement_basis(mani_be_t2019, observed).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1889, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1889, 0.35).
narrative_ontology:measurement_basis(mani_su_t1889, observed).
narrative_ontology:measurement(mani_su_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1914, 0.6).
narrative_ontology:measurement_basis(mani_su_t1914, observed).
narrative_ontology:measurement(mani_su_t1919, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement_basis(mani_su_t1919, observed).
narrative_ontology:measurement(mani_su_t1933, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement_basis(mani_su_t1933, observed).
narrative_ontology:measurement(mani_su_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(mani_su_t1945, observed).
narrative_ontology:measurement(mani_su_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement_basis(mani_su_t1968, observed).
narrative_ontology:measurement(mani_su_t1989, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1989, 0.55).
narrative_ontology:measurement_basis(mani_su_t1989, observed).
narrative_ontology:measurement(mani_su_t2008, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement_basis(mani_su_t2008, observed).
narrative_ontology:measurement(mani_su_t2019, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement_basis(mani_su_t2019, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'manifesto_revolutionary_method' is a natural-language label covering three structurally distinct constraints, decomposed per the ε-invariance principle: the democratic gradualist channel (this file, ε 0.40 by its own lights), the vanguard-seizure method, and the councilist method. Each has its own ε, beneficiary/victim structure, and enforcement machinery; the colloquial label 'the revolutionary method' conflates them. This file is upstream in institutional resources: the channel's historical dominance shaped the siblings' formation — vanguardism defined itself against the channel's 1914 and 1918-19 conduct, councilism against both the channel and the vanguard. Family members link via affects_constraints; the foreclosure edges among the readings are recorded in each file's cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
