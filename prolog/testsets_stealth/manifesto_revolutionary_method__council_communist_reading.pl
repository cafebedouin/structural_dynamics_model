% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Reading: Federated Workplace Assembly Rule
 *   domain: political philosophy/revolutionary theory/historical materialism
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the manifesto_revolutionary_method
 *   kernel: the council-communist reading, in which revolutionary power is
 *   held by federated workplace assemblies with recallable, wage-paid,
 *   imperative-mandate delegates, replacing both the capitalist state and any
 *   vanguard party. The arrangement has a genuine coordination core — it
 *   solves strike coordination, supply, and defense across workplaces without
 *   building a commanding hierarchy — and it simultaneously dispossesses a
 *   specific stratum: state bureaucrats and party officials whose careers
 *   consist in mediating between the class and power. It operates, in every
 *   historical instantiation, inside a hostile enforcement environment
 *   created by rival organizational forms, which crush or absorb it; that
 *   external hostility is recorded in the resistance metric, the
 *   suppression_requirement series, and the network edges, while the
 *   extractiveness metric keeps its referent fixed on the council arrangement
 *   itself, assessed by the reading's own lights. The claim/metrics gap is
 *   deliberate: the reading CLAIMS a coordination-dominant hybrid, and the
 *   metrics are authored independently as the descriptive record — the engine
 *   computes per-seat classifications from the structural data. KEY AGENTS
 *   (by structural relationship): - autonomous_worker_collectives: Primary
 *   beneficiary and collective agenda-setter (organized/identity_locked) —
 *   the federated assemblies holding power - rank_and_file_workers: Net
 *   beneficiary carrying diffuse participation costs (moderate/constrained) -
 *   state_bureaucrats: Primary target (institutional/arbitrage) — displaced
 *   administrative authority - party_officials: Primary target
 *   (institutional/arbitrage) — displaced party command -
 *   council_political_minorities: Secondary cost-bearer inside the assemblies
 *   (powerless/trapped) - agricultural_laborers: Excluded voice outside the
 *   workplace franchise (powerless/trapped) - labor_movement_historians:
 *   Analytical observer — sees the full structure across waves
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.58).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Reading: Federated Workplace Assembly Rule").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political philosophy/revolutionary theory/historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'b5f37daa-edd4-4f9f-bdff-fc4966cee38e').
narrative_ontology:cs_kernel_codification('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', fixed_text).
narrative_ontology:cs_authority_grounding('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', lineage).
narrative_ontology:cs_interpretation_layer_present('b5f37daa-edd4-4f9f-bdff-fc4966cee38e').
narrative_ontology:cs_reading_relation('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', manifesto_revolutionary_method__democratic_gradualism_reading, influences).
narrative_ontology:cs_axiom('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', foundational, emancipation_through_class_own_organs).
narrative_ontology:cs_axiom_status(emancipation_through_class_own_organs, holdable).
narrative_ontology:cs_axiom_grounding('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', emancipation_through_class_own_organs, deontological).
narrative_ontology:cs_axiom('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', foundational, rejection_of_transitional_state_form).
narrative_ontology:cs_axiom_status(rejection_of_transitional_state_form, holdable).
narrative_ontology:cs_axiom_grounding('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', rejection_of_transitional_state_form, empirically_contingent).
narrative_ontology:cs_reference_frame('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', spontaneous_council_self_emancipation).
narrative_ontology:cs_drift_state('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', contemporary_post_soviet_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b5f37daa-edd4-4f9f-bdff-fc4966cee38e', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, rank_and_file_workers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, council_political_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, rank_and_file_workers).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, workers_self_emancipation_principle).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, imperative_mandate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federated workplace assemblies that hold decision-making power directly: they set production priorities, allocate surplus, organize defense, and administer through delegates who are elected, paid workmen's wages, immediately recallable, and rotate. Their organizational self-conception is fused with the assembly form itself — the class acting through its own organs rather than through any mediating leadership — so abandoning the form would mean conceding the central claim of their rivals. Exit looks like dissolution of the federation back into unorganized strike committees or absorption into a party machine, both of which the tradition treats as capitulation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% Individual workers who vote in assembly, serve stints as recallable delegates, and carry the running costs of direct democracy: meeting hours, delegate service away from production, decision fatigue, and binding majority votes they may have opposed. They cannot opt out of the class situation that makes the assemblies necessary, and during open struggle the workplace is where they already stand.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rank_and_file_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, rank_and_file_workers, payer).

% Career administrators of the existing state whose command positions, salary differentials, and insulation from removal are abolished when assemblies take over administration. Under the commune-form precedent they may continue as technical functionaries at workers' wages under immediate recall, or migrate their expertise to successor administrations, rival regimes, or private employment. What they lose is positional authority, not livelihood as such.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, arbitrage, national).

% Full-time officers of mass revolutionary and social-democratic parties whose organizational purpose — leading the class through a disciplined apparatus — is voided by an arrangement in which the class leads itself. Their career structures, editorial posts, and command roles depend on mediation between the class and power, exactly what the assembly form eliminates. Historically their characteristic response has been to enter the councils in order to subordinate them to party fractions, or to pivot to electoral and union-bureaucratic careers.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, party_officials, payer,
    institutional, generational, arbitrage, continental).

% Dissenting workers and factions inside the assemblies whose positions lose majority votes that then bind everyone, including them. During crises, minorities have faced expulsion, factional proscription, or worse when assemblies hardened under military pressure. Exit means leaving the class organism itself, which reads as defection to the enemy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, council_political_minorities, payer,
    powerless, biographical, trapped, local).

% Field hands, seasonal workers, and rural laborers whose labor falls outside the workplace franchise on which assembly representation is built. They would claim voice in a polity that claims to speak for the producing class as a whole, but they hold no workshop in which to seat a delegate, and the territorial adjuncts historically proposed for them never achieved parity with workplace seats.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, agricultural_laborers, excluded,
    powerless, generational, trapped, regional).

% Scholars of the 1905 and 1917 soviets, the German Revolution councils, the Spanish collectives, and the Hungarian workers' councils of 1956, who reconstruct from archives and testimony how the assembly form actually operated, when delegates stayed recallable, and under what conditions the form was captured or crushed. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, labor_movement_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production, distribution, and defense across many workplaces without hierarchical delegation: federated assemblies aggregate local decisions upward through mandated delegates while retaining recall and instruction downward, solving the collective-action problems of strike coordination, supply, and armed defense that no single workplace can solve alone, and solving the guardian problem by rotating administrative labor instead of professionalizing it.
% TRANSFER_FUNCTION: Moves decision-making authority and surplus-allocation power from state ministries and party executive committees to workplace assemblies and their federations; moves administrative labor onto rotating, wage-paid, recallable delegates; and strips positional rents (command authority, salary differentials, irremovability) from officialdom of both the state and the party apparatus.
% ABSENT_VOICES: Agricultural laborers, domestic workers, and the unemployed — everyone whose labor lies outside the workplace franchise — would object that a polity of workplace assemblies speaks for production sites, not for the producing class; they sit outside the assembly door with no seat of their own. Political minorities inside the assemblies are present but outvoted, and historically have been silenced further in crises. Neither group appears in the founding texts' enumeration of the ruling organs except as problems to be annexed later.
% DISAPPEARANCE_RATIONALE: If the assembly federation vanished overnight, the coordination it performs would not evaporate — it would be re-seized by whichever rival organizational form was positioned to fill the vacuum: a party apparatus claiming to act on the class's behalf, or the reconstructed ministries of the old state. Every historical instance shows the vacuum being filled within months, with the delegates' recall rights among the first casualties.
% FOUNDING_PROBLEM: Built to solve the problem the Paris Commune posed and the Manifesto's method left open: how the working class can exercise power directly, without reproducing the state form and without a professional leadership stratum congealing above it — how to win and hold a revolution without the revolutionaries becoming the new governors.
% FOUNDING_PROBLEM_CORROBORATION: The problem's historical reality is corroborated from outside the benefiting parties: labor historiography of the 1905 Petersburg and 1917 Petrograd soviets documents councils arising from strike committees before any party directed them; German Revolution council archives and participant testimony from 1918-19 attest the same spontaneous origin; and hostile witnesses concede it — Bolshevik leaders' own 1917 slogans acknowledged the soviets' independent emergence. Whether the problem remains LIVE, however, is attested only within the council-communist tradition itself; rival readings and most academic observers hold either that the historical window closed or that the problem was solved by other means, and no source outside the tradition currently argues the assembly form is the live answer.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.25) because the arrangement's transfers run overwhelmingly toward its participants: delegates are unpaid-or-wage-paid, recallable, and rotated, so the classic extraction channel — permanent office — is structurally closed. The residual 0.25 is real, not decorative: assembly majorities bind minorities, participation costs fall on all members, and the commune-form conversion of officials into recallable functionaries is a genuine imposition on the displaced. Suppression (0.58) is authored as the constraint's own coercive force — imperative-mandate enforcement, anti-caucus discipline under military pressure, majority rule — and is deliberately NOT loaded with the crushing external suppression the arrangement suffers, which belongs to resistance (0.78) and to the suppression_decomposition omega; suppression is a raw structural property unscaled by power or scope. Theater is low-moderate (0.22): where the form runs free it is substantively participatory; the high-theater episodes in the series are the form running captive. Accessibility collapse is low (0.35) because the rival methods remain conceptually live — this reading forecloses nothing in public discourse. The temporal series run on one shared eleven-point grid, and they trace TWO FULL CYCLES of rise, capture-or-crush, dormancy, and revival (circa 1905-1930 and circa 1956-1980 in the mapped decades): the oscillation's driver is external — rival-reading enforcement capacity and crisis waves — not intermittent reinforcement by the arrangement itself, and the base_properties scalars are measured at t=100, the late-dormancy/early-revival phase. Receipt surface: gain_flow is authored 'diffuse' as a checked affirmative — the nearest candidate receiver, autonomous_worker_collectives, is coextensive with the governed population itself, so no seat captures the gains separately from those who bear the costs. Fixing cost is 'prohibitive': for the only seat that could change the arrangement (the collectives), dismantling the federation mid-struggle leaves no coordinator at all, and every historical attempt to rebuild direct democracy after bureaucratic substitution has failed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the beneficiary seats (collectives, rank-and-file), the arrangement computes near-rope: genuine coordination, negligible personal extraction, identity-fused commitment. From the displaced-officialdom seats, the same structure computes as enforced expropriation of positional authority — they experience the recall machinery as the instrument of their dispossession. From the minority seat, it computes as majoritarian compulsion with no exit. The engine derives this divergence from power, exit, and directionality; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: the collectives subsidize the arrangement and are identity-locked into it; rank-and-file workers sit near symmetric (genuine benefit, diffuse costs). Targets map high: state bureaucrats and party officials are the people the arrangement exists to strip of command authority. The derivation chain would get this WRONG for the officialdom seats without help: both hold arbitrage-grade exit (skills and networks migrate to successor administrations, rival regimes, electoral and union careers), and mobility damps derived directionality toward the beneficiary end — yet they are unambiguously the dispossessed, not the subsidized. A single override on the institutional power atom (d=0.82) corrects both seats at once; no other stakeholder holds that atom, so the override touches nothing else. Council political minorities derive high directionality from trapped exit and payer position; agricultural laborers are excluded rather than governed, feeding the consensus-provenance check rather than the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters doubly here because both mislabelings are politically loaded. Reading the arrangement as pure extraction (because it names victims and needs enforcement) erases the coordination core that every historical soviet actually performed — strike logistics, supply, defense — and would classify the most participatory form in the corpus as a protection racket. Reading it as pure rope (because participants are net beneficiaries) erases the real imposition on officialdom and on internal minorities, and would launder the displacement of a whole stratum as costless. The tangled_rope claim holds both truths: coordination for the many, dispossession of the few, enforced. On obsolescence: the founding problem (holding power without breeding a new governing stratum) is contested rather than dead — the tradition says every subsequent revolution re-poses it; rivals say history answered it otherwise. Because status is contested and the disappearance verdict is world_rearranges, the mismatch consumer finds no dead-mandate signature. The honest wrinkle is inter-wave persistence: between revivals the doctrine survives as maintained commitment with thinning functional attachment, which is the profile the diffuse-plus-prohibitive receipt cell flags; the interwave_lifecycle_phase omega carries that ambiguity openly rather than letting the endpoint scalars hide it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the manifesto_revolutionary_method kernel; would collapsing the three readings into one story produce a coherent constraint at all?',
    'Keep the readings as separate files with separate epsilon values, beneficiary sets, and victim sets, linked by network.affects_constraints; refuse any merged story whose epsilon averages across incompatible arrangements.',
    'A merged story would fabricate a hybrid arrangement nobody holds, average a low-epsilon participatory form with high-epsilon party-state and electoral forms, and destroy the per-seat divergence the corpus exists to measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexicality guard: classification here applies only to the council-communist reading, not to the kernel label.').

omega_variable(
    suppression_decomposition,
    'How much of the suppression surrounding this arrangement is the constraint''s own coercive force (imperative-mandate enforcement, assembly discipline under arms) versus coercive pressure imposed ON it by rival organizational forms?',
    'Compare council-form operation across environments differing in rival-reading strength — e.g., the Spanish collectives of 1936-37 under weak party penetration versus the Russian soviets of 1918-21 under consolidating party control — and decompose observed coercion by source.',
    'If most measured suppression is external, the arrangement''s intrinsic suppression drops well below 0.58 and its internal profile approaches pure coordination; if internal discipline coercion dominates, the hybrid classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_decomposition, empirical, 'Own-coercion versus suffered-coercion decomposition of the suppression scalar.').

omega_variable(
    displaced_officialdom_victim_status,
    'Are state bureaucrats and party officials genuine victims of extraction, or merely losers of previously-held extraction rights?',
    'Assess whether the arrangement imposes ongoing costs on them beyond position-loss — proscription, exile, expropriation records vary sharply across the 1917-1921, 1918-1923, and 1956 episodes — versus simple conversion to recallable functionary status.',
    'If the cost is mere rent-loss, effective extraction toward them approaches zero and the arrangement trends rope; if active persecution recurs across episodes, the extraction component is durable and the hybrid classification solidifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_officialdom_victim_status, conceptual, 'Whether dispossession of officialdom counts as extraction or as de-extraction.').

omega_variable(
    interwave_lifecycle_phase,
    'Do the endpoint metrics describe the arrangement in-wave (functioning coordination) or inter-wave (doctrine maintained with thinning functional attachment)?',
    'Date-stamp assessments to wave phases using the measurement series: t=100 sits at late dormancy approaching revival; re-measure during an active wave before certifying steady-state properties.',
    'An inter-wave assessment trends toward inertial persistence with performative maintenance; an in-wave assessment trends toward functioning coordination — the same structure certifies differently by phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interwave_lifecycle_phase, empirical, 'Phase-dependence of the arrangement''s operational profile across its episodic history.').

omega_variable(
    workplace_franchise_exclusion,
    'Does the workplace-based franchise systematically exclude non-workplace populations (agricultural laborers, domestic workers, the unemployed) deeply enough that the arrangement extracts voice from them?',
    'Compare historical council composition against the laboring population in each episode (1905, 1917-21, 1918-23, 1936-37, 1956), and track whether territorial adjuncts ever achieved parity with workplace seats.',
    'If exclusion is systematic and unremedied, the victim set expands beyond officialdom and internal minorities, pushing extractiveness materially above 0.25 and strengthening the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(workplace_franchise_exclusion, empirical, 'Franchise-boundary extraction: voice taken from those outside the workplace seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(council_communist_reading_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(council_communist_reading_tr_t10, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(council_communist_reading_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(council_communist_reading_tr_t30, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(council_communist_reading_tr_t40, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(council_communist_reading_tr_t50, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(council_communist_reading_tr_t60, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement(council_communist_reading_tr_t70, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 70, 0.2).
narrative_ontology:measurement(council_communist_reading_tr_t80, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(council_communist_reading_tr_t90, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 90, 0.3).
narrative_ontology:measurement(council_communist_reading_tr_t100, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(council_communist_reading_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(council_communist_reading_be_t10, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(council_communist_reading_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(council_communist_reading_be_t30, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(council_communist_reading_be_t40, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(council_communist_reading_be_t50, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(council_communist_reading_be_t60, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(council_communist_reading_be_t70, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 70, 0.24).
narrative_ontology:measurement(council_communist_reading_be_t80, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(council_communist_reading_be_t90, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 90, 0.26).
narrative_ontology:measurement(council_communist_reading_be_t100, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(council_communist_reading_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(council_communist_reading_su_t10, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(council_communist_reading_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(council_communist_reading_su_t30, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(council_communist_reading_su_t40, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(council_communist_reading_su_t50, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(council_communist_reading_su_t60, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(council_communist_reading_su_t70, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 70, 0.68).
narrative_ontology:measurement(council_communist_reading_su_t80, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(council_communist_reading_su_t90, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 90, 0.55).
narrative_ontology:measurement(council_communist_reading_su_t100, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the manifesto_revolutionary_method kernel, per the epsilon-invariance principle: the colloquial label 'the revolutionary method' covers three structurally distinct arrangements — federated assembly rule (this file, epsilon 0.25, victims: displaced officialdom and internal minorities), party-guided transitional state (epsilon substantially higher; victims include the class itself under party discipline), and electoral-incremental socialism (epsilon depends on capital-flight and institutional-capture dynamics; victims diffuse). The council reading is upstream of neither sibling empirically — the historical sequence ran councils-first (1905/1917), then party-state consolidation, then parliamentary absorption — but each sibling's legitimacy claims cite or suppress the council record, so contamination propagates along all three edges. Each file carries its own stable epsilon; no observable-switching parameter is admitted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__council_communist_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
