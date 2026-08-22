% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default: Hybrid Amnesia Reading
 *   domain: political_economy/ideology_studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'market_as_natural_default': the hybrid amnesia reading. The constraint
 *   describes how the naturalization of markets emerged through a two-stage
 *   process: (1) a period of genuine institutional forgetting and
 *   carrier-community fragmentation (1930s-1970s) when non-market
 *   coordination systems were materially suppressed by war, cold war, and
 *   post-war institutional reconstruction; and (2) a subsequent period
 *   (1980s-present) during which incumbent beneficiaries (multinational
 *   corporations, financial institutions, neoclassical establishment)
 *   inherited the amnesia and actively weaponized it as ideology, defending
 *   market naturalism against emerging critique through sophisticated
 *   communications and institutional control of credentialing. The constraint
 *   coordinates a real function (price signals allocate resources
 *   efficiently) WHILE extracting the framing power to define what counts as
 *   possible, natural, or serious. Extractiveness rises over the interval
 *   (0.20 → 0.45) as beneficiaries inherit and deepen the amnesia-based
 *   defense; theater rises faster (0.25 → 0.68) as the enforcement becomes
 *   less about coercive suppression of alternatives and more about
 *   sophisticated narrative management and delegitimization. The reading is
 *   distinguished from siblings by its causal story: neither pure lapsed
 *   memory (lapsed_alternative_reading) nor beneficiary-orchestrated from
 *   inception (beneficiary_maintained_reading), but a hybrid where inherited
 *   amnesia enables later capture.
 *
 * KEY AGENTS:
 *   - Incumbent Market Actors: multinational corporations, financial institutions, capital-intensive enterprises; beneficiaries of the market-natural framing; inherit rather than create the amnesia; d ≈ 0.2 (beneficiary end)
 *   - Neoclassical Economic Establishment: economists, central banks, policy advisors, international institutions; agenda-setter for what counts as legitimate knowledge; inherited much amnesia, now actively defending it; d ≈ 0.15 (beneficiary end)
 *   - Cooperative Movement Practitioners: worker cooperatives, credit unions, mutual organizations; victims of delegitimization flowing from amnesia; d ≈ 0.9 (target end)
 *   - Labor Movements: trade unions, labor organizations; strategically weakened by loss of alternative repertoire; d ≈ 0.8 (target end)
 *   - Ordinary Citizens: both benefit from market coordination and bear costs of externalities; constrained imagination of alternatives by amnesia; d ≈ 0.5 (symmetric)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.72).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default: Hybrid Amnesia Reading").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'a69abf61-8fd3-4cf3-953c-8dd8c81e329d').
narrative_ontology:cs_kernel_codification('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', distributed).
narrative_ontology:cs_authority_grounding('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', extraction).
narrative_ontology:cs_interpretation_layer_present('a69abf61-8fd3-4cf3-953c-8dd8c81e329d').
narrative_ontology:cs_reading_relation('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', foundational, amnesia_enables_beneficiary_capture).
narrative_ontology:cs_axiom_status(amnesia_enables_beneficiary_capture, holdable).
narrative_ontology:cs_axiom_grounding('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', amnesia_enables_beneficiary_capture, empirically_contingent).
narrative_ontology:cs_axiom('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', foundational, two_stage_temporal_dynamic).
narrative_ontology:cs_axiom_status(two_stage_temporal_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', two_stage_temporal_dynamic, empirically_contingent).
narrative_ontology:cs_reference_frame('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', market_coordination_as_deliberate_choice).
narrative_ontology:cs_drift_state('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', contemporary_naturalized_inevitability, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a69abf61-8fd3-4cf3-953c-8dd8c81e329d', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_actors).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economic_establishment).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_memory_of_alternatives).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, non_market_coordination_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, ordinary_citizens_and_workers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, cooperative_movement_practitioners).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor_movements_and_union_traditions).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, ordinary_citizens_and_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large corporations, financial institutions, and capital-intensive enterprises that consolidated dominance during the post-war period. They benefit from the narrative that market allocation is natural and inevitable because it insulates their position from challenge based on alternatives. They have sophisticated communications infrastructure to maintain this narrative. They do not directly run the amnesia but inherit and weaponize the pre-existing forgetting.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_actors, beneficiary,
    institutional, generational, arbitrage, global).

% Academic economists, central banks, policy advisors, and international institutions (IMF, World Bank, OECD) that set the intellectual framework for policy discussion. They institutionalized the market-as-natural framing through textbooks, training, and policy prescription. Their power lies in defining what counts as legitimate economic knowledge and what alternatives are even discussable. They actively defend against rival frameworks but inherited much of the original amnesia rather than creating it deliberately.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economic_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Worker cooperatives, credit unions, community land trusts, mutual aid networks, and other non-market coordination institutions that persist despite amnesia. They operate under the stigma of being 'marginal' or 'inefficient' because their existence contradicts the naturalized market narrative. They pay the cost of reduced policy support, reduced access to capital, and delegitimization in public discourse.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, cooperative_movement_practitioners, payer,
    moderate, biographical, constrained, regional).

% Trade unions and labor organizations that historically articulated alternatives to market wage-setting and commodity production. The amnesia about alternatives weakens their capacity to articulate a coherent counter-vision; they fight defensive battles over wages and conditions rather than advancing structural alternatives. The forgetting of the cooperative and mutual-aid traditions they once embedded in is a material loss of strategic repertoire.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor_movements_and_union_traditions, payer,
    organized, generational, constrained, national).

% Scholars, activists, and public voices who attempt to recover and articulate non-market alternatives (degrowth advocates, commoners, anarchists, heterodox economists, indigenous-knowledge practitioners). They are systematically marginalized from mainstream policy discussion, academic credentialing, and media platforms. Their exclusion is maintained partly through the amnesia: if alternatives are 'not serious' because they are 'not historically viable,' they do not merit discussion.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_intellectuals_and_dissidents, excluded,
    moderate, biographical, constrained, national).

% Individuals embedded in market-coordinated economies who both benefit from some market efficiencies and bear the costs of market externalities (precarity, atomization, commodification). The amnesia constrains their ability to imagine collective alternatives; they experience market coordination as inevitable rather than as one choice among several. This constrains their political agency.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, ordinary_citizens_and_workers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, ordinary_citizens_and_workers, beneficiary).

% Archivists, historians, and oral-tradition bearers who maintain records of past non-market coordination systems. They document what was lost and how. They are largely outside the institutional power structure but their work is the source material for recovering amnesia.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, historical_archive_and_memory_keepers, observer,
    powerless, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economic_establishment).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Markets coordinate resource allocation through price signals and voluntary exchange; decentralized decision-making without central planner; efficiency gains from specialization and voluntary trade. This function is real and addresses a genuine coordination problem: how to allocate scarce resources across many independent agents.
% TRANSFER_FUNCTION: Transfers the framing power — the ability to define what counts as 'natural' economic organization — from democratic deliberation and social memory to the institutional apparatus that maintains market ideology. Transfers legitimacy from 'chosen because it works for most people' to 'inevitable because this is how nature works.' Transfers the agenda-setting power over alternatives from lived communities to economists and policy elites.
% ABSENT_VOICES: Communities and traditions that practiced non-market coordination during the period of amnesia (1930s-1970s) are dead or fragmented; their knowledge carriers are marginalized from policy discussion; indigenous and non-Western coordination systems are classified as 'not serious' by the neoclassical framework; future generations born into the amnesia have no experiential memory of alternatives to inherit.
% DISAPPEARANCE_RATIONALE: If the market-as-natural framing disappeared overnight, policy discourse would immediately reopen questions about what coordination mechanisms to use for what purposes. Alternative institutions (cooperatives, mutual aid, commons, planning) would become discussable as strategic choices rather than marginal oddities. The economy would not collapse but its legitimacy structure and the range of political options would shift fundamentally. Incumbent beneficiaries would lose the ability to defend their position on 'market necessity' grounds and would have to argue on grounds of actual performance and distributional fairness.
% FOUNDING_PROBLEM: Early 20th-century capitalist markets faced repeated crises (financial collapse, labor unrest, colonial competition, resource depletion). Alternative coordination systems were actively practiced (cooperative movements, socialist experimentation, guild traditions, commons management) and politically contested. The Great Depression and its aftermath saw these alternatives marginalizing through a combination of: (a) genuine defeat of left movements by state repression and WW2; (b) post-war institutional reconstruction that privileged market frameworks in newly-rebuilt institutions (GATT, IMF, World Bank); (c) the accelerating professionalization of economics as a discipline with a narrow theoretical core; (d) cold-war ideology that delegitimized non-market talk; and (e) successful performance of Keynesian-managed markets in post-war growth, which created genuine prosperity that made the memory of alternatives less urgent.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Polanyi, Hirschman, Streeck, Stiglitz) document the active suppression and marginalization of alternatives; oral histories from cooperative movements and labor unions confirm the loss of institutional memory; archival records show that non-market coordination schemes were deliberately written out of official histories and policy frameworks post-1945. Corroboration comes from OUTSIDE the beneficiary establishment: the beneficiaries maintain that the founding problem (how to coordinate without markets) was 'never really solved' and therefore markets 'proved best'; historians outside the economic mainstream document that alternatives were real, functional, and actively erased. Cooperative movements that still operate show functional non-market coordination at scale. The corroboration is strongest from archival and sociological work, weakest from within the neoclassical economic establishment, which has an institutional interest in maintaining that no viable alternatives exist.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45 at interval end, up from 0.20 at start) reflects the increasing capture of framing power by beneficiaries: at interval start (1930), the constraint is still structurally a genuine coordination response to a real problem; by interval end (2026), the same coordination function persists but is wrapped in a naturalized-inevitability narrative that delegitimizes alternatives and insulates beneficiaries from challenge. Suppression (0.72) is substantially higher than would be needed for the coordination function alone (which is real) because it must actively maintain the amnesia: it operates through curriculum control, professional credentialing (heterodox economists struggle to get jobs), media marginalization, and delegitimization of alternative practitioners. Theater (0.68) is the highest metric because the enforcement is increasingly performative: much of the 'defense of markets' is rhetorical (TED talks about market efficiency, op-eds about 'what socialism got wrong,' economic textbooks that never mention cooperatives) rather than material suppression. Accessibility collapse (0.81) is high because the amnesia has become so complete that alternatives are not just suppressed but *not thinkable* within mainstream policy discourse—they have collapsed out of the possibility space. Resistance (0.42) is lower than suppression because the target communities (cooperatives, labor, dissidents) are fragmented, underfunded, and operating from a knowledge deficit created by the amnesia itself. The rise in theater_ratio over the interval (0.25 → 0.68) marks the shift from material suppression (1930-1980: closing cooperatives, banning labor organizing, excluding non-market talk from institutions) to narrative suppression (1980-present: delegitimizing alternatives through 'scientific' economic theory, managing media framing, controlling credentialing). The coercion grid shows that suppression rises across all four levels but particularly at the organizational and structural levels (institutional enforcement), while resistance falls most sharply at the structural level (system-level alternatives are not articulated), suggesting the constraint operates by controlling what is discussable at the policy level rather than crushing dissent at the individual level.
 *
 * PERSPECTIVAL GAP:
 *   A neoclassical economist or central banker (agenda-setter seat, d ≈ 0.15) would experience this constraint as 'what we discovered to be true' — they see the coordination function working, see prosperity resulting, and read market naturalism as scientific truth rather than as inherited amnesia weaponized by beneficiaries. A cooperative practitioner (payer seat, d ≈ 0.9) experiences the same structure as 'we are rendered invisible and illegitimate' — they run successful non-market coordination but are told it 'doesn't scale' or is 'romantic nostalgia' because the amnesia has erased the historical record that it did scale and does work. A labor organizer (payer, d ≈ 0.8) experiences the constraint as loss of strategic vocabulary: they cannot articulate 'this is extractive because we could organize production differently' because the amnesia makes 'organizing production differently' unthinkable within mainstream politics. An incumbent corporation executive (beneficiary, d ≈ 0.2) experiences the constraint as good luck: 'the market happens to naturally favor us because we're efficient,' unaware or unwilling to acknowledge that the amnesia about alternatives has foreclosed political pressure for redistribution or regulation. The engine computes these divergent experiences from the structural data: different power levels, exit options, and directionality vectors produce different type classifications for the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent market actors and the neoclassical establishment occupy the beneficiary end of the directionality spectrum (d ≈ 0.15-0.2) because they extract the framing power: their preferred arrangement (markets) is naturalized as inevitable, which insulates them from political pressure and delegitimizes rivals. Cooperatives and labor movements occupy the target end (d ≈ 0.8-0.9) because they bear the cost of delegitimization: their institutions are starved of capital and policy support, their knowledge is marginalized from curricula, and their practitioners are excluded from policy discussion. Ordinary citizens sit near the midpoint (d ≈ 0.5) because they both benefit from genuine market coordination and bear costs from market externalities and the foreclosure of alternatives; their directionality is symmetric but unstable — depending on which externality or alternative is salient, they could shift either way. The beneficiary/victim declarations establish the asymmetry: beneficiaries = incumbent_market_actors + neoclassical_economic_establishment (they benefit from the naturalization); victims = public_memory_of_alternatives + non_market_coordination_practitioners (the constraint erases and delegitimizes them). This asymmetry would not be apparent from the coordination function alone (which is genuinely beneficial for coordination), but becomes apparent when we ask: 'who benefits from the framing that this is natural and inevitable?' Answer: those who profit from this particular coordination regime and want to prevent redistribution or structural change.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: 'how do we allocate resources without markets?' has been answered affirmatively in multiple contexts (socialist economies, cooperatives, commons, planning systems) and many of these answers worked at scale and for extended periods. But the constraint persists, now maintained mostly by amnesia and beneficiary weaponization rather than by need. The disappearance verdict is contested: if the market-natural framing vanished, would the world rearrange? Yes — policy discourse would reopen alternatives, institutional innovation would accelerate, political alignments would shift. But incumbent beneficiaries have a large stake in that NOT happening, so they maintain the constraint even though its founding problem is solved. This is the classic mandatrophy signature: founding function (coordinate without central planner) was real and is still useful, but founding problem (we have no other way to do this) is dead, yet the constraint persists because beneficiaries now use it not for its original function but to defend their position. The theater rise (0.25 → 0.68) is diagnostic: if the constraint were still about coordination, we would expect theater to stay low (theater measures performative vs. functional activity); the rise in theater indicates the constraint's function has shifted from 'coordinate' to 'defend the natural-market framing,' which requires more narrative work and less material coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_forgetting_vs_deliberate_erasure,
    'What proportion of the 1930-1970 amnesia about non-market alternatives was genuine forgetting (institutional fragmentation, loss of carrier communities, non-recording) versus deliberate suppression (active rewriting of history, exclusion from curricula, blacklisting)?',
    'Archival analysis of what was intentionally expunged from institutional records; oral histories from surviving members of suppressed movements; comparison with regions where suppression was weaker (Yugoslavia, Scandinavia) to identify the differential trajectory.',
    'If primarily genuine forgetting, the constraint is closer to natural attrition of memory; if substantially deliberate erasure, the early period shows higher active enforcement and the constraint is closer to designed snare from the start. This affects how we interpret the two-stage reading (initial forgetting, later weaponization) versus a single coordinated program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_forgetting_vs_deliberate_erasure, empirical, 'The balance between passive memory loss and active suppression in the amnesia origin.').

omega_variable(
    reversibility_of_amnesia,
    'Is the amnesia about market alternatives structurally reversible through education, archival recovery, and community reconnection, or has the generational loss created irreversible epistemic closure?',
    'Examine recovery attempts: do communities that recover archived knowledge and reconnect with historical alternatives show ability to articulate coherent alternatives? Can new generations be trained in non-market coordination practice? What is the learning curve?',
    'If reversible, the constraint is contingent on continuous suppression and could be undone through counter-memory work. If irreversible, the 1930-1970 amnesia has calcified into structural illiteracy and the constraint is now a mountain-like feature of the epistemic landscape, not a maintained extraction device. This affects remediation strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_amnesia, empirical, 'Whether the historical amnesia is a reversible constraint or an irreversible epistemic feature.').

omega_variable(
    beneficiary_agency_in_amnesia_maintenance,
    'How much of the current theater and suppression in this constraint is actively maintained by incumbent beneficiaries, versus inherited institutional inertia from the original amnesia period?',
    'Examine funding flows, personnel flows, and public statements of key institutional actors (central banks, economic departments, think tanks). Do they actively fund amnesia-maintenance, or do they passively benefit from it? What happens when dissident voices gain resources and platform?',
    'If high active agency: the constraint is a live extractive device held by beneficiaries and remediation requires changing incentives for powerful actors. If low active agency and mostly inherited inertia: the constraint is more piton-like and could be disrupted through counter-memory work and reframing without directly confronting beneficiary interests. This distinguishes seat divergence (payer sees extraction, beneficiary sees accidental-inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_in_amnesia_maintenance, conceptual, 'The degree to which beneficiaries actively maintain the amnesia versus passively inherit it.').

omega_variable(
    reading_boundary_amnesia_vs_active_defense,
    'Does this hybrid_amnesia_reading coherently hold that initial lapsed closure (1930s-1970s genuine forgetting) created the conditions for beneficiary capture, and that beneficiaries then weaponized pre-existing amnesia starting ~1980s? Or is the distinction between ''amnesia-enabled-capture'' and ''beneficiary-maintained-rationalization'' a false separation?',
    'Trace the timeline of institutional defenses: when did think tanks, economic associations, and policy institutions begin ACTIVELY publishing market-naturalization content as a political program? Was it post-1980, post-1990, post-2008? If post-80, the two-stage reading holds; if earlier, the distinction dissolves.',
    'This omega names the conceptual boundary of THIS reading against its sibling. If the boundary holds (genuine two stages), the hybrid_amnesia reading is coherent and distinct from both lapsed_alternative_reading (no beneficiary agency) and beneficiary_maintained_reading (beneficiary agency from the start). If the boundary collapses, this reading merges with beneficiary_maintained_reading. This is an omega documenting the reading''s internal coherence against the sibling set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_amnesia_vs_active_defense, conceptual, 'The structural coherence of the two-stage model that distinguishes this reading from siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.45).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(mark_tr_t2008, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2008, 0.65).
narrative_ontology:measurement(mark_tr_t2026, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(mark_be_t2008, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(mark_be_t2026, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.4).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(mark_su_t2008, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(mark_su_t2026, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2026, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1930, tn=2026
narrative_ontology:measurement(mark_grid_01, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(class), 1930, 0.55).
narrative_ontology:measurement(mark_grid_02, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(class), 2026, 0.75).
narrative_ontology:measurement(mark_grid_03, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(individual), 1930, 0.4).
narrative_ontology:measurement(mark_grid_04, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(individual), 2026, 0.7).
narrative_ontology:measurement(mark_grid_05, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(organizational), 1930, 0.45).
narrative_ontology:measurement(mark_grid_06, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(organizational), 2026, 0.78).
narrative_ontology:measurement(mark_grid_07, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(structural), 1930, 0.35).
narrative_ontology:measurement(mark_grid_08, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(structural), 2026, 0.81).
narrative_ontology:measurement(mark_grid_09, market_as_natural_default__hybrid_amnesia_reading, resistance(class), 1930, 0.55).
narrative_ontology:measurement(mark_grid_10, market_as_natural_default__hybrid_amnesia_reading, resistance(class), 2026, 0.4).
narrative_ontology:measurement(mark_grid_11, market_as_natural_default__hybrid_amnesia_reading, resistance(individual), 1930, 0.6).
narrative_ontology:measurement(mark_grid_12, market_as_natural_default__hybrid_amnesia_reading, resistance(individual), 2026, 0.45).
narrative_ontology:measurement(mark_grid_13, market_as_natural_default__hybrid_amnesia_reading, resistance(organizational), 1930, 0.58).
narrative_ontology:measurement(mark_grid_14, market_as_natural_default__hybrid_amnesia_reading, resistance(organizational), 2026, 0.32).
narrative_ontology:measurement(mark_grid_15, market_as_natural_default__hybrid_amnesia_reading, resistance(structural), 1930, 0.65).
narrative_ontology:measurement(mark_grid_16, market_as_natural_default__hybrid_amnesia_reading, resistance(structural), 2026, 0.38).
narrative_ontology:measurement(mark_grid_17, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(class), 1930, 0.45).
narrative_ontology:measurement(mark_grid_18, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(class), 2026, 0.65).
narrative_ontology:measurement(mark_grid_19, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(individual), 1930, 0.35).
narrative_ontology:measurement(mark_grid_20, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(individual), 2026, 0.58).
narrative_ontology:measurement(mark_grid_21, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(organizational), 1930, 0.38).
narrative_ontology:measurement(mark_grid_22, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(organizational), 2026, 0.72).
narrative_ontology:measurement(mark_grid_23, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(structural), 1930, 0.3).
narrative_ontology:measurement(mark_grid_24, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(structural), 2026, 0.68).
narrative_ontology:measurement(mark_grid_25, market_as_natural_default__hybrid_amnesia_reading, suppression(class), 1930, 0.48).
narrative_ontology:measurement(mark_grid_26, market_as_natural_default__hybrid_amnesia_reading, suppression(class), 2026, 0.72).
narrative_ontology:measurement(mark_grid_27, market_as_natural_default__hybrid_amnesia_reading, suppression(individual), 1930, 0.4).
narrative_ontology:measurement(mark_grid_28, market_as_natural_default__hybrid_amnesia_reading, suppression(individual), 2026, 0.68).
narrative_ontology:measurement(mark_grid_29, market_as_natural_default__hybrid_amnesia_reading, suppression(organizational), 1930, 0.42).
narrative_ontology:measurement(mark_grid_30, market_as_natural_default__hybrid_amnesia_reading, suppression(organizational), 2026, 0.75).
narrative_ontology:measurement(mark_grid_31, market_as_natural_default__hybrid_amnesia_reading, suppression(structural), 1930, 0.38).
narrative_ontology:measurement(mark_grid_32, market_as_natural_default__hybrid_amnesia_reading, suppression(structural), 2026, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'market_as_natural_default.' The kernel asks: why is market coordination naturalized as the default allocation mechanism, and who benefits from that framing? Three distinct readings produce three constraint stories: (1) hybrid_amnesia_reading (this file) — initial genuine forgetting (1930s-1970s) created epistemic conditions; beneficiary capture and weaponization came later (1980s-present); extractiveness rises as amnesia is weaponized (ε 0.20 → 0.45); (2) lapsed_alternative_reading — pure historical forgetting of alternatives with no deliberate orchestration; beneficiary advantage is incidental; (3) beneficiary_maintained_reading — beneficiary agency visible from the post-war institutional order onward; active defense against alternatives as a political program from the start. Each reading produces a different ε value, different stakeholder structure, and different remediation logic. The three stories form a kernel family: sibling relationships are recorded in cs_structure.reading_relations of each file; temporal sequence affects each other through network dependencies (amnesia-outcome influences both 'lapsed' and 'beneficiary-maintained' readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
