% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Socialism via Democratic Electoral Gradualism
 *   domain: political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-gradualist reading of
 *   contested revolutionary method: the claim that socialism is achievable
 *   through electoral majorities and gradual institutional reform, exercised
 *   through existing democratic structures. The constraint operates by
 *   channeling working-class political energy into parties and unions that
 *   negotiate within capitalist-written legal frameworks, while marginalizing
 *   and suppressing alternatives (revolutionary parties, syndicalist
 *   movements, council communists) as incompatible with democratic procedure
 *   and political realism. The reading shares a kernel (the founding
 *   revolutionary problem: how to accumulate working-class power) with the
 *   vanguard-rupture reading and the council-communist reading, but produces
 *   a structurally distinct constraint with different beneficiaries, victims,
 *   and suppression mechanisms. The constraint is CLAIMED as rope (genuine
 *   coordination of dispersed workers) but authored metrics show substantial
 *   suppression and rising theater (enforcement machinery growing even as
 *   extraction plateaus), suggesting the coordination function may be
 *   degrading into extractive institutional maintenance.
 *
 * KEY AGENTS:
 *   - social_democratic_parties: Agenda-setters; define strategy as electoral accumulation; benefit from union affiliation and state legitimacy
 *   - established_labor_unions: Beneficiaries and partial agenda-setters; institutionally locked into social-democratic parties; trade autonomy for formal recognition
 *   - revolutionary_militants: Victims, identity-locked; suppressed as adventurist within working-class organizations; cannot exit without abandoning communist identity
 *   - extra_parliamentary_movements: Victims, resource-starved; receive hostility from unions and state repression legitimated by gradualist consensus
 *   - syndicalist_workers: Victims, trapped; isolated without inter-union infrastructure; dependent on cooperation that institutionally opposes them
 *   - capitalist_state: Observer; prefers electoral incorporation to revolutionary rupture; benefits from having working-class opposition organized in state-controllable institutions
 *   - vanguard and council traditions: Excluded; architecturally incompatible with electoral gradualism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.62).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Socialism via Democratic Electoral Gradualism").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'b834af53-8b72-46cf-b5d8-3ce65fba4c8b').
narrative_ontology:cs_kernel_codification('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', distributed).
narrative_ontology:cs_authority_grounding('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', distributed).
narrative_ontology:cs_reading_relation('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', foundational, electoral_majorities_sufficient_for_socialism).
narrative_ontology:cs_axiom_status(electoral_majorities_sufficient_for_socialism, holdable).
narrative_ontology:cs_axiom_grounding('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', electoral_majorities_sufficient_for_socialism, empirically_contingent).
narrative_ontology:cs_axiom('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', foundational, institutional_continuity_compatible_with_transformation).
narrative_ontology:cs_axiom_status(institutional_continuity_compatible_with_transformation, holdable).
narrative_ontology:cs_axiom_grounding('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', institutional_continuity_compatible_with_transformation, deontological).
narrative_ontology:cs_reference_frame('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', universal_suffrage_achievable_within_capitalism).
narrative_ontology:cs_drift_state('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', contemporary_post_welfare_state_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b834af53-8b72-46cf-b5d8-3ce65fba4c8b', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, established_labor_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_political_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, syndicalist_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutionalize and represent the working-class interest through electoral competition and parliamentary politics. Set the strategic orientation of the labor movement toward legal reform, welfare-state expansion, and negotiated transition. Define what counts as 'realistic' working-class aspiration. Benefit from union membership, campaign resources, and the legitimacy of being the only 'serious' vehicle for working-class power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain institutional recognition, collective bargaining rights, workplace representation, and influence over social-democratic party platforms. Trade away autonomy and direct action capability for seat at the reform table. Become administrators of welfare systems and legitimators of incremental change. Exit would mean losing hard-won formal status and returning to precarity.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, established_labor_unions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, established_labor_unions, agenda_setter).

% Are marginalized within the labor movement, isolated as 'adventurist' or 'dogmatic,' denied platform and resources within working-class organizations, and subject to police suppression legitimated by claims that they destabilize the electoral path. Identity-locked: their self-conception as communists depends on rejection of the reformist consensus that has captured the movement; exit means abandoning the political identity itself.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, biographical, identity_locked, national).

% Attempt direct action, workplace occupations, general strikes, or autonomous organization outside the electoral framework. Starved of union resources and solidarity (unions officially neutral or opposed), delegitimized as 'not serious,' and subject to legal repression (with social-democratic parties often voting for security measures). The constraint structures their suppression as internally-generated working-class consensus rather than external state violence.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_movements, payer,
    moderate, biographical, constrained, national).

% Organized in non-aligned or autonomous unions, excluded from the bargaining table with established unions, unable to coordinate across regions without the infrastructure the social-democratic apparatus controls. Trapped: they depend on cross-union solidarity to overcome isolation, but the apparatus that could provide it is institutionally hostile to their strategic approach.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, syndicalist_workers, payer,
    powerless, biographical, trapped, local).

% Manages the constraint as a stabilizing mechanism: by legitimating electoral paths to socialism and marginalizing revolutionaries, it channels working-class political energy into institutions the state controls through law, media, and funding. The state can absorb welfare-state expansion without losing productive capacity; it cannot absorb expropriation and direct control. The constraint's benefit to the state is structural — it prefers the social-democratic opponent to the revolutionary one.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_state, observer,
    institutional, generational, analytical, national).

% Is architecturally incompatible with electoral gradualism: the vanguard reading demands organized rupture and disciplined party seizure, not diffuse ballot accumulation. Excluded from the conversation because the democratic-gradualist reading treats vanguardism as empirically falsified by soviet degeneration and normatively unsustainable within liberal-democratic institutions. Would argue that reliance on electoral rules written by capitalists guarantees incorporation and defeat.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_party_tradition, excluded,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_party_tradition).

% Is architecturally incompatible with both electoral gradualism and vanguardism: councils require direct democracy and federated autonomy, which electoral machines dissolve and vanguard parties subordinate. Absent from formal negotiation because established institutions have no category for workplace direct democracy. Would argue that both vanguardism and electoralism corrupt working-class autonomy and install new hierarchies.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_tradition, excluded,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_non_agent(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of translating dispersed working-class interests into unified political pressure: the democratic-socialist reading proposes that electoral aggregation, union federation, and party discipline provide the mechanism for organizing the working class as a coherent force. Without this mechanism, working-class power remains atomized across workplaces and localities; with it, the class can exercise majority power through the ballot.
% TRANSFER_FUNCTION: Moves control over the timing and method of working-class transformation from autonomous workers and their councils to electoral parties and union bureaucracies. Workers surrender direct-action autonomy and strategic initiative in exchange for formal representation and the promise that electoral majorities will produce socialism gradually. The arrangement also transfers legitimacy-power from the capitalist state (which must enforce the rules) back to the working-class movement (which can win elections and claim mandate).
% ABSENT_VOICES: Revolutionary militants, syndicalist workers, council communists, and autonomous organizing traditions are excluded by design. They would argue that the electoral framework cannot produce socialism because it is written and administered by capitalists, that gradualism means permanent incorporation, and that working-class power requires direct democracy and expropriation, not ballot accumulation. Their exclusion is enforced by social-democratic parties and unions that define them as 'adventurist' and by capitalist media and state forces that suppress them.
% DISAPPEARANCE_RATIONALE: If the constraint — the institutional linkage between unions and social-democratic parties, the electoral strategy, and the suppression of revolutionary alternatives — disappeared, the working-class movement would immediately splinter into competing strategic tendencies. Revolutionary organizations would resurface, autonomous unions would proliferate, council-communist ideas would become organizationally viable. Capitalist restoration would risk acceleration because the unified class pressure that social democracy (despite gradualism) provides would fragment. The constraint's disappearance would destabilize both the working-class movement and the state's preferred mode of incorporating it.
% FOUNDING_PROBLEM: The coordination of geographically dispersed workers, the inability of isolated workplace struggles to accumulate into systematic power, and the danger that revolutionary action without sufficient preparation would be crushed by capitalist military and police. Electoral democracy, universal suffrage, and independent labor organizations offered a path to accumulate working-class force without frontal confrontation with state violence.
% FOUNDING_PROBLEM_CORROBORATION: Social-democratic parties and established unions attest the founding problem is live: economic power remains concentrated, capitalist states retain military monopoly, and uncoordinated insurrection risks catastrophic defeat (witness: various failed uprisings in 1848, 1871, 1920s). Revolutionary traditions attest the founding problem is obsolete or falsely framed: the problem is not insufficient coordination for electoral majorities but insufficient independence from capitalist institutions; the solution is not gradual reform but organizational rupture. Third-party observers (historians, Marxist theorists outside both camps) corroborate that the founding problem WAS historically live (pre-1870s working-class organizing was indeed fragmented and vulnerable) but that its persistence as a rationale is contested — gradualist parties now use it to preempt challenges rather than to meet a current condition.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.40) because the constraint channels real working-class political power through electoral channels — it genuinely aggregates and amplifies working-class force in a way isolated struggles cannot. But it also extracts: the parties and unions siphon working-class initiative, resources, and strategic autonomy to institutions they control. Suppression is high (0.62) because the constraint's persistence requires active marginalization of alternatives — revolutionary parties are prosecuted, autonomous unions are denied federation resources, council-communist ideas are deemed 'unrealistic.' Theater rises from 0.30 to 0.50 over the interval: as elections fail to produce socialism and economic conditions deteriorate, the constraint's actual function becomes increasingly theatrical — maintaining the appearance of working-class political agency through electoral ritual while defensive struggles lose. Suppression requirement peaks at t=32 and then falls slightly, reflecting cycles of intensified repression (responses to rising militant activity) followed by temporary relaxation as movements burn out. The constraint's ultimate trajectory is plateauing extraction with rising theater — the signature of a piton candidate: once-genuine coordination (early social democracy) now maintained through performative institutional theater rather than living function.
 *
 * PERSPECTIVAL GAP:
 *   The social-democratic party and union leadership perceive this constraint as rope: it solves coordination problems, amplifies working-class voice, achieves reforms. From the revolutionary and syndicalist seats, it is snare: it disarms the working class by substituting for direct power, extracts leadership and initiative to institutions the capitalist class controls, and suppresses alternatives with the workers' own organizations. The engine computes per-seat perception from the authored structural data. From the state's position, it is optimal infrastructure — it provides the working class a legitimate channel for grievance that stops short of expropriation. The divergence is not an error; it is the heart of what the constraint DOES: it is experienced as coordination by those who benefit from it and as suppression by those trapped in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Social-democratic parties and unions are structural beneficiaries: they command resources, set agendas, and extract membership and political energy to institutions they administer. They have arbitrage-quality exit (they can always retreat into welfare-state administration or become centrist parties without the working-class base). Revolutionary militants and extra-parliamentary movements are victims: they pay through marginalization, isolation, and active suppression, with constrained or identity-locked exit (they cannot leave without abandoning the entire political identity). The state is an observer: it benefits structurally from the constraint's operation (working-class power channeled into statable institutions) but does not directly collect from it. Syndicalist workers are trapped victims: they lack the federated infrastructure to coordinate resistance without the very unions that suppress them.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows mandatrophy symptoms: it was founded to solve a live problem (how to accumulate sufficient working-class force to challenge capitalism), but that problem has shifted. Where electoral democracy once maximized working-class power (before the franchise extended, before unions legalized), it now may constrain it (working-class majorities are electoral realities, but electoral bodies lack command over capital and production). The theater ratio climbs even as extractiveness plateaus, suggesting the constraint persists through institutional inertia and the locked interests of union bureaucracies, not through solving the founding problem. The founding-problem-status assessment is 'contested' precisely because gradualists still attest the problem is live (capital still has military power) while revolutionaries attest it has been obsoleted by changed conditions (working-class electoral majorities, welfare-state power, global solidarity infrastructure). The mandatrophy_resolved flag is NOT set because the constraint still produces substantial coordination (elections do aggregate working-class interests) even as it ossifies into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_socialism_empirical_exhaustion,
    'Has democratic electoral gradualism produced socialism in any historical instance, or does the historical record show welfare-state capture without expropriation in every case where social democracy held power?',
    'Historical comparative analysis of social-democratic governments'' tenure in power: did any use electoral majorities to expropriate capital? Did all eventually compromise on ownership in exchange for welfare administration?',
    'If no historical case produced expropriation, the constraint''s founding-problem-status should revert from ''contested'' to ''dead'' — gradualism solves coordination but not transformation. The type would drift from rope toward piton (theater-maintained coordination without functional output).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_socialism_empirical_exhaustion, empirical, 'Whether electoral gradualism has ever produced socialism as claimed, or only welfare-state capitalism.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the suppression of revolutionary movements structurally (police, law, capital) versus internalized (workers believing gradualism is realistic and revolution impossible)?',
    'Post-suppression trajectory: if suppression ceases (law changes, police withdraw), do revolutionary movements resurface or remain inactive? Internalization persists absent external repression; structural suppression evaporates when enforcement stops.',
    'If suppression is primarily structural, removing it would release revolutionary movements immediately and the constraint''s persistence would depend on police power. If internalized, the constraint persists even after repression stops because workers have absorbed the gradualist framing as realistic. Classification implications: structurally suppressed constraints are more contingent; internalized ones are more entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternatives is structural or internalized in working-class consciousness.').

omega_variable(
    institutional_capture_irreversibility,
    'Can social-democratic parties and unions reverse their institutional embedding in capitalist state structures (welfare boards, collective bargaining, media access) to return to independent working-class organization, or does the sunk cost of institutional position make such reversal impossible?',
    'Structural analysis of union and party finance, legal obligations, and administrative dependencies: how much of their resource base is now tied to state recognition? What would reversal cost them?',
    'If reversal is costly but possible, social democracy remains a strategic choice constrained by interests. If impossible, social democratic institutions are no longer working-class organizations but capitalist-incorporated bodies that use working-class membership as a base. This would reframe the constraint from asymmetric-extraction (snare) to institutional-capture (false-summit mountain mistaken for genuine working-class power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_irreversibility, conceptual, 'Whether institutional embedding of labor movements in state structures is reversible or locks them into capitalism indefinitely.').

omega_variable(
    reading_sibling_foreclosure_ambiguity,
    'Does the democratic-gradualist reading''s core premise (electoral majorities can achieve socialism through institutional reform) logically foreclose the vanguard-rupture reading''s core premise (socialism requires party seizure of state and transitional dictatorship), or do both remain structurally live options for different parties and historical moments?',
    'Logical analysis: if a party wins electoral majorities and uses state power to expropriate capital, has it vindicated gradualism or vanguardism or both? If the ballot-path fails and extraparliamentary forces seize power, has gradualism been empirically falsified or merely historically interrupted?',
    'If the readings foreclose each other, the kernel is genuinely contested and only one can be instantiated. If both remain live (as different strategic bets on different conditions), the network linking them should show ''coexists_with'' rather than deep opposition. This affects how the constraint community (social democrats, communists, libertarians) manages disagreement: as logical contradiction or as strategic divergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure_ambiguity, conceptual, 'Whether the democratic-gradualist reading and the vanguard-rupture reading are logically incompatible or strategically divergent options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mani_tr_t8, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(mani_tr_t16, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(mani_tr_t32, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(mani_tr_t50, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mani_be_t8, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(mani_be_t16, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(mani_be_t32, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 32, 0.43).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(mani_be_t50, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mani_su_t8, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(mani_su_t16, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(mani_su_t32, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(mani_su_t50, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% The 'manifesto_revolutionary_method' kernel is instantiated in three structurally distinct constraints, one per reading. The democratic-gradualist reading is defined by its core premises: electoral majorities and gradual institutional reform can achieve socialism within existing democratic structures. The vanguard reading demands organized party seizure and transitional dictatorship. The council reading demands federated workplace assemblies and direct democracy. These readings are incommensurable claims sharing a kernel (the founding problem of how to accumulate working-class power into socialism) but producing different beneficiary/victim structures, suppression mechanisms, and ε values. All three constraint stories must be generated to map the full kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
