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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Democratic Gradualism: Electoral Socialism via Institutional Reform
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   The democratic gradualism reading of Marxist revolutionary method asserts
 *   that working-class power can be achieved through democratic electoral
 *   majorities and gradual institutional reform within capitalist
 *   democracies. This reading structures working-class political organization
 *   around social-democratic parties and trade-union hierarchies,
 *   establishing them as the authorized representatives of working-class
 *   interest and delegitimizing revolutionary and extra-institutional
 *   approaches as 'adventurist.' The constraint is claimed as a rope (genuine
 *   coordination unifying working-class forces) but operates with substantial
 *   asymmetric extraction: institutional leadership benefits from monopoly
 *   over working-class representation; revolutionary militants and
 *   extra-institutional movements bear the cost of delegitimization and
 *   exclusion. This is ONE READING of the contested kernel
 *   manifesto_revolutionary_method. Sibling readings
 *   (council_communist_reading, vanguard_rupture_reading) instantiate
 *   structurally different constraints with different ε values and
 *   beneficiary/victim sets — they are not variations on this story but a
 *   constraint family (ε values differ by 0.35+ across readings;
 *   beneficiary/victim sets are distinct; type classifications are
 *   independent).
 *
 * KEY AGENTS:
 *   - social_democratic_parties — institutional agenda-setter, beneficiary (control legitimate working-class voice, electoral access, policy influence)
 *   - trade_union_bureaucracies — institutional beneficiary + secondary agenda-setter (legitimacy from official worker representation, constrained exit because position depends on party coordination)
 *   - working_class_electoral_base — organized power, beneficiary (electoral representation, welfare gains, constrained by requirement that transformation stay institutional)
 *   - revolutionary_militant_factions — moderate power, identity-locked victims (analysis delegitimized, excluded from authorized discourse, suppressed through denunciation and organizational competition)
 *   - extra_institutional_movements — powerless, trapped victims (wildcat strikes, assemblies suppressed through competing organizations and resource disadvantage)
 *   - liberal_democratic_institutions — institutional co-authority, analytical scope (the standing rules defining legitimate political method)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.51).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism: Electoral Socialism via Institutional Reform").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'c006a7b5-be80-4a34-a073-0c41244afa35').
narrative_ontology:cs_kernel_codification('c006a7b5-be80-4a34-a073-0c41244afa35', distributed).
narrative_ontology:cs_authority_grounding('c006a7b5-be80-4a34-a073-0c41244afa35', extraction).
narrative_ontology:cs_interpretation_layer_present('c006a7b5-be80-4a34-a073-0c41244afa35').
narrative_ontology:cs_reading_relation('c006a7b5-be80-4a34-a073-0c41244afa35', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('c006a7b5-be80-4a34-a073-0c41244afa35', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('c006a7b5-be80-4a34-a073-0c41244afa35', foundational, capitalist_state_incrementally_reformable).
narrative_ontology:cs_axiom_status(capitalist_state_incrementally_reformable, holdable).
narrative_ontology:cs_axiom_grounding('c006a7b5-be80-4a34-a073-0c41244afa35', capitalist_state_incrementally_reformable, empirically_contingent).
narrative_ontology:cs_axiom('c006a7b5-be80-4a34-a073-0c41244afa35', foundational, electoral_majority_sufficient_for_socialism).
narrative_ontology:cs_axiom_status(electoral_majority_sufficient_for_socialism, holdable).
narrative_ontology:cs_axiom_grounding('c006a7b5-be80-4a34-a073-0c41244afa35', electoral_majority_sufficient_for_socialism, empirically_contingent).
narrative_ontology:cs_reference_frame('c006a7b5-be80-4a34-a073-0c41244afa35', institutional_continuity_with_liberal_democracy).
narrative_ontology:cs_drift_state('c006a7b5-be80-4a34-a073-0c41244afa35', contemporary_post_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c006a7b5-be80-4a34-a073-0c41244afa35', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militant_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_institutional_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electoral_base).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_institutional_direct_action_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the electoral strategy for socialist transformation through institutional reform. Control the interpretation of legitimate working-class action within democratic frameworks. Benefit from institutional access, parliamentary representation, and coalition with trade unions. Their position depends on maintaining working-class unity around electoral channels and preventing extra-institutional rupture. They set the terms of what counts as responsible working-class politics and what is delegitimized as adventurist.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, mobile, national).

% Official institutional representatives of organized labor. Gain legitimacy, negotiating power, and control over working-class mobilization through recognition as the sole authorized representative. Benefit from coordination with social-democratic parties and from the constraint that working-class action must flow through union hierarchies. Their structural position requires suppressing wildcats and militant direct action as irresponsible, creating a monopoly on legitimate working-class voice.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, agenda_setter).

% The mass constituency whose votes are mobilized for social-democratic parties. Gain the prospect of electoral power, welfare expansion, labor protections, and democratic participation in governance. Bear the constraint that transformation must proceed through existing state institutions, slowing concrete improvements and requiring long organizational commitment without guarantee that socialist transformation will actually materialize.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electoral_base, beneficiary,
    organized, biographical, constrained, national).

% Political currents that argue capitalist state cannot be reformed electorally and that working-class power requires extra-institutional rupture and direct action. Bear systematic suppression through denunciation as sectarian, ultra-left, or adventurist. Their analysis is delegitimized within the authorized discourse of working-class politics. Exit from this position requires abandoning the revolutionary analysis of state power — a fused identity commitment that makes exit feel like betrayal of class loyalty.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militant_factions, payer,
    moderate, biographical, identity_locked, national).

% Grassroots movements outside the union-party hierarchy: wildcat strikers, neighborhood assemblies, tenant unions, community organizing. Operate at immediate, local scales. Suppressed through union/party denunciation, competing worker organizations that channel activity into official channels, and structural disadvantage in resource access. The constraint systematically funnels working-class organization toward centralized, institutional forms that dilute local power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, extra_institutional_direct_action_movements, payer,
    powerless, immediate, trapped, local).

% The standing structure being reformed. Not strategic actors within this constraint's composition but the analytical object. The constraint's structural assumption is that capitalist state institutions can be progressively reformed through electoral majority; the capitalist class and state apparatus remain the referent, not active defenders of the constraint.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_class_and_state_apparatus, observer,
    institutional, generational, analytical, national).

% The standing institutional forms (universal suffrage, parliamentary procedure, constitutional law, rule of law) within which working-class transformation is authorized to proceed. Co-authority with the social-democratic parties in defining what counts as legitimate political action. The constraint's enforcement depends on the stability and legitimacy of liberal-democratic rules as the binding framework.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_institutions, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_institutions).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies diverse working-class forces (unions, parties, constituencies, intellectuals) around a shared method of transformation — electoral majorities leading to institutional reform. Solves the problem of how to mobilize mass working-class consent for systemic change within existing legal-democratic frameworks, avoiding fragmentation into competing revolutionary sects.
% TRANSFER_FUNCTION: Transfers political authority from extra-institutional movements and revolutionary analysts to social-democratic parties and union bureaucracies. Working-class mobilization energy flows toward institutional channels controlled by party/union leadership; legitimacy and resources accumulate to the institutional authorities; revolutionary militants and extra-institutional movements bear the cost of systematic delegitimization and organizational marginality.
% ABSENT_VOICES: Revolutionary parties (Leninist vanguards), council-communist movements (workplace assemblies), and autonomist direct-action networks are structurally excluded from legitimate working-class representation. They would contest the foundational premise that capitalist state institutions can be reformed without rupture, and would argue that gradual institutional methods materially serve capital's interest in decomposing revolutionary consciousness. Their exclusion is enforced through denunciation and organizational competition, not legal prohibition.
% DISAPPEARANCE_RATIONALE: If the constraint dissolved — if the institutional monopoly on legitimate working-class representation shattered and extra-institutional direct action regained equal standing — working-class politics would reorganize: wildcat strikes and assembly-based movements would compete directly with electoral channels for power and resources; revolutionary analysis would reenter authorized discourse as a live option; the tempo of demands would accelerate and forms of struggle would diversify. Electoral parties would lose their gatekeeping function.
% FOUNDING_PROBLEM: Late 19th-century European working-class movements faced the historical question: can workers achieve power through democratic electoral systems being extended to them, or is capitalist state rupture necessary? Democratic gradualism answered: yes, electoral majorities and institutional reform can transform capitalism into socialism, provided working-class forces remain unified, disciplined, and institutional.
% FOUNDING_PROBLEM_CORROBORATION: Social-democratic theorists and parties maintain the founding problem is live: capitalism persists, electoral socialism offers lower-risk transformation than rupture. Revolutionary communists, autonomists, and independent historical scholars (Perry Anderson, Arno Mayer) attest the founding problem is dead or falsely posed: electoral channels have consistently failed to deliver systemic change; state institutions resist reform when capitalism's core interests are threatened; the constraint now prevents rather than enables working-class power. No consensus exists outside the benefiting parties themselves.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42 endpoint) because the constraint binds working-class power into forms that produce genuine benefits (electoral voice, labor protections, welfare expansion) alongside asymmetric institutional consolidation. Unlike a pure snare, real coordination gains exist: unified working-class forces have greater bargaining power than fragmented ones. But extractiveness is not negligible: the constraint's persistence depends on actively preventing extra-institutional methods from competing equally, and the institutional leadership captures concentrated rents (control over strategy, authority to define legitimate action, resource allocation) from the coordination function. Suppression is substantial (0.51) because the constraint actively suppresses revolutionary analysis through denunciation and organizational competition; extra-institutional movements face structural disadvantage. Theater is moderate (0.38) because much genuine organizational work occurs, but a growing share of activity (party conferences, union congresses, ritual campaigns) becomes performative maintenance of institutional legitimacy. The measurement trajectory shows extractiveness rising through early periods (working-class forces disciplining around electoral consensus, t=0-15), plateauing during mature institutional consolidation (t=15-30), and stabilizing around 0.40-0.42 in recent decades (modern constraint steady-state). Suppression requirement follows a similar arc: rising as the institutional monopoly tightens (t=0-15), peaking when extra-institutional competition is most active (t=15-20), then declining as the constraint normalizes and alternatives become structurally marginal (t=25+, less active suppression needed because the terrain is already structured against them). Theater ratio shows the pattern of performance work: rising during periods of contested legitimacy (t=0-20) as the institutional parties must actively perform their revolutionary credentials and working-class authenticity, then settling to a steady ~0.38 as the performance becomes routine and normalized.
 *
 * PERSPECTIVAL GAP:
 *   Social-democratic parties and union leadership experience this constraint as genuine coordination — they see themselves as unifying a fractious working class and preventing sectarian fragmentation that would dissipate power. From the revolutionary-militant and extra-institutional perspective, the same constraint appears as institutional cooptation that disables working-class power by funneling energy into channels that capitalism can manage, absorb, and nullify. Neither perspective is empirically false — they describe the same constraint from different structural positions. The institutional leadership sits at low d (beneficiary end): they experience coordination benefits and face no organized rivals. Revolutionary militants sit at high d (target end, d=0.78 by override): they experience extraction through delegitimization and trapped identity. The engine's classification computation will be divergent per seat: the institutional agenda-setters will show ROPE or TANGLED_ROPE from their seat; militants will show SNARE or TANGLED_ROPE (extraction side) from theirs. The structural data (beneficiary/victim declarations, power atoms, exit options) support this divergence; the metrics support it; the override captures the identity-lock that makes the moderate-power militants sit as high-extraction targets despite moderate organizational resource.
 *
 * MANDATROPHY ANALYSIS:
 *   Democratic gradualism exhibits acute mandatrophy structure. It was founded to solve the problem: can working-class power achieve socialism without revolutionary rupture, through electoral majorities and institutional reform? The contemporary status of that founding problem is CONTESTED: gradualists attest it remains live (capitalism persists; gradual reform offers safest path; vanguard ruptures have failed or failed worse than gradualism). Critics attest it is DEAD (electoral methods have consistently failed to deliver systemic change; the constraint now functions to PREVENT rather than enable working-class transformation). The contested status of the founding problem — combined with the constraint's persistent asymmetric extraction from revolutionary alternatives — creates mandatrophy: the institutional justification for the monopoly (need to unify forces toward transformation) is no longer universally accepted, yet the machinery persists because institutional parties have organizational power to maintain it. Evidence for mandatrophy: (1) suppression_requirement remains high (0.51) despite decades of constraint operation — active defense is required, not passive legitimacy; (2) theater_ratio is substantial (0.38) — much activity is performance of the constraint's necessity rather than actual strategic advance toward socialism; (3) founding_problem_status is CONTESTED not LIVE — no external corroboration exists that the problem the constraint was built to solve remains unsolved; the constraint persists through organizational inertia and power concentration, not through agreement on its necessity. Mandatrophy is NOT RESOLVED in this story. The reading itself does not settle whether democratic gradualism has outlived its function — that remains a living contestation between gradualists and revolutionaries. What the constraint story documents is that the institutional persistence of the constraint is no longer grounded in universal agreement about the founding problem's solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is democratic gradualism a coherent strategy for working-class power within capitalism, or does it systematically serve capital''s interest by decomposing revolutionary consciousness and channeling working-class energy into institutional forms that cannot deliver systemic change?',
    'Historical-empirical: systematic comparison of working-class outcomes (redistribution, power, transformation) under gradualist vs. revolutionary methods across 20th-century cases (Nordic social democracy, Soviet transitions, Chinese revolution, European May 1968, etc.). Conceptual: theoretical dispute over whether capitalist state institutions can be reformed without rupture — this is not empirically resolvable alone.',
    'If gradualism is effective, the constraint''s asymmetric extraction is justified as the price of coordination and is sustainable. If it is ineffective, the constraint functions as a trap that prevents working-class power from materializing — mandatrophy becomes acute and the constraint should be reclassified toward snare. If the dispute is conceptual (empirical evidence does not settle which reading is correct), then the kernel remains contested and both constraint stories persist in the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether democratic gradualism is a viable or illusory strategy for working-class power.').

omega_variable(
    institutional_monopoly_necessity,
    'Is the monopolization of working-class representation within social-democratic parties and trade-union hierarchies necessary for effective coordination, or does it concentrate authority in ways that prevent transformative outcomes?',
    'Comparative institutional analysis: do working-class movements with decentralized, federated, or assemblied authority structures (council communism, autonomist networks, contemporary assembly movements) show higher or lower effectiveness in delivering working-class interests compared to centralized party/union structures? Effectiveness measured by redistributive outcomes, power in production, speed of transformation, democratic participation.',
    'If centralization is necessary, the extracted rent to institutional leadership is the price of coordination effectiveness. If decentralization is equally or more effective, the constraint''s extraction represents pure institutional capture — the monopoly persists because the institutional leadership has organizational power to enforce it, not because it is functionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_monopoly_necessity, empirical, 'Whether institutional monopoly over working-class representation is functionally necessary or merely preservative of institutional power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of revolutionary alternatives structurally enforced (organizational barriers, institutional exclusion, resource disparities) or internalized (revolutionary activists have absorbed gradualist convictions or fused identity with revolutionary method such that integration feels like betrayal)?',
    'Post-exit suppression trajectories: track individuals who leave revolutionary factions and integrate into social-democratic parties or mainstream unions. Measure persistence of suppression (sense of delegitimization, political marginality, felt constraint on discourse) after structural barriers are removed. Internalized suppression persists; structural suppression does not.',
    'If suppression is internalized, the constraint''s extractiveness is higher than the structural measure suggests — the targets carry the suppression with them after exit, making the constraint more durable and more difficult to dissolve through institutional opening. If structural, a change in institutional barriers could rapidly dissolve the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of revolutionary analysis is structural or internalized.').

omega_variable(
    electoral_threshold_for_transformation,
    'What electoral majority is required for social-democratic parties to enact socialism through institutional reform, and is such a majority achievable under capitalism without revolutionary rupture?',
    'Empirical case studies of moments where gradualist parties approached or achieved large parliamentary majorities (post-WWII European social democracy, 1945-1975; contemporary Latin American left; post-1989 Eastern European transitions). Analysis: did they enact socialism, or did they enact welfare expansion within capitalism? Did they face capital flight, coup threats, or other forms of state/capital sabotage? At what point of reform does capital''s structural veto become binding?',
    'If the required majority is politically unachievable (capital fragments working-class unity through media, sabotage, or violence before the majority can form), or if transformation within institutional limits is impossible (capital''s structural veto is binding), the founding problem of democratic gradualism is revealed as malposed — the constraint persists as a trap, not a solution. If transformation is achievable, the constraint''s extraction is the price of effective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_threshold_for_transformation, empirical, 'Whether democratic electoral majorities can achieve socialism without capital flight, state rupture, or external sabotage.').

omega_variable(
    alternative_reading_coexistence,
    'Can the three readings of the manifesto_revolutionary_method kernel coexist as equally valid interpretations within different working-class movements, or does one reading''s truth-claim foreclose the others within any consistent strategic framework?',
    'Conceptual-historical: analyze whether a single working-class movement can hold multiple readings simultaneously (e.g., a movement that deploys both electoral and council structures, or both gradualist reform and vanguard discipline). Examine historical movements that attempted this (Eurocommunism, participatory socialism, Bolivarian processes) and their outcomes. Assess whether reading conflicts at the strategic level create incompatibility or whether they operate at different organizational scales.',
    'If readings coexist without foreclosure, the constraint family should be modeled as three live alternatives that different parties hold simultaneously. If one reading forecloses others (most likely: vanguard and council readings both foreclose gradualism''s assumption that capitalist state is reformable), the coexists_with relations should be replaced with forecloses relations in cs_structure.reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_coexistence, conceptual, 'Whether the three revolutionary-method readings are mutually coexistent or logically incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(mani_tr_t15, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(mani_tr_t25, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(mani_be_t15, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(mani_be_t25, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(mani_su_t15, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(mani_su_t25, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% Democratic gradualism is one of three structurally distinct readings of the kernel manifesto_revolutionary_method. The kernel is the contested claim: what method can working-class movements use to achieve socialist transformation? This reading argues electoral majorities and institutional reform within capitalist democracy. Sibling readings: vanguard_rupture (revolutionary party seizure + dictatorship of proletariat) and council_communist (workers' councils replace both capitalist state and vanguard party). Each reading instantiates a different constraint with different ε, beneficiary/victim sets, and type. They form a constraint family linked by affects_constraints edges: gradualism influences both siblings by establishing the electoral-institutional framework as the authorized terrain; vanguard reading forecloses gradualism's assumption of capitalist state reformability; council reading coexists with both (represents a distinct organizational form that neither forces nor is forced by gradualism or vanguardism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
