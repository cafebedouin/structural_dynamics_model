% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure and Transitional Dictatorship of the Proletariat (Rupture Reading)
 *   domain: political/philosophical
 *
 * SUMMARY:
 *   This file instantiates one reading of the manifesto_revolutionary_method
 *   kernel; the sibling readings are separate constraints (see
 *   kernel_context). The reading authored here holds that revolutionary
 *   transformation requires an organized party's seizure of state power,
 *   exercised as a transitional dictatorship of the proletariat under party
 *   guidance. As a standing arrangement it prescribes a centralized
 *   party-state: a professional revolutionary apparatus monopolizing
 *   political interpretation, a planning apparatus allocating resources, and
 *   the closure of every rival pathway — electoral gradualism, independent
 *   councils, anarchism — as deviation. The claim/metric split is deliberate:
 *   claimed_type records the structure judged true (tangled_rope — a genuine
 *   seizure-phase coordination function carrying asymmetric, mandate-less
 *   extraction), while the metrics record the arrangement's actual operation,
 *   including its 1936 terror peak and post-1991 partial retreat. The engine
 *   computes per-seat classifications from the structural data; where
 *   computed types diverge from the claim, that divergence is the datum.
 *   Epsilon's referent is the standing party-led arrangement itself, assessed
 *   substantially by the reading's own internal critics — not by the sibling
 *   readings' preferred arrangements.
 *
 * KEY AGENTS:
 *   - - vanguard_party_cadres: agenda-setting ruling apparatus (institutional / identity_locked) — administers the political monopoly and collects its principal gains
 *   - - state_planning_apparatus: secondary beneficiary (institutional / constrained) — runs allocation under plan discipline; careers and authority flow from the arrangement
 *   - - working_class_majority: nominal sovereign, dual-positioned (moderate / trapped) — receives guaranteed employment and services while stripped of independent organs
 *   - - political_pluralists: target (moderate / constrained) — bear censorship, exclusion, and prosecution for advocating competition
 *   - - autonomous_worker_organizations: primary organized target (organized / trapped) — workplace organs dissolved into state unions; leaders jailed or exiled
 *   - - left_opposition_factions: framework-bound target (organized / identity_locked) — Marxist dissidents expelled and erased while remaining inside the tradition
 *   - - anarchist_militants: excluded voice (organized / trapped) — anti-state socialists suppressed first and everywhere
 *   - - nonparty_citizens: excluded voice (powerless / trapped) — carry the arrangement's daily demands with no channel of objection
 *   - - comparative_politics_scholars: analytical observer (analytical / analytical) — compiles the comparative record from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.62).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.62).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure and Transitional Dictatorship of the Proletariat (Rupture Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political/philosophical").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '2c4657cb-52c3-4181-87da-903cf04e49f4').
narrative_ontology:cs_kernel_codification('2c4657cb-52c3-4181-87da-903cf04e49f4', fixed_text).
narrative_ontology:cs_authority_grounding('2c4657cb-52c3-4181-87da-903cf04e49f4', extraction).
narrative_ontology:cs_interpretation_layer_present('2c4657cb-52c3-4181-87da-903cf04e49f4').
narrative_ontology:cs_reading_relation('2c4657cb-52c3-4181-87da-903cf04e49f4', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c4657cb-52c3-4181-87da-903cf04e49f4', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('2c4657cb-52c3-4181-87da-903cf04e49f4', foundational, proletarian_emancipation_requires_party_seizure_of_state_power).
narrative_ontology:cs_axiom_status(proletarian_emancipation_requires_party_seizure_of_state_power, holdable).
narrative_ontology:cs_axiom_grounding('2c4657cb-52c3-4181-87da-903cf04e49f4', proletarian_emancipation_requires_party_seizure_of_state_power, instrumental).
narrative_ontology:cs_axiom('2c4657cb-52c3-4181-87da-903cf04e49f4', foundational, transitional_dictatorship_withering_away_promise).
narrative_ontology:cs_axiom_status(transitional_dictatorship_withering_away_promise, holdable).
narrative_ontology:cs_axiom_grounding('2c4657cb-52c3-4181-87da-903cf04e49f4', transitional_dictatorship_withering_away_promise, empirically_contingent).
narrative_ontology:cs_reference_frame('2c4657cb-52c3-4181-87da-903cf04e49f4', party_seizure_transitional_dictatorship).
narrative_ontology:cs_drift_state('2c4657cb-52c3-4181-87da-903cf04e49f4', contemporary_post_1991, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c4657cb-52c3-4181-87da-903cf04e49f4', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, left_opposition_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, anarchist_militants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, working_class_majority).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, working_class_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Full-time professional revolutionaries who staff the central committee, regional committees, and after victory the ministries and planning bodies. They set the political line, decide which organizations may legally exist, and control appointments throughout the state. Their livelihood, status, and personal safety are bound to the party's continued monopoly; leaving means defection charges, loss of everything, and in hard periods worse.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, agenda_setter,
    institutional, generational, identity_locked, national).

% Economists, engineers, and administrators who run the state plan, allocate investment goods, and manage production targets. The arrangement channels careers, housing, and authority to them that a market economy would price differently; their technical judgment counts insofar as it serves plan fulfillment. A few emigrate; most are bound by posts, pensions, and files.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Industrial and agricultural workers in whose name the party rules. They receive guaranteed employment, housing queues, literacy campaigns, and subsidized staples, and they lost independent unions, strike rights, and any organ through which they could replace the leadership. Exit abroad was gated by exit visas most never received; exit within the system meant keeping one's head down.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, working_class_majority, beneficiary,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, working_class_majority, payer).

% Liberal lawyers, journalists, clergy, and rival-party politicians who argue for multiparty competition and rule of law. They publish until censored, stand in elections until these are cancelled, and face prison, exile, or silence. Some emigrate; many stay and shrink into private life.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, constrained, national).

% Factory committees, strike committees, and independent unions that organize workers directly at the workplace. They flourished in the revolutionary months, were folded into state-run unions once the party consolidated, and their leaders were arrested or exiled when they insisted on independence — Kronstadt 1921 is the emblematic case. Their members cannot exit the state economy that employs them.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, national).

% Marxist dissidents inside the governing tradition — the Workers' Opposition, Left Communists, later Trotskyists and reform-communist critics — who accept the revolutionary project but dispute party supremacy. They fight inside the framework, are expelled, jailed, or airbrushed from photographs, and rarely renounce the framework itself; their identity is at stake in staying.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, left_opposition_factions, payer,
    organized, generational, identity_locked, national).

% Anti-state socialists who reject party rule entirely and were the first competitors suppressed — Makhno's movement destroyed, the Spanish anarcho-syndicalists crushed in the May Days of 1937. They operate underground or in exile networks across borders; the arrangement has no place for them anywhere in its jurisdiction.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, anarchist_militants, excluded,
    organized, biographical, trapped, continental).

% Ordinary people outside the party who vote in uncontested elections, attend mandatory demonstrations, and read censored press. They carry the arrangement's demands daily with no channel to object except anonymous grumbling; leaving the country required permission most never obtained.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, nonparty_citizens, excluded,
    powerless, biographical, trapped, national).

% Academic observers who compile the comparative record of party-states, measure outcomes against comparable societies, and publish classifications the participants dispute. They hold no stake in the arrangement and can analyze it from outside.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, comparative_politics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates a dispersed, propertyless class into a disciplined instrument capable of seizing and holding state power against organized resistance, and afterwards of directing reconstruction through a single chain of command.
% TRANSFER_FUNCTION: Moves decision rights and armed force from popular assemblies and workplaces into the party center; moves obedience and public legitimation from the population to the party; moves the economic surplus through the state plan under apparatus control.
% ABSENT_VOICES: Anarchists and council communists objected from the first years and were physically removed from the conversation (Kronstadt 1921, the destruction of the Makhnovshchina, the 1937 May Days); nonparty workers had no forum at all. Their absence is not incidental — the arrangement defines legitimate politics as party-mediated, so the strongest objections come from voices the arrangement itself excludes.
% DISAPPEARANCE_RATIONALE: In every society where the arrangement held, its overnight removal reopened political competition, dissolved the planning hierarchy's control of employment and prices, and stripped the cadre stratum of position and immunity — the 1989-1991 collapses rearranged constitutions, borders, and property systems within months.
% FOUNDING_PROBLEM: After 1848 and the Paris Commune, the observable problem was that dispersed popular majorities repeatedly lost to centralized armed states: the Commune's militias were beaten by Versailles within weeks. The arrangement was built to solve how a propertyless majority acquires the coordination, discipline, and coercive capacity to seize a modern state and defend the seizure against internal counter-revolution and foreign intervention.
% FOUNDING_PROBLEM_CORROBORATION: That the founding military problem was real is corroborated from outside the benefiting parties by the mainstream historiography of 1848 and the Commune and by contemporaneous social-democratic and council-communist writers who accepted the problem while rejecting the vanguard solution. That the problem REMAINS live is attested only by the tradition's own parties and theorists; no corroborating source outside the benefiting parties affirms the live status, and that absence is itself signal.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.62 against the standing arrangement this reading prescribes — party seizure followed by party-guided transitional rule — assessed substantially by the reading's own internal critics (Luxemburg, the Workers' Opposition, the council communists it expelled): the class in whose name the party rules exercises no removable control over the party, and every pathway by which it might (independent unions, rival factions, contested soviet elections) was closed by enforcement. Suppression is authored at 0.62 as a RAW structural property — the engine scales only extractiveness by directionality and scope; suppression here is the deliberate closure of alternative socialist pathways, which is precisely what distinguishes this reading from its siblings. Theater rises to 0.62 at the 1936 peak (uncontested elections, soviets as acclamation bodies) and settles near 0.48 as surviving instances alternate ritual affirmation with genuine administrative function. Accessibility_collapse 0.60: within any consolidated jurisdiction alternatives collapse almost completely, but they survive abroad and revive wherever enforcement decays (1989-91), so collapse is jurisdictional, not cognitive. Resistance 0.55: Kronstadt, the 1953 East German strikes, Hungary 1956, Prague 1968, and Solidarity show sustained resistance, consistently crushed until enforcement itself collapsed. The measurement series share one eight-point grid (1848-2021). Base_extractiveness accumulates monotonically to the 1936 peak — rent-seeking layered onto a genuine seizure-phase coordination function, the classic accumulation signature — then declines as instances fail. Suppression_requirement is authored because the story specifically traces enforcement-capacity change: Cheka-to-terror buildup through 1936, post-1956 relaxation, 1989 collapse, and reconsolidation in surviving instances; the post-1991 scalars pool surviving high-enforcement party-states with defunct ones (compositional bimodality; see omega). Identity-lock dynamics: cadre exit is unthinkable because professional-revolutionary identity, nomenklatura status, and ideological worldview fuse; when that frame broke in 1989-91, enforcement collapsed in months without external conquest — the cheapest weakening the arrangement ever suffered was its own cadres stopping believing.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as coordination it personally built and staffs: from the central committee, suppressing factions is defense of the revolution, and the plan is the economy working. The payer seats compute the opposite: pluralists meet censorship, autonomous worker organizers meet prisons, and the class meets a sovereignty it cannot exercise. The working-class seat splits internally — guaranteed employment and literacy against the loss of every organ of self-rule — which is why it is authored dual-positioned rather than forced into one role. The engine derives these divergent per-seat classifications from power, exit, and role data; the divergence between the cadre seat's coordination-shaped experience and the payer seats' extraction-shaped experience is the measurement, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: vanguard_party_cadres collect the arrangement's principal gains (office, immunity, allocative power) and sit near the beneficiary end despite identity-locked exit; state_planning_apparatus collects careers and authority. Victims: political_pluralists, autonomous_worker_organizations, left_opposition_factions, and anarchist_militants bear the transfer of decision rights with trapped or identity-locked exit, placing them near the full-target end. The working_class_majority is dual-positioned: beneficiary of the project's outputs, bearer of the political dispossession — its derived directionality sits mid-range, the honest reading of a class that gained materially and lost instrumentally. Coalition potential among the powerless seats existed and was real (the 1917 soviet coalition of workers, soldiers, and left parties), and the arrangement's defining early move was preempting exactly that coalition — the faction ban and the dissolution of constituent bodies — which is why the powerless seats stay powerless rather than aggregating. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate the seats, and the one override candidate (the working class) is better expressed as a dual role than as a corrected scalar.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a dispersed majority seizes and defends state power against organized forces — was real, and the seizure-phase coordination function is genuine: this is why the classification must not collapse into pure extraction. But the arrangement's DECLARED mandate was transitional: the withering of the state once class rule consolidated. No instance ever executed that mandate; the state apparatus grew monotonically everywhere it held, and surviving instances affirm the trajectory only ritually. The mandate has atrophied while the arrangement persists — mandatrophy_resolved is authored true, and the missing sunset clause (has_sunset_clause false) is the structural marker: a genuinely transitional form would carry one. The classification therefore separates the live coordination core (seizure, defense, initial reconstruction) from the atrophied mandate (transition to statelessness), which is the tangled-rope determination: real coordination function, asymmetric and now mandate-less extraction riding on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the manifesto_revolutionary_method kernel; what would change structurally if a sibling reading were instantiated instead?',
    'Compile the sibling stories (democratic_gradualism_reading, council_communist_reading) and compare computed per-seat classifications, victim sets, and epsilon against this file.',
    'The council reading relocates coordination to federated workplace assemblies, removing the cadre beneficiary seat and changing the victim set to party apparatchiks; the gradualism reading removes the seizure and suppression machinery entirely, lowering suppression and replacing insurrectionary victims with reform-blockage victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: sibling readings would instantiate different constraints with different victim sets and epsilon.').

omega_variable(
    transitional_mandate_sincerity,
    'Is the transitional character of the dictatorship of the proletariat an operative design feature or a decorative doctrine — does any instance carry a binding mechanism by which party rule would end?',
    'Comparative constitutional review of every party-state for sunset or devolution mechanisms; doctrinal analysis of whether withering-away ever acquired institutional form.',
    'If transitional-by-design, the arrangement belongs to the scaffold family pending its sunset; the historical record (no instance ever devolved) supports treating the mandate as atrophied and the arrangement as open-ended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_mandate_sincerity, empirical, 'Whether the transitional mandate is operative or decorative.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the closure of alternative pathways maintained purely by external enforcement, or also by internalized identification (members and subjects experiencing rivalry as betrayal)?',
    'Post-1989 revival speeds: jurisdictions where enforcement collapsed saw rapid revival of pluralist and council forms (structural suppression dominant), while surviving instances show durable self-censorship and cadre loyalty beyond enforcement capacity (internalized component).',
    'A large internalized component predicts persistence after enforcement decay and raises effective suppression above the structural measure; a small one predicts rapid opening once enforcement fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    class_benefit_net_assessment,
    'Does the working class net-benefit from the arrangement (guaranteed employment, literacy, industrialization, welfare) sufficiently that the coordination side outweighs the political dispossession, or does cadre capture dominate?',
    'Distributional and outcome comparison of party-state populations against matched comparator societies, plus intra-class incidence of repression.',
    'A strong net-benefit finding weights the coordination side of the determination; a weak one pushes the arrangement toward the pure-extraction boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_benefit_net_assessment, empirical, 'Net benefit vs capture for the class in whose name the party rules.').

omega_variable(
    phase_indexicality_decomposition,
    'Is the vanguard method ONE continuous arrangement (seizure through transition, as the reading itself asserts) or two structurally distinct arrangements — an insurrectional coordination phase and a consolidating extraction phase?',
    'Test whether epsilon measured on the seizure window (1917-1921) differs invariantly from epsilon on the consolidation window (1921 onward) across instances; if the two windows yield different stable epsilon, decompose into two linked stories.',
    'Decomposition would classify the seizure phase as lower-epsilon coordination and the consolidation phase as higher-epsilon extraction; this file authors the unified reading because the reading''s own doctrine asserts continuity, and the omega records the alternative rather than resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_indexicality_decomposition, conceptual, 'Whether the reading''s asserted phase-unity survives epsilon-invariance testing.').

omega_variable(
    post1991_compositional_bimodality,
    'Do the post-1991 measurement scalars describe one arrangement or a bimodal mixture of surviving high-enforcement instances and defunct ones?',
    'Instance-weighted re-measurement separating surviving party-states from post-collapse jurisdictions; report per-instance series alongside the pooled series.',
    'Pooled scalars understate suppression where the arrangement survives and overstate it where it fell; terminal classification flips between the weightings, so the pooled series should be read as a mixture, not a mean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post1991_compositional_bimodality, empirical, 'Compositional bimodality of late-interval measurements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 1848, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1848, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1848, 0.08).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1902, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1902, 0.14).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1917, 0.22).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1921, 0.36).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1936, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1936, 0.62).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1956, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1956, 0.56).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1989, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1989, 0.44).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t2021, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 2021, 0.48).

% Extraction over time
narrative_ontology:measurement(vanguard_rupture_reading_be_t1848, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1848, 0.28).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1902, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1902, 0.36).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1917, 0.55).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1921, 0.7).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1936, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1936, 0.84).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1956, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1956, 0.76).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1989, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1989, 0.64).
narrative_ontology:measurement(vanguard_rupture_reading_be_t2021, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 2021, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vanguard_rupture_reading_su_t1848, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1848, 0.12).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1902, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1902, 0.22).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1917, 0.48).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1921, 0.68).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1936, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1936, 0.9).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1956, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1956, 0.74).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1989, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1989, 0.42).
narrative_ontology:measurement(vanguard_rupture_reading_su_t2021, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 2021, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Communist Manifesto's revolutionary method' conflates three structurally distinct constraints about how class power is constituted: the vanguard-seizure reading (this file), the democratic-gradualism reading, and the council-communist reading. Each has its own epsilon, beneficiary/victim structure, and enforcement profile; the vanguard reading's epsilon is highest because its distinctive content is the suppression of the other two pathways. Family links run through network.affects_constraints; the shared upstream text (the Manifesto's insurrectionary passages) is cited by all three, which is why the upstream claim functions as evidence for the downstream contested one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
