% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Council Communist Reading: Federated Workers' Councils Replacing State and Party
 *   domain: political/economic
 *
 * SUMMARY:
 *   This story instantiates the council communist reading of the
 *   manifesto_revolutionary_method kernel: revolutionary transformation is
 *   achieved not by seizing or constructing a state apparatus (vanguard
 *   rupture reading) nor by winning parliamentary majorities within existing
 *   state structures (democratic gradualism reading), but by federated
 *   workplace assemblies directly holding power through mandated, recallable
 *   delegates, dissolving both capitalist state and revolutionary party into
 *   horizontal coordination among producers. Internally, within the council
 *   structure itself, this reading claims low extraction (ε=0.25) —
 *   coordination is close to peer-to-peer, mandates are binding, recall is
 *   immediate, and no standing bureaucratic layer captures surplus
 *   decision-making authority. The high suppression figure (0.72) is not
 *   internal to the council form; it measures the external suppression this
 *   reading has faced historically and continues to face from rival readings
 *   and their state apparatuses (Bolshevik suppression of independent
 *   soviets, SPD suppression of German Räte, Portuguese state
 *   re-consolidation after 1975), which is the structural reason council
 *   formations have rarely persisted long enough for their steady-state
 *   properties to be tested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.72).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Reading: Federated Workers' Councils Replacing State and Party").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political/economic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'e84bf8b1-6f12-4beb-b3e4-486aa2c11e26').
narrative_ontology:cs_kernel_codification('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', fixed_text).
narrative_ontology:cs_authority_grounding('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', practice).
narrative_ontology:cs_interpretation_layer_present('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26').
narrative_ontology:cs_reading_relation('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', foundational, no_standing_authority_above_recallable_mandate).
narrative_ontology:cs_axiom_status(no_standing_authority_above_recallable_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', no_standing_authority_above_recallable_mandate, deontological).
narrative_ontology:cs_axiom('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', foundational, party_form_reproduces_bureaucratic_separation).
narrative_ontology:cs_axiom_status(party_form_reproduces_bureaucratic_separation, holdable).
narrative_ontology:cs_axiom_grounding('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', party_form_reproduces_bureaucratic_separation, empirically_contingent).
narrative_ontology:cs_reference_frame('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', workers_self_emancipation_without_standing_authority).
narrative_ontology:cs_drift_state('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', post_1921_kronstadt_suppression, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e84bf8b1-6f12-4beb-b3e4-486aa2c11e26', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, shop_floor_delegates).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_assembly_participants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, professional_union_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, shop_floor_delegates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold direct decision-making power over production and distribution through mandated, recallable workplace delegates. They set the agenda of the council directly rather than through intermediaries, and can recall delegates immediately if they diverge from the base. Their exit option is federation with other councils rather than dependence on any central authority — they can reconstitute coordination bodies horizontally if a given federation structure fails them.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% Elected and instantly recallable representatives who carry mandates from their workplace assembly to the federated council level. They bear the cost of accountability directly — a delegate who drifts from the base's mandate is removed without institutional buffer or appeal, unlike party officials or bureaucrats.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, shop_floor_delegates, agenda_setter,
    moderate, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, shop_floor_delegates, payer).

% Administer the existing capitalist state apparatus — tax collection, policing, regulatory enforcement, civil administration. Under this reading, their entire institutional function is dissolved rather than captured or repurposed; there is no seat for them in a council system built on direct workplace mandate. Their professional identity and material position both depend on the state form this reading eliminates.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Occupy leadership positions in a centralized revolutionary party premised on organizing, educating, and eventually governing on behalf of the working class. This reading holds that any transitional party-state reproduces bureaucratic command rather than abolishing it, and so treats party officialdom itself as a structure to be dissolved into the councils, not preserved as a directing layer above them. They can attempt to enter council structures as ordinary delegates, but lose the standing office and command function that constituted their prior role.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    organized, biographical, constrained, national).

% Salaried union officials whose function is negotiating on behalf of members through formal collective bargaining machinery. Council communism holds that this layer's institutionalized separation from the shop floor makes it structurally analogous to the state and party forms it targets; their negotiating function is displaced by direct assembly decision-making.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, professional_union_apparatus, payer,
    organized, biographical, constrained, national).

% Workers not yet integrated into any workplace assembly or council structure — precarious, informal, or dispersed labor. They would have a stake in whether federation actually reaches them, but the council structure as described coordinates existing organized workplaces first; whether it extends to them depends on capacities this reading does not fully specify.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, unorganized_workers, excluded,
    powerless, immediate, trapped, local).

% Vanguardist and social-democratic currents contesting the same revolutionary terrain. They are not consulted within this reading's own account of legitimate authority — from this reading's standpoint their claims to represent the working class are exactly what the council form is built to bypass. They actively work to suppress or absorb council formations wherever they hold organizational leverage.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rival_revolutionary_currents, excluded,
    organized, generational, constrained, national).

% Evaluate the historical record of council formations (1917-21 Russia, 1918-19 Germany, 1956 Hungary, 1970s Portugal) against the claims made for direct democratic self-management, assessing whether councils crushed by external force represent a viable sustained alternative or a transitional moment always defeated before its structural properties could stabilize.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, historical_materialist_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production and social reproduction directly among workers through federated, mandate-bound, instantly recallable delegate assemblies — solving the problem of how large-scale economic coordination can occur without reconstituting a standing state or party apparatus that separates decision-making from those who carry out the decisions.
% TRANSFER_FUNCTION: Moves decision-making authority, control over production, and social surplus away from state bureaucratic and party-official strata and directly into workplace assemblies; nothing is extracted from workers to sustain a separate governing class because no such class is reconstituted by design.
% ABSENT_VOICES: Unorganized, precarious, and dispersed workers not yet integrated into any workplace assembly have no guaranteed channel into the federation until they organize a base unit of their own; rival revolutionary currents (vanguardist and social-democratic) are excluded from adjudicating legitimacy under this reading's own terms, since their claim to represent workers is precisely what the council form displaces.
% DISAPPEARANCE_RATIONALE: Within this reading's own account, if council structures disappeared the world would rearrange sharply — production coordination would revert to either capitalist management or reconstituted party-state command, both of which this reading treats as regressions. But the reading's critics (both rival readings within this kernel) hold that councils have never persisted long enough to demonstrate they are a stable steady-state rather than a transitional moment that inevitably resolves into either state or market coordination, making the counterfactual itself disputed territory.
% FOUNDING_PROBLEM: The founding problem is the reproduction of bureaucratic separation between rulers and ruled under both capitalist state administration and vanguard party rule — the observation that seizing existing state machinery (or building a substitute party-state) recreates a governing stratum with interests distinct from and often opposed to the working class it claims to represent.
% FOUNDING_PROBLEM_CORROBORATION: Historical materialist analysts outside the council-communist tradition (e.g. historians of the German and Russian council movements, comparative studies of the Kronstadt rebellion and the German Räte-system's suppression by the SPD-led state) corroborate that council formations arose from genuine grievances against both capitalist administration and party bureaucratization, and that in every major instance the councils were dissolved or subordinated by external force before their capacity for sustained self-governance could be empirically tested — so the founding problem's continued relevance is attested from outside the tradition, but its resolution (whether councils can hold power durably) remains unobserved rather than confirmed.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, contested).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.25) and essentially flat because the reading's own structural claim is that mandate-and-recall coordination prevents the accumulation of separate governing interests — there is no delegate stratum insulated enough from its base to extract durable rents. Suppression rises over the interval (0.45 to 0.72) tracking the historical pattern: each subsequent generation of council attempts faced more sophisticated and better-organized suppression from both state and vanguard-party apparatuses that had learned from prior council uprisings (1917-21 informing 1956 and 1975 responses). Theater ratio stays low and nearly flat (0.10 to 0.15) because this reading, where it has been tried, has tended toward genuine functional assemblies rather than performative ones — the crushing of councils has generally come from outside rather than internal hollowing-out.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and shop-floor delegates sit near the beneficiary end: they hold direct decision authority and can exit into re-federation if a given structure fails, giving them low derived directionality. State bureaucrats, vanguard party officials, and the professional union apparatus sit near the target end: their institutional function and standing office are precisely what this reading eliminates, and their exit options are constrained or trapped because their material position depends on the very structures being dissolved. This is a genuine structural asymmetry, not an override — the reading's core claim IS that these strata are made structurally superfluous, not merely inconvenienced.
 *
 * MANDATROPHY ANALYSIS:
 *   The council form is explicitly the reading's answer to mandatrophy: the founding problem (bureaucratic separation of rulers from ruled) is treated as recurring under BOTH capitalist administration and vanguard party rule, and the proposed fix (recallable, mandate-bound delegates with no standing office) is designed precisely to prevent the arrangement from outliving its coordinating function and calcifying into a new extractive layer. Whether this design succeeds in practice over a full historical cycle is exactly the empirical question the interval's suppression trajectory cannot answer, since every tested instance was suppressed externally before an internal mandatrophy process (if any) could be observed to run its course.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_stability_untested,
    'Would federated workplace councils, if not suppressed externally, stabilize as a durable low-extraction coordination form, or would internal pressures (scale problems, free-rider dynamics in federation, informal leadership calcifying into a new bureaucratic stratum) eventually reproduce the same separation between decision-makers and producers this reading claims to abolish?',
    'A sustained historical instance of council governance surviving multiple decades without external military or political suppression would allow direct observation of whether internal mandatrophy occurs; no such case exists in the historical record to date.',
    'If councils prove internally stable, the low ε=0.25 claim is vindicated as a genuine structural property rather than an artifact of short observation windows. If they prove internally unstable and reproduce bureaucratic separation once external pressure is removed, this reading''s central claim collapses toward the vanguard rupture reading''s critique that some standing coordinating layer is unavoidable at scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(council_stability_untested, empirical, 'Whether federated councils are structurally stable or merely untested due to universal external suppression.').

omega_variable(
    scale_federation_coordination_gap,
    'Can horizontal federation of workplace assemblies actually coordinate a modern industrial or post-industrial economy at national or transnational scale, or does the coordination function this reading assigns to federation require reintroducing centralized planning capacities functionally equivalent to a state?',
    'Comparative analysis of federation coordination mechanisms attempted historically (Spanish CNT-FAI collectives 1936-39, Yugoslav workers'' self-management) against the coordination demands of complex supply chains and cross-regional resource allocation.',
    'If federation genuinely scales without reconstituting centralized administrative power, the reading''s claim to dissolve the state entirely is structurally supported. If it does not scale, some of what this reading calls ''federation'' functionally resembles the coordinating apparatus of the state form it claims to replace, narrowing the structural distance between this reading and its rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_federation_coordination_gap, conceptual, 'Whether horizontal federation can substitute for state-scale coordination without reproducing state-like functions.').

omega_variable(
    framing_kernel_versus_institution,
    'Is the correct unit of analysis the manifesto''s revolutionary-method kernel (a single contested textual/doctrinal commitment with three readings), or should this reading instead be evaluated as its own free-standing institutional proposal independent of manifesto exegesis?',
    'Track whether council-communist practice historically derives its legitimacy claims from textual fidelity to the founding manifesto tradition versus from independent workers''-council practice that predates and is largely indifferent to that textual lineage (e.g. Pannekoek''s explicit departure from Leninist textual authority).',
    'If the kernel framing is adopted, this reading''s legitimacy is partly bound to interpretive fidelity to a shared founding text, and cs_structure fields (reading_relations, axioms) meaningfully capture the contest. If the institutional framing is adopted instead, the constraint should be evaluated purely on structural/empirical grounds independent of any kernel, and the kernel apparatus itself would be a mischaracterization of a movement that largely rejected textual authority in favor of practice-based legitimacy (authority_grounding: practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_kernel_versus_institution, conceptual, 'Whether council communism is best modeled as a kernel-reading (textual contest) or a free-standing practice-grounded institutional claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(mani_tr_t1936, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1936, 0.12).
narrative_ontology:measurement(mani_tr_t1956, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1956, 0.13).
narrative_ontology:measurement(mani_tr_t1975, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(mani_tr_t2000, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(mani_tr_t2026, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(mani_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.18).
narrative_ontology:measurement(mani_be_t1936, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(mani_be_t1956, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1956, 0.22).
narrative_ontology:measurement(mani_be_t1975, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1975, 0.24).
narrative_ontology:measurement(mani_be_t2000, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(mani_be_t2026, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 2026, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.45).
narrative_ontology:measurement(mani_su_t1936, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1936, 0.55).
narrative_ontology:measurement(mani_su_t1956, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1956, 0.65).
narrative_ontology:measurement(mani_su_t1975, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement(mani_su_t2000, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(mani_su_t2026, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the manifesto's revolutionary method' per the ε-invariance principle: council_communist_reading (this file, ε=0.25 internal, rope claim), vanguard_rupture_reading (party-state seizure, expected higher internal extraction as party officialdom is retained as a standing stratum), and democratic_gradualism_reading (electoral/institutional reform, expected lowest suppression but contested whether it achieves the founding problem's resolution at all). Each carries its own beneficiary/victim structure and its own ε; they are linked here rather than merged because measuring 'the revolutionary method' by the council-form observable versus the party-form observable versus the electoral-form observable yields three different, non-reconcilable ε values — the classic signal for decomposition rather than a single parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
