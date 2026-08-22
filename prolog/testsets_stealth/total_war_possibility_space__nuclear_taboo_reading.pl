% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo: Constructed Normative Prohibition on Total War
 *   domain: international relations/strategic studies/institutional history
 *
 * SUMMARY:
 *   Eight decades without a second nuclear war is the datum this constraint
 *   organizes. On this reading, the standing arrangement under contest is the
 *   normative-prohibition complex itself: a constructed taboo on nuclear use,
 *   built by identifiable norm entrepreneurs and leader decisions, together
 *   with the enforcement machinery it generated — the non-proliferation
 *   regime, no-first-use pledges, and the stigma practices that mark use as
 *   categorically illegitimate rather than merely costly. The arrangement
 *   leaves total war materially possible but normatively foreclosed, and its
 *   persistence is claimed to be independent of the underlying capability
 *   balance. The extraction profile is real but bounded: the regime codifies
 *   a permanent asymmetry between arsenal-holding insiders and the majority
 *   who forgo the option, while the collective good it produces — non-use —
 *   is consumed by everyone, including its payers. KEY AGENTS (by structural
 *   relationship): nuclear_weapon_states — administrator-beneficiaries
 *   (institutional/identity_locked) who enforce the line and surrender every
 *   use-option; norm_entrepreneur_communities — constructors and maintainers
 *   (organized/identity_locked) collecting standing from the norm's survival;
 *   civilian_populations_globally — diffuse beneficiaries (powerless/trapped)
 *   receiving non-use without consenting to exposure; would_be_nuclear_states
 *   and non_nuclear_npt_parties — payers (powerful and organized
 *   respectively, both constrained) bearing foregone options and codified
 *   second-class status; extended_deterrence_dependents — sheltered
 *   beneficiaries (powerful/constrained); regime_outside_arsenal_states —
 *   excluded outsiders (powerful/mobile); strategic_studies_analysts —
 *   analytical observers.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: administrator-beneficiary (institutional/identity_locked) — makes every use-or-refrain decision, runs the regime, collects prestige and arsenal retention, surrenders all employment options
 *   - norm_entrepreneur_communities: constructor-beneficiary (organized/identity_locked) — builds and maintains the prohibition, collects standing and access, professionally fused with the norm's survival
 *   - civilian_populations_globally: diffuse beneficiary (powerless/trapped) — receives eight decades of non-use, bears unconsented exposure, no seat and no exit
 *   - would_be_nuclear_states: primary payer (powerful/constrained) — forgoes the deterrent option under sanction threat
 *   - non_nuclear_npt_parties: primary payer (organized/constrained) — bears the codified asymmetry, contests it collectively without winning
 *   - extended_deterrence_dependents: sheltered beneficiary (powerful/constrained) — protection without proliferation cost or stigma
 *   - regime_outside_arsenal_states: excluded outsider (powerful/mobile) — holds arsenals outside the bargain, barred from rewriting its terms
 *   - strategic_studies_analysts: analytical observer (analytical/analytical) — examines the arrangement without maintaining it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.48).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.45).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo: Constructed Normative Prohibition on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international relations/strategic studies/institutional history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '50612951-ef9a-4527-a360-4e5635888429').
narrative_ontology:cs_kernel_codification('50612951-ef9a-4527-a360-4e5635888429', distributed).
narrative_ontology:cs_authority_grounding('50612951-ef9a-4527-a360-4e5635888429', expertise).
narrative_ontology:cs_interpretation_layer_present('50612951-ef9a-4527-a360-4e5635888429').
narrative_ontology:cs_reading_relation('50612951-ef9a-4527-a360-4e5635888429', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('50612951-ef9a-4527-a360-4e5635888429', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('50612951-ef9a-4527-a360-4e5635888429', foundational, non_use_tracks_constructed_norm_not_capability).
narrative_ontology:cs_axiom_status(non_use_tracks_constructed_norm_not_capability, holdable).
narrative_ontology:cs_axiom_grounding('50612951-ef9a-4527-a360-4e5635888429', non_use_tracks_constructed_norm_not_capability, empirically_contingent).
narrative_ontology:cs_axiom('50612951-ef9a-4527-a360-4e5635888429', foundational, nuclear_use_categorically_barred_even_when_advantageous).
narrative_ontology:cs_axiom_status(nuclear_use_categorically_barred_even_when_advantageous, holdable).
narrative_ontology:cs_axiom_grounding('50612951-ef9a-4527-a360-4e5635888429', nuclear_use_categorically_barred_even_when_advantageous, deontological).
narrative_ontology:cs_reference_frame('50612951-ef9a-4527-a360-4e5635888429', taboo_constituted_non_use_order).
narrative_ontology:cs_drift_state('50612951-ef9a-4527-a360-4e5635888429', contemporary_multipolar_nuclear_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('50612951-ef9a-4527-a360-4e5635888429', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, civilian_populations_globally).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_communities).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, would_be_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_npt_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_dependents).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_norm_causation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine governments hold nuclear arsenals and together administer the restraint arrangement: their heads of state make every use-or-refrain decision, their militaries write the targeting plans that go unused, and their diplomats run the treaty conferences that police the line between legitimate and illegitimate nuclear behavior. Each refrains from employing weapons even in wars it has lost conventionally, and each collects the standing prestige and security that attach to arsenal possession under rules that forbid everyone else from following suit. Stepping off the arrangement — by using a weapon or by disarming — would cost them their seat at the table that writes the rules, and 'responsible stewardship' has become part of how these establishments understand what they are.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary).

% The historians, international lawyers, physicians' associations, and advocacy networks that built the prohibition and keep it visible — documenting decision points, drafting treaty language, convening humanitarian-consequence conferences. They collect standing, funding, and institutional access from the arrangement's continuation; several generations of careers and professional identities now rest on the norm surviving, and exit would mean repudiating their own life's work.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_communities, beneficiary,
    organized, generational, identity_locked, global).

% Everyone living under the shadow of the arsenals receives the arrangement's core output — eight decades without a second nuclear war — while bearing diffuse, uncompensated exposure: cities remain on targeting plans and no population ever consented to being hostage to someone else's restraint. They hold no seat in treaty conferences and no individual exit from planetary exposure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, civilian_populations_globally, beneficiary,
    powerless, generational, trapped, global).

% Governments with the industrial and scientific capacity to build weapons but barred from doing so by the regime the taboo anchors. They forgo the deterrent option their rivals' allies enjoy, accept inferior conventional positions or dependence on a patron's umbrella, and face sanctions if they cross the line. A few have paid the exit price and left the arrangement entirely; most stay inside under protest.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, would_be_nuclear_states, payer,
    powerful, generational, constrained, regional).

% The majority of states, bound by treaty to forgo indefinitely the weapons a handful of governments keep. They receive promised peaceful-technology cooperation and security assurances of varying credibility, and they bear the codified asymmetry — permanent second-class status in the nuclear order — which they contest collectively at five-year review conferences without ever winning. Their coalition leverage is real: it produced a competing ban treaty, but not a change in the bargain.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_npt_parties, payer,
    organized, generational, constrained, global).

% Allies that shelter under a patron's arsenal while publicly endorsing restraint. They receive protection without paying the proliferation cost or carrying the stigma, and their endorsement gives the arrangement a breadth it could not otherwise claim; their own security planning quietly depends on the weapons they forswear, which makes open exit costly in both directions.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_dependents, beneficiary,
    powerful, biographical, constrained, regional).

% Governments holding arsenals wholly outside the treaty — three that never signed and one that withdrew. They are barred from the arrangement's decision councils, sanctioned for the arsenals they keep, and would rewrite the bargain's terms if ever seated; their bare existence is a standing argument that the line the arrangement polices can be crossed and survived.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, regime_outside_arsenal_states, excluded,
    powerful, generational, mobile, regional).

% Academic and governmental analysts who watch the arrangement and argue about why it holds. They take no side in its maintenance, command the archival and modeling tools for examining it, and include holders of rival causal accounts whose explanations this story does not adjudicate.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_studies_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recurring-total-war problem: gives every arsenal holder and every alliance a shared, mutually observable expectation that nuclear weapons will not be employed, converting an unstable multi-party brink into a bargained standoff, and gives non-weapon states a common framework for policing the line between legitimate and illegitimate nuclear behavior.
% TRANSFER_FUNCTION: Moves usable military option out of the hands of arsenal holders and would-be holders into a collective reserve nobody may spend; moves security dependence from non-weapon states toward the arsenal-holding patrons whose umbrellas they shelter under; and moves normative authority and agenda-setting access to the expert and advocacy communities that administer the prohibition's meaning.
% ABSENT_VOICES: Populations living under targeting plans never consented to their exposure and have no seat in review conferences. Governments outside the treaty are barred from rewriting terms they are sanctioned under. Holders of rival causal accounts of non-use are published and audible in academic venues but structurally sidelined in the forums where the arrangement's self-understanding is fixed.
% DISAPPEARANCE_RATIONALE: Crisis bargaining would lose its shared expectation of restraint within one confrontation cycle; alliance umbrellas would be repriced or abandoned; latent-capacity states would race to weaponize; and the first use would reset every threshold the arrangement currently holds closed. The world rearranges around whichever norm, or fear, replaces the prohibition.
% FOUNDING_PROBLEM: After 1945 the demonstrated capacity of nuclear weapons for civilizational destruction, combined with the prospect of recurring US-Soviet confrontations, posed a new problem: how to prevent a third world war fought with nuclear weapons from becoming a normal instrument of great-power rivalry.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the Bulletin of the Atomic Scientists' Doomsday Clock assessments, SIPRI's annual arsenal inventories, and the public-health literature on blast and fallout consequences — bodies with no stake in the arrangement's continuation. The arsenal-holding governments attest liveness in their own defense postures; no independent source attests the founding problem is dead.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.48 because the arrangement genuinely extracts — a codified, indefinitely extended asymmetry plus the surrender of every arsenal holder's employment options — while producing a collective good its payers also consume; it is neither negligible nor confiscatory. Suppression at 0.45 reflects an enforcement apparatus that has hardened over the interval (treaty verification, sanction regimes, stigma enforcement) but operates predominantly through reputational and internalized channels rather than physical coercion: the operative mechanism is professional socialization and diplomatic cost, not imprisonment. Theater_ratio at 0.30 captures the growing ceremonial share — review conferences that conclude without changing the bargain, no-first-use pledges unbacked by doctrine — atop a function that remains real. Accessibility_collapse at 0.5 is the signature of this reading: alternatives (use) remain materially accessible and are foreclosed normatively, so understanding the constraint closes off perhaps half the practical option space, not all of it. Resistance at 0.55 records sustained pushback: arsenal states refusing no-first-use commitments, opposing the ban treaty, and non-weapon coalitions contesting the asymmetry every cycle. The three measurement series share one nine-point grid (t=0..80, roughly 1945–2025) so every metric is authored at every examined time point; trajectories are monotonic rather than cyclical — enforcement machinery matured and hardened (rising suppression_requirement), asymmetry was codified and then locked in (rising extractiveness), and ceremonial maintenance accumulated (rising theater). Identity-lock operates on two seats: arsenal-holding establishments have fused with 'responsible stewardship' as institutional identity, and the entrepreneur communities have fused their careers and worldview with the norm's survival — on this reading, exit by either carrier is the leading indicator of decay.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administrator-beneficiary seat should compute differently. From the non_nuclear_npt_parties and would_be_nuclear_states positions the arrangement is enforced foreclosure: a bargain written by others, locking in second-class status under sanction threat. From the civilian and entrepreneur positions it is an achievement being defended. The nuclear_weapon_states seat straddles the divide — they administer and profit from the regime while being its most directly bound subjects, having surrendered every use-option their weapons nominally exist to provide. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real receipts: populations receive non-use, entrepreneurs receive standing and access, arsenal states receive legitimacy and preserved privilege. Victim declarations map to real costs: would-be states forgo the option under sanction threat, and treaty-bound majorities bear the codified asymmetry. One override is authored: the institutional power atom is assigned d=0.3 because the automatic derivation from nuclear_weapon_states' beneficiary declaration alone would place them near the full-beneficiary pole (~0.1), understating their cost-bearing — they are the arrangement's most directly bound subjects, surrendering every employment option, and their net position is beneficiary-with-substantial-cost rather than pure subsidy recipient. In this story only nuclear_weapon_states occupies the institutional atom, so the override keys cleanly to that seat. All other seats derive correctly from declaration plus exit structure: trapped diffuse beneficiaries near d=0, constrained payers near the target pole, the mobile excluded outsiders deriving from their outside position.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandate outliving function to resolve: the founding problem — preventing a third world war fought with nuclear weapons — remains live and is corroborated by sources outside the beneficiary set, so mandatrophy_resolved stays undeclared. The tangled-rope classification is what blocks the two standard mislabels. Reading the arrangement as pure coordination would erase the codified asymmetry its payers bear and the options its administrator surrenders; reading it as pure extraction would erase the genuine collective good — eight decades of non-use — that even its victims consume, and would mispredict why the payers stay: their coalition leverage produced a rival treaty, not mass exit, because exit forfeits the good along with the burden. The hybrid category holds both faces without letting either cover story absorb the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates one reading of the total_war_possibility_space kernel — that a constructed normative prohibition explains the non-use record independent of material capability. Sibling readings attribute the same record to mutual-vulnerability deterrence or to cognitive foreclosure. Which causal account does the record actually support?',
    'Process-tracing of use-decision points under favorable capability asymmetry (the US monopoly and superiority years) against rational-deterrence predictions; comparative archival work on whether leaders weighed normative or strategic variables at each decision point.',
    'If the deterrence reading prevails, the constraint''s persistence tracks capability balances rather than norm maintenance, its enforcement mechanisms are epiphenomenal, and classification shifts to whatever structure the deterrence arrangement itself exhibits; if the taboo reading prevails, weakening norm entrepreneurship becomes the leading indicator of failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which sibling reading of the total-war kernel the non-use record supports.').

omega_variable(
    capability_independence_claim,
    'Is non-use genuinely independent of material capability, or has assured-retaliation capability been silently doing the work the taboo claims credit for?',
    'Examine decision points where first use carried limited reprisal risk — the early US superiority window: sustained non-use there is strong taboo evidence; conversely, identify any point where normative and capability calculations diverged and capability won.',
    'If capability-dependence is established, the taboo is a redescription of deterrence outcomes and this reading collapses into its sibling; if independence holds in the low-reprisal window, the constructed-taboo claim is confirmed and the constraint''s maintenance dependencies are social rather than material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_independence_claim, empirical, 'Whether the prohibition operates independently of the underlying capability balance.').

omega_variable(
    norm_entrepreneur_exit_sensitivity,
    'Does the taboo weaken as its norm-entrepreneur carriers thin out or exit, as this reading specifically predicts?',
    'Track taboo-strength indicators (leader rhetoric, doctrine language, crisis behavior) against epistemic-community density and generational turnover inside nuclear establishments and the advocacy field.',
    'Confirmed sensitivity makes the constraint''s persistence contingent on a renewable social carrier — a maintenance dependency a pure capability account would not exhibit; a null result shifts explanatory weight toward the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_sensitivity, empirical, 'Whether the prohibition decays with the exit of its constructing and maintaining coalition.').

omega_variable(
    constructed_vs_emergent_origin,
    'Was the prohibition deliberately constructed by identifiable norm entrepreneurs, or did it emerge as spontaneous societal revulsion that entrepreneurs merely articulated?',
    'Archival tracing of specific rhetorical and doctrinal interventions — early use-or-refrain deliberations, movement campaigns, treaty drafting histories — to distinguish construction from articulation.',
    'If emergent, the taboo needs no maintenance coalition and is far more durable than this reading''s own fragility prediction implies; if constructed, the constraint requires continuous reproduction and carries higher persistence risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_emergent_origin, empirical, 'Whether the prohibition was built or merely named.').

omega_variable(
    use_bar_vs_possession_bar_decomposition,
    'Are the use-prohibition (the taboo proper) and the possession-asymmetry (the regime it anchors) one constraint or two with different extraction profiles?',
    'Author a sibling story isolating the possession regime and compare epsilon and seat structures across the family; if the use-bar alone shows near-zero extraction while asymmetry concentrates in the possession bar, the family splits.',
    'Splitting would move this story toward a lower-extraction, coordination-dominant profile and relocate the extraction finding to the possession-regime story; keeping them merged attributes the regime''s asymmetry to the taboo itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_bar_vs_possession_bar_decomposition, conceptual, 'Whether the use-bar and the possession-bar are one epsilon-invariant constraint or a two-member family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuclear_taboo_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nuclear_taboo_tr_t10, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(nuclear_taboo_tr_t20, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(nuclear_taboo_tr_t30, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(nuclear_taboo_tr_t40, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(nuclear_taboo_tr_t50, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(nuclear_taboo_tr_t60, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(nuclear_taboo_tr_t70, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 70, 0.29).
narrative_ontology:measurement(nuclear_taboo_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.3).

% Extraction over time
narrative_ontology:measurement(nuclear_taboo_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(nuclear_taboo_be_t10, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(nuclear_taboo_be_t20, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(nuclear_taboo_be_t30, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(nuclear_taboo_be_t40, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(nuclear_taboo_be_t50, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement(nuclear_taboo_be_t60, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(nuclear_taboo_be_t70, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 70, 0.47).
narrative_ontology:measurement(nuclear_taboo_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(nuclear_taboo_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(nuclear_taboo_su_t10, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(nuclear_taboo_su_t20, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(nuclear_taboo_su_t30, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(nuclear_taboo_su_t40, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(nuclear_taboo_su_t50, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement(nuclear_taboo_su_t60, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(nuclear_taboo_su_t70, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 70, 0.42).
narrative_ontology:measurement(nuclear_taboo_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why has total war not recurred?' decomposes into three structurally distinct constraints sharing the total_war_possibility_space kernel. This story (nuclear_taboo_reading) authors the normative-prohibition arrangement; deterrence_equilibrium_reading authors the mutual-vulnerability arrangement; space_contraction_reading authors the cognitive-foreclosure arrangement. Each has its own epsilon, beneficiaries, and victims; the family links exist so contamination and evidence propagate across the readings instead of being averaged inside any one of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
