% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Gate on Statehood
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   Under the constitutive reading, a polity becomes a state only when
 *   existing states recognize it; satisfying the four Montevideo criteria
 *   (permanent population, defined territory, effective government, capacity
 *   for foreign relations) is necessary but never sufficient. The standing
 *   arrangement under contest — and the sole epsilon referent for this file —
 *   is that recognition-gated regime: treaty participation, UN and IFI
 *   membership, market access, passport validity, and contractual
 *   enforceability all run through recognition acts that incumbent states
 *   confer or withhold at discretion. Polities meeting every objective
 *   criterion (Somaliland, Transnistria, Northern Cyprus, and historically
 *   others) remain outside the legal universe, while their residents carry
 *   the costs in mobility, finance, and legal protection. Per the
 *   committer-frame discipline, this file authors ONLY the constitutive
 *   reading as a clean, epsilon-invariant constraint: the declaratory and
 *   hybrid readings are separate constraints in separate files, linked
 *   through the network section, and no averaging or hedging across readings
 *   appears here.
 *
 * KEY AGENTS:
 *   - - incumbent_recognized_states: agenda-setter and beneficiary (institutional/arbitrage) — administers recognition bilaterally and through multilateral admission, collects border stability and membership control
 *   - - great_power_gatekeepers: primary beneficiary (powerful/arbitrage) — their consent effectively controls multilateral admission; they convert recognition discretion into geopolitical leverage
 *   - - unrecognized_de_facto_regimes: primary target (moderate/trapped) — control territory and populations but are denied legal personality despite meeting the objective criteria
 *   - - populations_of_unrecognized_territories: primary target (powerless/trapped) — bear denied documents, financing exclusion, and absence from treaty protections
 *   - - self_determination_movements: excluded voice (powerless/trapped) — would argue for criteria-based inclusion but have no seat in the admission process except through sponsoring states
 *   - - multilateral_admission_bodies: administering gate (institutional/constrained) — run the admission votes but are bound by member-state control
 *   - - international_law_doctrine: analytical observer (non-agent) — the scholarly corpus recording and contesting the two theories; listed for completeness, excluded from directionality as a non-agent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.7).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Gate on Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7').
narrative_ontology:cs_kernel_codification('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', formalized).
narrative_ontology:cs_authority_grounding('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', practice).
narrative_ontology:cs_interpretation_layer_present('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7').
narrative_ontology:cs_reading_relation('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', foundational, recognition_necessary_for_legal_personality).
narrative_ontology:cs_axiom_status(recognition_necessary_for_legal_personality, holdable).
narrative_ontology:cs_axiom_grounding('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', recognition_necessary_for_legal_personality, conventional).
narrative_ontology:cs_axiom('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', secondary, recognition_discretion_sovereign_prerogative).
narrative_ontology:cs_axiom_status(recognition_discretion_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', recognition_discretion_sovereign_prerogative, conventional).
narrative_ontology:cs_reference_frame('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', recognition_gated_membership_order).
narrative_ontology:cs_drift_state('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', contemporary_post_kosovo_opinion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cd2e2d1e-9a92-4956-bfc7-3797ee99ebc7', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, incumbent_recognized_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, great_power_gatekeepers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_de_facto_regimes).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_of_unrecognized_territories).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, territorial_integrity_norm).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, westphalian_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Confer or withhold recognition through bilateral declarations and multilateral admission votes, and maintain the shared expectation that unrecognized entities cannot sign treaties, join organizations, or issue internationally honored documents. Each retains full freedom over its own recognition decisions and suffers no denial of personality itself; what flows to them is a stable map of permissible counterparties and insulation of existing borders from unilateral revision.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, incumbent_recognized_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, incumbent_recognized_states, beneficiary).

% A handful of states whose consent effectively decides multilateral admission and whose non-recognition campaigns other states broadly follow. They trade recognition and non-recognition for concessions, basing rights, votes, and alignment, and their own clients and protégés are reliably admitted while adversaries' protégés are reliably blocked. Withholding recognition costs them nothing they cannot replace; granting it is a resource they allocate strategically.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_power_gatekeepers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, great_power_gatekeepers, agenda_setter).

% Govern territories, collect taxes, run courts and schools, and satisfy every objective criterion of statehood, yet cannot accede to treaties, borrow from international financial institutions, join technical bodies, or obtain enforceable judgments abroad. Their officials travel on documents most states do not honor; their firms bear risk premiums that recognized competitors do not. Mutual recognition exchanged among similarly situated entities unlocks almost nothing, because the resources behind the gate are held by the states refusing to recognize them. Leaving the international system entirely is not an option — their economies and security depend on it.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_de_facto_regimes, payer,
    moderate, biographical, trapped, regional).

% Live under administrations whose status blocks ordinary life at the margins: passports and diplomas not honored abroad, bank transfers routed through intermediaries at added cost, investment deterred by unenforceable contracts, humanitarian and development programs structured around their territory's legal limbo. They chose none of the arrangement and have no channel to contest it except through the very governments whose recognition campaigns produced it.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_of_unrecognized_territories, payer,
    powerless, biographical, trapped, local).

% Peoples seeking independent statehood who would argue for criteria-based automatic inclusion — meet the four tests, gain legal personality — but hold no seat in any admission process. They reach the conversation only through sponsoring states willing to spend capital on their behalf, and the prevailing reading treats their aspirations as matters for the parent states' consent rather than their own showing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, self_determination_movements, excluded,
    powerless, generational, trapped, regional).

% Run the formal admission machinery — security council recommendation, general assembly vote, membership of technical agencies — under rules that give a single permanent-member objection blocking force. They administer the gate faithfully but control neither its criteria nor its outcomes; member states set both, and the bodies cannot admit an entity the gatekeepers decline to pass.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, multilateral_admission_bodies, agenda_setter,
    institutional, civilizational, constrained, global).

% The scholarly and judicial corpus — treatises, advisory opinions, commission reports — that records both theories, notes that the declaratory position is codified in the Montevideo Convention's own third article, and observes that practice runs constitutive anyway. It attests and critiques but collects nothing and decides nothing; it is retained in the record for completeness and excluded from any computation over actors.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_doctrine, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(montevideo_statehood_criteria__constitutive_reading, international_law_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, great_power_gatekeepers).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one shared, settled answer to the question 'who may act as a state' for the entire international system: every treaty signature, embassy exchange, loan agreement, and airline landing right presupposes a determinate counterparty, and the recognition rule settles that determination once system-wide instead of case-by-case in each interaction.
% TRANSFER_FUNCTION: Moves legal personality, treaty capacity, financing access, and document validity away from polities lacking recognition and concentrates allocation discretion over those goods in the hands of incumbent governments — above all the small set of gatekeeper states whose consent controls multilateral admission.
% ABSENT_VOICES: The residents of unrecognized territories and the self-determination movements seeking statehood are the structurally absent voices: no seat, no vote, no treaty capacity, heard only through sponsoring states. They would argue for criteria-based automatic inclusion and against a rule that makes their status a gift of the governments they are trying to leave. Their absence is not incidental — the admission machinery is composed entirely of incumbents.
% DISAPPEARANCE_RATIONALE: If the recognition gate vanished overnight, every de facto regime meeting the objective criteria would immediately assert full statehood, dozens of frozen conflicts would reopen as legal questions, treaty systems would face a flood of new counterparties with unsettled obligations, and every existing border would become negotiable by whichever movement could field an effective administration. The entire architecture of membership, admission, and territorial settlement would have to be rebuilt — the world does not stay the same without this rule.
% FOUNDING_PROBLEM: After the collapse of dynastic empires and the wave of revolutions and decolonizations, the society of states needed a rule for deciding which new polities count as sovereign members — without fighting a war over each claim and without treating every effective territorial control as automatically entitled to full membership.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the doctrinal literature (Crawford's treatment of state creation; Lauterpacht's own argument that recognition duties attach to facts shows the tradition's internal critics), the International Court of Justice's Kosovo advisory opinion recording both theories as live, and the Montevideo Convention's signatories — including the United States' reservation expressly rejecting the constitutive implication — all attest that the membership problem is real and recurring. No source outside the incumbent states asserts that the problem is dead; the dispute is over the solution, not the problem.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.62 for the standing recognition-gated arrangement as this reading assesses it: the burden is concentrated on a minority of trapped seats but severe in kind (total exclusion from treaty capacity, financing, and legal personality rather than a marginal fee). Suppression is authored at 0.70 as a RAW STRUCTURAL property, unscaled by power or scope: maintaining the gate requires organized non-recognition campaigns, pressure on third states considering recognition, and blocking of admission votes — the arrangement does not persist by participant preference. Theater ratio is moderate-low (0.28): the gate performs real work (settling counterpart status for billions of daily interactions), but a growing share of activity is symbolic — ceremonial recognitions, non-recognition statements aimed at domestic audiences, recognition exchanged for concessions with no operational content. Accessibility collapse is 0.50: alternatives genuinely persist (the declaratory doctrine is codified in Montevideo Article 3, de facto relations and informal cooperation channels operate around the gate, observer statuses exist), so understanding the gate does not eliminate every workaround. Resistance is 0.55: unrecognized polities lobby, litigate, seek advisory opinions, and exchange mutual recognitions among themselves, but escalation is muted (see the internalized_futility omega). The measurement series run on ONE SHARED GRID (t=0..100 at intervals of 20) with all three metrics authored at every point: base extractiveness climbs from 0.40 to 0.62 as recognition shifted from a post-imperial housekeeping act to a weaponized instrument of bloc politics and secession management; suppression_requirement climbs from 0.42 to 0.70 as organized non-recognition campaigns institutionalized; theater_ratio rises from 0.10 to 0.28 as symbolic recognition grew. The trajectory is monotonic rather than cyclical — enforcement machinery accumulated rather than oscillated.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the incumbent_recognized_states seat, the arrangement presents as the modest price of orderly coexistence — a membership rule every club needs, experienced as rope-like coordination. From the trapped unrecognized seats, the identical structure presents as a closed gate that converts full compliance with objective criteria into nothing — experienced as pure denial with a coordination cover story. Same-level actor dynamics sharpen the gap among nominally equal sovereigns: great powers hold arbitrage-grade exit (they choose recognition strategically and suffer no denial themselves), while small recognized states are constrained followers who must align with bloc leaders under pressure and cannot independently open the gate for anyone. The engine computes per-seat classifications from the declared power, exit, and directional data; this commentary explains why the divergence is structural rather than notational.
 *
 * DIRECTIONALITY LOGIC:
 *   incumbent_recognized_states and great_power_gatekeepers are declared beneficiaries: the gate subsidizes them with border stability, membership control, and veto leverage, placing them near the beneficiary end of directionality (low d, damped or inverted effective burden). unrecognized_de_facto_regimes and populations_of_unrecognized_territories are declared victims with trapped exit: they cannot leave the international system, form no alternative legal universe, and sit near the full-target end (high d, amplified effective burden). multilateral_admission_bodies administer the gate without collecting its product — near-symmetric administrators whose position derives from member-state control. international_law_doctrine is authored with agent:false precisely so the doctrine feeds no directionality computation: a proposition collects nothing and must not masquerade as a beneficiary. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct qualitative placement for every seat, and the two beneficiary seats are differentiated by power atom (institutional vs powerful) rather than by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling membership of the society of states after imperial collapse and revolution without war over every claim — is LIVE, not dead: new secession crises, contested annexations, and de facto regimes keep regenerating it. The R5 mismatch consumer therefore reads status=live paired with verdict=world_rearranges: a consistent profile, no zombie flag. Mandatrophy discipline matters here in both directions. Labeling the arrangement a pure snare would erase the real coordination service (a single shared answer to 'who may act as a state' settles counterpart status once for the entire system, and the mass-recognition episodes show that service operating even against some incumbents' wishes). Labeling it a pure rope would erase the concentrated veto rents and the trapped minority paying for them. The tangled_rope claim holds both halves: genuine coordination function, asymmetric burden, active enforcement required to sustain the asymmetry. Theater is tracked separately (0.28 and rising slowly) so performative recognition is visible without being mistaken for the test of the whole structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the constitutive reading of the montevideo_statehood_criteria kernel: recognition by existing states is NECESSARY for legal personality. Where exactly does the sibling disagreement sit, and what would the sibling readings change structurally?',
    'The sibling files montevideo_statehood_criteria__declaratory_reading and montevideo_statehood_criteria__hybrid_reading carry the alternative structures; doctrinal analysis of state practice and opinio juris locates the live disagreement at the necessity-of-recognition premise.',
    'Under the declaratory sibling the victim set largely empties (polities meeting the four objective criteria gain legal personality automatically, and the incumbent veto dissolves); under the hybrid sibling the victim set shifts toward polities failing legitimacy tests rather than lacking recognition. This file''s epsilon, victims, and classification are valid only for the constitutive structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of the statehood-criteria kernel; siblings instantiate different constraints with different victim sets.').

omega_variable(
    naturalness_of_acknowledgment_gate,
    'Is some act of acknowledgment unavoidable in ANY legal order (making the recognition gate near-natural, an irreducible price of ordered membership), or is the incumbent veto a constructed privilege that identifiable agents maintain for their own benefit?',
    'Compare membership systems that operate on objective filing rather than incumbent consent (domestic incorporation registries, Montevideo-style declaratory operation). If functioning large-scale systems exist where status follows criteria satisfaction without incumbent sign-off, the gate is constructed rather than necessary.',
    'If constructed, pressure toward reclassification as pure extraction strengthens and the beneficiary declarations become the load-bearing fact; if near-natural, the residual burden on unrecognized polities is closer to the irreducible cost of having any membership rule at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_acknowledgment_gate, conceptual, 'Whether the acknowledgment requirement is a structural feature of legal order or an incumbent-maintained construct.').

omega_variable(
    attribution_of_unrecognized_deprivation,
    'How much of the material deprivation of unrecognized polities and their populations is caused by non-recognition itself, versus underlying conflict, poverty, or governance failure that would persist under any membership rule?',
    'Matched comparison of unrecognized de facto regimes against recognized states with comparable instability histories, isolating the recognition variable across financing access, treaty capacity, investment flows, and mobility documents.',
    'If non-recognition accounts for most of the measurable gap, the authored epsilon of 0.62 understates the burden on trapped seats; if deprivation is mostly endogenous, the arrangement''s marginal contribution is materially smaller than the victim declarations suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_of_unrecognized_deprivation, empirical, 'Causal share of non-recognition in the observed deprivation of unrecognized polities.').

omega_variable(
    veto_coordination_vs_border_defense,
    'Does the incumbent veto persist because it solves a genuine membership-coordination problem for the international system, or primarily because incumbent governments defend existing borders against secession and revision?',
    'Examine recognition episodes where coordination needs and incumbent interests diverge: the rapid mass recognition of post-Soviet and post-Yugoslav successor states despite some incumbents'' objections shows coordination logic operating; sustained non-recognition of economically useful partners (trade and investment continuing with formally unrecognized entities) shows interest logic overriding the coordination story.',
    'If border defense dominates, the arrangement slides from hybrid coordination-plus-burden toward pure extraction riding on a coordination cover story; if coordination dominates, the burden on unrecognized polities is bounded by the real service the gate performs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_coordination_vs_border_defense, conceptual, 'Relative weight of coordination function versus incumbent border-defense interest in sustaining the veto.').

omega_variable(
    internalized_futility_of_claims,
    'Is the comparatively low escalation of statehood claims by long-unrecognized polities structural (the gates are genuinely closed) or partly internalized (learned futility suppressing claim-making, as in Somaliland''s deliberate restraint about asserting independence from Somalia in order to preserve internal clan settlement)?',
    'Track claim activity after partial openings: if polities escalate claims when a door cracks (the wave of recognition-seeking after the Kosovo episode), suppression was structural; if claim-making stays quiet even as formal barriers loosen, internalization is carrying part of the load.',
    'An internalized component means the scalar suppression measure understates total suppressive force: exit remains locked in practice even where formal barriers ease, and removal of the formal gate would not immediately release pent-up claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_futility_of_claims, empirical, 'Structural versus internalized share of suppressed statehood claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_constitutive_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(montevideo_constitutive_tr_t20, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(montevideo_constitutive_tr_t40, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(montevideo_constitutive_tr_t60, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(montevideo_constitutive_tr_t80, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(montevideo_constitutive_tr_t100, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(montevideo_constitutive_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(montevideo_constitutive_be_t20, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(montevideo_constitutive_be_t40, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(montevideo_constitutive_be_t60, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(montevideo_constitutive_be_t80, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 80, 0.61).
narrative_ontology:measurement(montevideo_constitutive_be_t100, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_constitutive_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(montevideo_constitutive_su_t20, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(montevideo_constitutive_su_t40, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(montevideo_constitutive_su_t60, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(montevideo_constitutive_su_t80, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 80, 0.67).
narrative_ontology:measurement(montevideo_constitutive_su_t100, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'statehood criteria' conflates three structurally distinct claims with different epsilon values, victim sets, and failure modes. This file authors the constitutive reading (epsilon 0.62, referent: the standing recognition-gated arrangement; victims: unrecognized polities and their populations). The declaratory sibling (older codified baseline, Montevideo Article 3) carries negligible gate-extraction because no incumbent veto exists in its structure; the hybrid sibling relocates the victim set onto polities failing legitimacy tests. Upstream/downstream: the declaratory text is the codified baseline from which constitutive practice diverged, and constitutive recognition discretion is the structural opening within which hybrid conditionality operates (recognition-with-conditions presupposes discretionary recognition). All three files cross-link through network.affects_constraints; orphaning any one would sever the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
