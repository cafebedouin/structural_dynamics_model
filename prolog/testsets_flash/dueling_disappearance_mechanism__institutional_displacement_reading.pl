% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling's Decline via Institutional Displacement
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint story models the decline of dueling as a primary
 *   dispute-resolution mechanism due to the rise of more effective and less
 *   costly institutional alternatives like courts, libel laws, and commercial
 *   arbitration. This 'institutional displacement' reading posits that
 *   dueling, as a form of coordination, was simply outcompeted, rather than
 *   actively suppressed or culturally repudiated. It remained an available,
 *   albeit increasingly disfavored, option in the gaps of the new
 *   institutional landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.25).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling's Decline via Institutional Displacement").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, 'bd896ee3-8359-4bc9-875c-fae336656831').
narrative_ontology:cs_kernel_codification('bd896ee3-8359-4bc9-875c-fae336656831', implicit).
narrative_ontology:cs_authority_grounding('bd896ee3-8359-4bc9-875c-fae336656831', practice).
narrative_ontology:cs_reading_relation('bd896ee3-8359-4bc9-875c-fae336656831', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd896ee3-8359-4bc9-875c-fae336656831', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('bd896ee3-8359-4bc9-875c-fae336656831', foundational, dispute_resolution_is_a_functional_market).
narrative_ontology:cs_axiom_status(dispute_resolution_is_a_functional_market, holdable).
narrative_ontology:cs_axiom_grounding('bd896ee3-8359-4bc9-875c-fae336656831', dispute_resolution_is_a_functional_market, empirically_contingent).
narrative_ontology:cs_axiom('bd896ee3-8359-4bc9-875c-fae336656831', foundational, institutional_efficiency_drives_adoption).
narrative_ontology:cs_axiom_status(institutional_efficiency_drives_adoption, holdable).
narrative_ontology:cs_axiom_grounding('bd896ee3-8359-4bc9-875c-fae336656831', institutional_efficiency_drives_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('bd896ee3-8359-4bc9-875c-fae336656831', dueling_as_primary_dispute_resolution).
narrative_ontology:cs_drift_state('bd896ee3-8359-4bc9-875c-fae336656831', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bd896ee3-8359-4bc9-875c-fae336656831', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, disputants_seeking_resolution).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, emerging_legal_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, banking_and_commerce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with grievances who historically might have resorted to dueling, but increasingly find more effective and less risky avenues for resolution through formal legal or commercial channels.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, disputants_seeking_resolution, beneficiary,
    moderate, biographical, mobile, local).

% Courts, libel laws, and other formal legal mechanisms that offered a more reliable, less violent, and increasingly legitimate means of resolving disputes over honor, property, and reputation, thereby outcompeting dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, emerging_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Commercial interests that benefited from a more stable and predictable dispute resolution environment, where conflicts did not escalate into violence that could disrupt business or endanger key personnel. They actively supported the development of alternative legal mechanisms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_and_commerce, beneficiary,
    organized, generational, mobile, regional).

% Individuals who, by tradition or personal conviction, still saw dueling as a legitimate means of defending honor, but found its social and legal costs increasing as alternatives gained prominence, pushing it to the social fringe.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_practitioners, payer,
    powerless, biographical, constrained, local).

% Scholars who analyze the historical forces that led to the decline of dueling, focusing on the interplay between legal, economic, and social changes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a structured, albeit violent, protocol for resolving disputes over honor and reputation, ensuring that grievances were addressed according to a recognized code.
% TRANSFER_FUNCTION: Transferred the social function of dispute resolution from a violent, personal code of honor to formal, institutionalized legal and commercial mechanisms, shifting the costs and risks from individuals to the state and legal system.
% ABSENT_VOICES: The 'honor culture' that once legitimized dueling, which would argue that institutional mechanisms cannot fully address matters of personal honor and courage. Their voice is now largely historical, not contemporary.
% DISAPPEARANCE_RATIONALE: If the institutional alternatives (courts, libel laws) had not emerged or had disappeared, dueling would likely have persisted as a more central, rather than fringe, mechanism for dispute resolution, particularly in matters of honor. The social landscape of conflict resolution would be fundamentally different.
% FOUNDING_PROBLEM: The need for a formalized, predictable, and socially sanctioned method for individuals to resolve serious disputes, particularly those involving honor, in a manner that upheld social standing and prevented uncontrolled vendettas.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists widely corroborate that the specific problem dueling addressed (honor disputes via personal combat) is now largely handled by other institutions, rendering dueling's original function obsolete. No contemporary beneficiaries of dueling attest to its founding problem being live.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because dueling was a voluntary, albeit high-stakes, coordination mechanism; its decline was driven by the superior utility of alternatives, not by direct extraction. Suppression is also low (0.25) because while dueling became illegal, its decline was more about obsolescence than active enforcement. The theater ratio is negligible (0.05) as there was little performative maintenance of dueling's function once alternatives emerged. Accessibility collapse is moderate (0.6) as alternatives became widely available, but dueling never fully disappeared, persisting in certain social niches. Resistance is low (0.05) because the shift was largely voluntary for most disputants.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the emerging legal system, dueling was an inefficient and dangerous relic being replaced by progress. From the perspective of dueling practitioners, it was a loss of a legitimate means of defending honor, even if they increasingly chose alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Disputants and commercial interests are beneficiaries, as they gained from the stability and predictability of institutional alternatives. The emerging legal system is an agenda-setter, actively shaping the new dispute resolution landscape. Dueling practitioners are payers, as they bore the increasing social and legal costs of adhering to a declining practice. The overall shift was a net benefit for society, making dueling's decline a 'rope' that was superseded by a more efficient 'rope'.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (dispute resolution) did not atrophy; rather, its function was absorbed and improved upon by new institutions. The 'mandatrophy resolved' flag is not set because the problem itself was not resolved, but the mechanism for addressing it evolved. This prevents mislabeling the decline as a 'piton' (inertial persistence) or 'snare' (coercive extraction) when it was a functional displacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_institutions,
    'To what extent was the decline of dueling primarily driven by the emergence of institutional alternatives, versus other factors like cultural shifts or direct legal prohibition?',
    'Comparative historical analysis across different societies with varying legal and cultural trajectories regarding dueling, to isolate the effect of institutional development.',
    'If institutional displacement was the dominant cause, this reading''s ''rope'' classification holds. If cultural shifts or direct prohibition were more primary, the classification might lean towards ''snare'' (if prohibition was highly coercive) or a different ''rope'' (if cultural shifts were voluntary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_institutions, empirical, 'Determining the primary causal driver of dueling''s decline.').

omega_variable(
    dueling_as_available_option,
    'How ''available'' was dueling as an option in the later stages of its decline, and what were the actual social and legal costs of choosing it?',
    'Detailed micro-historical studies of individual cases of dueling in the late 19th and early 20th centuries, examining the consequences for participants.',
    'If dueling remained a genuinely viable, albeit disfavored, option with moderate costs, this reading''s ''rope'' classification is strengthened. If costs were prohibitive and suppression was high, it might suggest a ''snare'' or a more coercive ''tangled_rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dueling_as_available_option, empirical, 'Assessing the true ''exit options'' for dueling practitioners in the late period.').

omega_variable(
    framing_underdetermination_dueling_decline,
    'Is the ''institutional displacement'' framing the most appropriate lens for understanding dueling''s decline, or do alternative framings (cultural contraction, overdetermined composite) offer a more complete or accurate account?',
    'A meta-analysis of historical and sociological scholarship, evaluating the explanatory power and empirical support for each competing reading.',
    'If an alternative framing is adopted, the classification of dueling''s decline might shift. For example, the ''contraction_reading'' might emphasize a different set of beneficiaries (those who benefit from a less violent culture) and a different mechanism of change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_dueling_decline, conceptual, 'Underdetermination of the causal framing for dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.2).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.18).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.28).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.2).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
