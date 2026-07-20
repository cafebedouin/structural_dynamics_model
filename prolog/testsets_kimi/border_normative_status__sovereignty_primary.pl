% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Border Norm: State Authority to Exclude Non-Members
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_primary reading of the
 *   border_normative_status kernel: the claim that territorial boundaries are
 *   legitimate instruments of collective self-determination and that states
 *   possess foundational authority to exclude non-members. Under this
 *   reading, border enforcement is a legitimate core state function, citizen
 *   populations are the primary beneficiaries of closure, and the harms
 *   experienced by excluded migrants are treated as externalities or
 *   non-issues. The constraint is claimed as tangled_rope because it carries
 *   a genuine coordination function (constituting a demos for collective
 *   self-determination and public goods) while simultaneously extracting from
 *   excluded migrants through active enforcement.
 *
 * KEY AGENTS:
 *   - citizen_populations (beneficiary, organized/constrained/national) â receive political membership, public goods, and labor-market protection from territorial closure.
 *   - state_apparatus (agenda_setter/beneficiary, institutional/constrained/national) â administers enforcement and derives authority/budget from the exclusion function.
 *   - excluded_migrants (payer, powerless/trapped/local) â bear the costs of denial: legal precarity, family separation, foregone opportunity, and violence exposure.
 *   - international_human_rights_institutions (observer, institutional/analytical/global) â critique exclusion against human rights standards but lack direct enforcement.
 *   - migrant_rights_advocates (excluded, moderate/mobile/national) â challenge the normative foundations of exclusion but are structurally absent from sovereign border decisions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Border Norm: State Authority to Exclude Non-Members").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'a747e079-6004-45b3-8ab9-1f10010d0117').
narrative_ontology:cs_kernel_codification('a747e079-6004-45b3-8ab9-1f10010d0117', fixed_text).
narrative_ontology:cs_authority_grounding('a747e079-6004-45b3-8ab9-1f10010d0117', lineage).
narrative_ontology:cs_interpretation_layer_present('a747e079-6004-45b3-8ab9-1f10010d0117').
narrative_ontology:cs_reading_relation('a747e079-6004-45b3-8ab9-1f10010d0117', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('a747e079-6004-45b3-8ab9-1f10010d0117', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('a747e079-6004-45b3-8ab9-1f10010d0117', foundational, collective_self_determination_requires_territorial_closure).
narrative_ontology:cs_axiom_status(collective_self_determination_requires_territorial_closure, holdable).
narrative_ontology:cs_axiom_grounding('a747e079-6004-45b3-8ab9-1f10010d0117', collective_self_determination_requires_territorial_closure, deontological).
narrative_ontology:cs_axiom('a747e079-6004-45b3-8ab9-1f10010d0117', foundational, state_exclusion_authority_prima_facie_legitimate).
narrative_ontology:cs_axiom_status(state_exclusion_authority_prima_facie_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('a747e079-6004-45b3-8ab9-1f10010d0117', state_exclusion_authority_prima_facie_legitimate, conventional).
narrative_ontology:cs_reference_frame('a747e079-6004-45b3-8ab9-1f10010d0117', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('a747e079-6004-45b3-8ab9-1f10010d0117', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a747e079-6004-45b3-8ab9-1f10010d0117', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_populations).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the benefits of territorial closure: access to public goods, political participation, labor-market protection, and collective self-determination bounded by the border. Their exit from this arrangement requires emigration and acceptance by another state.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_populations, beneficiary,
    organized, generational, constrained, national).

% Administers border enforcement, visa regimes, and deportation as a core function of statehood. Derives institutional authority, budget, and legitimacy from the claimed monopoly on territorial admission and exclusion.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, state_apparatus, beneficiary).

% Bear the costs of exclusion: denial of entry, family separation, exposure to violence, economic opportunity foregone, and legal precarity. Trapped by enforcement capacity and lack of standing in the admitting state's political process.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, immediate, trapped, local).

% Monitor and report on state border practices against human rights standards. Issue legal opinions and judgments critical of exclusion, but lack direct enforcement capacity to alter sovereign border decisions.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_institutions, observer,
    institutional, generational, analytical, global).

% Advance claims for liberalized migration and challenge the normative foundations of exclusion. Present in public discourse but structurally excluded from sovereign border-policy decisions.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, migrant_rights_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delimits a political community capable of collective self-determination, enabling democratic governance, public goods provision, and shared legal frameworks by bounding the demos.
% TRANSFER_FUNCTION: Transfers access to territory, legal status, and collective resources from non-members to members, enforced by state exclusion and border policing.
% ABSENT_VOICES: Excluded migrants are physically and legally absent from the sovereign decisions that determine admission; migrant_rights_advocates are present in discourse but excluded from the coercive decision-making apparatus. Their objections are externalized as foreign policy or humanitarian concerns rather than internal political claims.
% DISAPPEARANCE_RATIONALE: If the authority to exclude vanished overnight, bounded political communities would dissolve as organizing units, global migration patterns would radically restructure, labor markets and public goods regimes would face immediate reorganization, and the Westphalian distribution of authority would unravel.
% FOUNDING_PROBLEM: How to constitute a political community capable of collective self-determination and public goods provision in a world of competing claims to territory and membership.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars and constitutional historians attest the founding problem as the emergence of the Westphalian state system. Human rights jurists and migration scholars attest that the problem has evolved and the current arrangement produces new harms not addressed by the original framing. No single external corroborator is uncontested; corroboration is split across disciplinary seats outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint systematically denies life-chance access to a defined population. Suppression (0.72) is higher: the arrangement persists only through active enforcement â deportation, detention, visa regimes, and border militarization â not through voluntary coordination. Theater ratio (0.25) is moderate-low: while some border performance is symbolic sovereignty signaling, enforcement is materially consequential. Accessibility collapse (0.45) reflects that open-border alternatives exist intellectually and in limited policy experiments but are politically inaccessible to trapped migrants. Resistance (0.58) is substantial but asymmetric: migrants and advocates resist, but citizens and states broadly support the arrangement. The temporal series show extraction, theater, and suppression rising together over the interval as globalization increased migratory pressure and states responded with harder enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The citizen beneficiary seat and the excluded migrant payer seat diverge sharply. From the citizen perspective, the constraint is protective coordination that sustains democratic community and public goods; from the migrant perspective, it is active extraction enforced by violence and legal exclusion. The state apparatus sits as agenda-setter and secondary beneficiary, experiencing the arrangement as legitimate authority rather than coercion. The engine computes this divergence from structural data: beneficiaries with constrained exit receive low directionality, while trapped victims receive high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen_populations are declared beneficiaries (low d, subsidized by the constraint). State_apparatus is declared agenda_setter and beneficiary (low-to-moderate d, authority subsidizes the institution). Excluded_migrants are declared victims (high d, full target). International_human_rights_institutions and migrant_rights_advocates are observer and excluded respectively: they do not materially collect or pay in the constraint's transfer flow, and their directionality defaults to the power atom's canonical fallback (analytical for observers, moderate for excluded advocates). No overrides are needed because the structural derivation matches the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare depends on acknowledging the genuine coordination function: bounded political communities do enable collective self-determination and public-goods provision that unbounded alternatives would struggle to replicate. However, classifying it as rope would ignore the asymmetric victimization of excluded migrants. The tangled_rope label captures the hybrid structure â coordination for citizens riding on extraction from non-members â and prevents either pure-narrative capture. If the coordination function were entirely cover, it would be a snare; if extraction were absent, it would be rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_contest,
    'Is the state authority to exclude an inherent feature of political order, or a contingent normative reading that benefits citizens at migrants'' expense?',
    'Comparative historical analysis of pre-Westphalian political organization and examination of whether alternative political orders without territorial exclusion have sustained collective self-determination.',
    'If contingent, the constraint''s classification as tangled_rope strengthens; if inherent, it drifts toward mountain with FSM implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_reading_contest, conceptual, 'Whether territorial exclusion is natural law or constructed norm.').

omega_variable(
    extraction_coordination_boundary,
    'Does territorial closure extract from excluded migrants to subsidize citizen populations, or is closure a necessary cost of genuine collective self-determination?',
    'Counterfactual analysis of political communities with liberalized borders: assessing viability of public goods and democratic governance under alternative membership regimes.',
    'If closure is necessary for coordination, extraction is the price of the rope; if unnecessary, the coordination story is cover and the constraint is a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Tests whether the coordination function is separable from the extraction.').

omega_variable(
    citizen_complicity_internalization,
    'To what extent do citizen beneficiaries internalize the suppression of migrant alternatives as legitimate, rather than experiencing it as externally enforced?',
    'Survey and discursive analysis of citizen attitudes toward border enforcement across jurisdictions with varying enforcement intensity.',
    'High internalization would raise effective suppression and lower resistance; the constraint would appear more natural to beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_complicity_internalization, empirical, 'Degree to which beneficiaries internalize exclusionary norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__sovereignty_primary, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__sovereignty_primary, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__sovereignty_primary, theater_ratio, 30, 0.2).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__sovereignty_primary, theater_ratio, 40, 0.22).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__sovereignty_primary, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bord_be_t10, border_normative_status__sovereignty_primary, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(bord_be_t20, border_normative_status__sovereignty_primary, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(bord_be_t30, border_normative_status__sovereignty_primary, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(bord_be_t40, border_normative_status__sovereignty_primary, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(bord_be_t50, border_normative_status__sovereignty_primary, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bord_su_t10, border_normative_status__sovereignty_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bord_su_t20, border_normative_status__sovereignty_primary, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(bord_su_t30, border_normative_status__sovereignty_primary, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(bord_su_t40, border_normative_status__sovereignty_primary, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(bord_su_t50, border_normative_status__sovereignty_primary, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_normative_status kernel, which decomposes into three structurally distinct claims: sovereignty_primary (foundational exclusion authority), freedom_primary (movement as a fundamental right), and qualified_sovereignty (proportionate balance). Each reading emits a different constraint with different victim/beneficiary structures and epsilon values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
