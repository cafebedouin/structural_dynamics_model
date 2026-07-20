% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P): Sovereignty-Conditional Intervention Authority
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) doctrine reads the UN Charter's
 *   tension between Article 2(7) non-intervention and Chapter VII enforcement
 *   as a conditional sovereignty framework: states are sovereign only so long
 *   as they protect their populations, and systematic atrocity triggers an
 *   international responsibility to intervene. This reading is contested by a
 *   sovereignty-first sibling that treats non-intervention as foundational.
 *   From the R2P reading's perspective, the constraint coordinates global
 *   action against atrocity while asymmetrically extracting sovereign
 *   autonomy from targeted states and eroding the absolute sovereignty norm.
 *   The claim is tangled_rope: genuine coordination function (halting
 *   atrocities) fused with asymmetric extraction (geopolitical capture of
 *   intervention authority, selective application).
 *
 * KEY AGENTS:
 *   - persecuted_populations: Primary beneficiary (powerless/trapped) â receive protection if intervention is authorized
 *   - targeted_states: Primary target (institutional/constrained) â bear loss of sovereign immunity and military coercion
 *   - p5_security_council_members: Agenda-setter (institutional/arbitrage) â control activation via Chapter VII veto
 *   - intervening_states: Agenda-setter (institutional/mobile) â execute interventions and capture legitimizing authority
 *   - global_south_dissenting_states: Excluded voice (institutional/constrained) â oppose the norm as neo-imperial but lack veto power
 *   - sovereignty_norm: Non-agent observer â the Westphalian norm eroded by conditional sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.82).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P): Sovereignty-Conditional Intervention Authority").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '55bb42d2-d285-491c-9199-e32196a7f145').
narrative_ontology:cs_kernel_codification('55bb42d2-d285-491c-9199-e32196a7f145', formalized).
narrative_ontology:cs_authority_grounding('55bb42d2-d285-491c-9199-e32196a7f145', lineage).
narrative_ontology:cs_interpretation_layer_present('55bb42d2-d285-491c-9199-e32196a7f145').
narrative_ontology:cs_reading_relation('55bb42d2-d285-491c-9199-e32196a7f145', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('55bb42d2-d285-491c-9199-e32196a7f145', foundational, sovereignty_conditional_on_responsibility_to_protect).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('55bb42d2-d285-491c-9199-e32196a7f145', sovereignty_conditional_on_responsibility_to_protect, deontological).
narrative_ontology:cs_axiom('55bb42d2-d285-491c-9199-e32196a7f145', foundational, systematic_atrocity_activates_supranational_authority).
narrative_ontology:cs_axiom_status(systematic_atrocity_activates_supranational_authority, holdable).
narrative_ontology:cs_axiom_grounding('55bb42d2-d285-491c-9199-e32196a7f145', systematic_atrocity_activates_supranational_authority, conventional).
narrative_ontology:cs_reference_frame('55bb42d2-d285-491c-9199-e32196a7f145', conditional_sovereignty_framework).
narrative_ontology:cs_drift_state('55bb42d2-d285-491c-9199-e32196a7f145', post_2005_world_summit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55bb42d2-d285-491c-9199-e32196a7f145', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations facing systematic atrocities within a state; the constraint promises external protection but delivery depends on P5 politics and intervention logistics. They cannot exit the state easily and have no direct voice in the authorization decision.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, national).

% Member states of the UN whose internal conduct is subject to external scrutiny and potential coercive intervention; they bear the cost of lost sovereign autonomy and military targeting. Exit from the UN system is practically unavailable.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states, payer,
    institutional, generational, constrained, national).

% The norm of non-intervention and sovereign equality as traditionally understood under the Westphalian system; it is eroded by the R2P reading's conditional sovereignty frame.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm).

% Coalitions of states or regional organizations that invoke the R2P norm to authorize and execute coercive intervention; they set the enforcement agenda, define atrocity thresholds, and bear operational costs, while gaining legitimizing authority.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, agenda_setter,
    institutional, generational, mobile, global).

% Permanent members of the UN Security Council who control Chapter VII authorization; they can activate or block R2P-based intervention via veto, using the constraint as a gatekeeping tool for geopolitical interests.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, p5_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% A bloc of states, primarily from the Global South, that view R2P as a neo-imperialist instrument; they are structurally excluded from P5 agenda-setting and would object to the erosion of sovereign equality but lack institutional veto power.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, global_south_dissenting_states, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective international action to halt or prevent systematic atrocities (genocide, war crimes, ethnic cleansing, crimes against humanity) when a state manifestly fails to protect its population, solving the collective-action problem of who may act across sovereign borders.
% TRANSFER_FUNCTION: Transfers the authority to legitimize coercive cross-border force from the targeted state to the international community, channeled through UN Security Council Chapter VII decisions or ad hoc coalitions, conditional on atrocity thresholds.
% ABSENT_VOICES: Targeted states and the broader Global South dissenting bloc, who regard the norm as selectively applied and imperial; they are structurally absent from P5 veto decisions and marginalized in the interpretive community that defines manifest failure.
% DISAPPEARANCE_RATIONALE: If the R2P intervention authority vanished, targeted states would regain absolute sovereign impunity over internal atrocities; persecuted populations would lose the formal international recourse mechanism; and the global security architecture would revert to strict non-intervention, rearranging the balance between human protection and sovereignty.
% FOUNDING_PROBLEM: The international community's repeated paralysis in the face of systematic atrocities (Rwanda 1994, Srebrenica 1995, Kosovo 1999) because the UN Charter's sovereignty shield blocked timely collective action.
% FOUNDING_PROBLEM_CORROBORATION: Independent international commissions (ICISS 2001, UN Rwanda/Srebrenica inquiry reports) attest the founding problem from outside the benefiting parties; targeted states and Global South scholars contest that the arrangement solves it, arguing instead that it licenses geopolitical extraction.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint authorizes overriding the foundational norm of sovereign non-intervention, transferring authority to external actors. Suppression (0.82) is higher because the constraint's persistence requires active enforcement through Chapter VII authorization, sanctions, and military coercion, while suppressing alternative sovereignty claims. Theater ratio (0.45) reflects significant performative activity: never again rhetoric and institutional forums that mask selective application and veto paralysis. Accessibility collapse (0.60) captures the partial but significant closure of absolute-sovereignty alternatives once the atrocity threshold is declared. Resistance (0.75) is high due to sustained opposition from targeted states, Russia, China, and the Global South. The measurement series tracks the 2001â2021 lifecycle: emergence (0), institutionalization at the 2005 World Summit (5), peak enforcement during Libya 2011 (10), legitimacy crisis over Syria and backlash (15), and selective-normalization (20).
 *
 * PERSPECTIVAL GAP:
 *   The targeted_state seat experiences the constraint as violent extraction of sovereign autonomy and territorial control; the persecuted_populations seat experiences it as either protective coordination (when activated) or abandonment (when vetoed); the p5_security_council_members seat experiences it as a discretionary governance tool. The engine computes these divergent types from the same structural data: high power plus arbitrage exit yields low effective extraction for the P5, while trapped plus powerless yields high extraction for populations, and constrained plus institutional yields high extraction for targeted states.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to persecuted_populations, who structurally gain protection (low d, subsidy toward survival). Victim declarations map to targeted_states, who structurally lose sovereign control (high d, amplified extraction). The sovereignty_norm is declared as a non-agent victim to mark the erosion of the Westphalian frame without attributing agentic directionality to a doctrine. Intervening states and P5 members are not declared as beneficiaries because their gains are primarily authority and legitimacy rather than direct extraction from the constraint's operation; their directionality falls to the institutional canonical fallback, yielding moderate d.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint would likely be misread as a snare (if viewed only from targeted states' perspective) or a rope (if viewed only from humanitarian intent). The tangled_rope capture is essential: the coordination functionâhalting systematic atrocitiesâis genuine and not merely cover, but it is inextricably fused with asymmetric extraction. The same Chapter VII mechanism that protects populations in one case legitimizes regime change in another. Active enforcement is required to hold the hybrid together: without SC authorization and coalition military action, the constraint would not persist. The mandate has not fully atrophied into piton because the coordination function remains live in institutional memory and occasional practice, even as selective application corrodes it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r2p_imperialism_ambiguity,
    'Is R2P a genuine protection mechanism or a legitimizing frame for great-power regime change?',
    'Comparative case analysis controlling for geopolitical interest: if R2P is invoked consistently regardless of P5 interest (or blocked consistently when atrocities occur in P5-aligned states), the mechanism is structural; if invocation tracks great-power interest, the frame is cover.',
    'If the latter, the constraint reclassifies toward snare (pure extraction) because the coordination function is cover; if the former, it remains tangled_rope with genuine but imperfect coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_imperialism_ambiguity, empirical, 'Whether R2P''s protection function is separable from great-power extraction').

omega_variable(
    sovereignty_norm_victim_status,
    'Does treating the sovereignty norm itself as a victim of R2P overextend the victim category beyond agents, or does the norm''s erosion constitute real structural harm to non-target states?',
    'Observe whether non-target states alter behavior (decreased cooperation, accelerated alliance formation) in response to sovereignty norm erosion independent of direct targeting.',
    'If norm erosion produces measurable behavioral changes across the state system, the victim set is valid; if not, the extraction is better modeled as concentrated solely on targeted_states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_norm_victim_status, conceptual, 'Whether a norm can structurally occupy the victim seat').

omega_variable(
    selective_enforcement_undermines_legitimacy,
    'Does the veto-gated selective enforcement of R2P (intervention in Libya, paralysis in Syria) undermine its normative legitimacy to the point of functional collapse?',
    'Track state citation of R2P in General Assembly debates and domestic courts over time; a declining trajectory indicates legitimacy decay, while stable or rising citation indicates resilience.',
    'If legitimacy collapses, the constraint''s coordination function is hollowed out and it drifts toward piton or snare; if legitimacy persists, the extraction remains bounded by genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_undermines_legitimacy, empirical, 'Whether selective application hollows the norm''s coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(arti_tr_t5, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t5, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This constraint and sovereignty_first_reading are dual readings of the Article 2(7)/Chapter VII tension kernel. They share the UN Charter text but instantiate mutually contestable normative constraints with opposed epsilon profiles and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
