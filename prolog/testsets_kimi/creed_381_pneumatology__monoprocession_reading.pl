% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: 381 Constantinopolitan Creed â Monoprocession Reading
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This constraint instantiates the monoprocession reading of the
 *   creed_381_pneumatology kernel. The kernel is the pneumatology clause of
 *   the 381 Constantinopolitan Creed. This reading holds that the Holy Spirit
 *   proceeds from the Father alone, that the 381 creed is inviolable without
 *   ecumenical consent, and that the Latin Filioque constitutes a unilateral
 *   breach. Sibling readings include the filioque_reading (Spirit proceeds
 *   from Father and Son, magisterial authority to clarify implicit doctrine)
 *   and the ecumenical_reunion_reading (both mono-procession and Filioque
 *   acceptable as regional theological expressions within a single
 *   communion). The reading functions as a wall-type commitment system: it
 *   blocks single-see magisterial supremacy and preserves Eastern
 *   autocephalous polity, but extracts compliance from Western unilateral
 *   innovators by classifying their doctrinal development as breach.
 *
 * KEY AGENTS:
 *   - Eastern patriarchates (beneficiary/agenda-setter) â administer the creedal boundary and benefit from the wall against Roman unilateralism
 *   - Roman See (payer) â bears the cost of blocked magisterial authority and identity-locked Filioque tradition
 *   - Western unilateral innovators (payer) â Latin theologians whose doctrinal contributions are suppressed as breach
 *   - Ecumenical reunion advocates (excluded) â propose bilateral recognition and are structurally excluded by the breach definition
 *   - Historical theologians (observer) â analyze the asymmetry between coordination function and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.79).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.76).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "381 Constantinopolitan Creed â Monoprocession Reading").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, 'b537d122-6e6f-4ce7-a09d-c93fde3ac668').
narrative_ontology:cs_kernel_codification('b537d122-6e6f-4ce7-a09d-c93fde3ac668', fixed_text).
narrative_ontology:cs_authority_grounding('b537d122-6e6f-4ce7-a09d-c93fde3ac668', lineage).
narrative_ontology:cs_interpretation_layer_present('b537d122-6e6f-4ce7-a09d-c93fde3ac668').
narrative_ontology:cs_reading_relation('b537d122-6e6f-4ce7-a09d-c93fde3ac668', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('b537d122-6e6f-4ce7-a09d-c93fde3ac668', creed_381_pneumatology__ecumenical_reunion_reading, forecloses).
narrative_ontology:cs_axiom('b537d122-6e6f-4ce7-a09d-c93fde3ac668', foundational, spirit_proceeds_from_father_alone).
narrative_ontology:cs_axiom_status(spirit_proceeds_from_father_alone, holdable).
narrative_ontology:cs_axiom_grounding('b537d122-6e6f-4ce7-a09d-c93fde3ac668', spirit_proceeds_from_father_alone, theological).
narrative_ontology:cs_axiom('b537d122-6e6f-4ce7-a09d-c93fde3ac668', foundational, ecumenical_consent_required_for_creedal_amendment).
narrative_ontology:cs_axiom_status(ecumenical_consent_required_for_creedal_amendment, holdable).
narrative_ontology:cs_axiom_grounding('b537d122-6e6f-4ce7-a09d-c93fde3ac668', ecumenical_consent_required_for_creedal_amendment, conventional).
narrative_ontology:cs_reference_frame('b537d122-6e6f-4ce7-a09d-c93fde3ac668', conciliar_ecumenical_consensus).
narrative_ontology:cs_drift_state('b537d122-6e6f-4ce7-a09d-c93fde3ac668', post_great_schism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b537d122-6e6f-4ce7-a09d-c93fde3ac668', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, roman_see).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, conciliar_supremacy_doctrine).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, nicaea_constantinople_creed_inviolability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uphold the 381 Constantinopolitan Creed as the fixed boundary of Trinitarian doctrine. They teach that the Holy Spirit proceeds from the Father alone and that any amendment, such as the Latin Filioque, requires the consent of an ecumenical council. They benefit from a structural wall that prevents the Roman See from unilaterally imposing doctrinal developments on the whole Church, thereby preserving autocephalous governance and conciliar supremacy. They maintain communion boundaries accordingly.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_patriarchates, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, eastern_patriarchates, agenda_setter).

% Holds that the papal and general magisterium possess the authority to clarify and develop doctrine, including the Filioque as an articulation of the Spirit's eternal relation to Son and Father. The monoprocession reading blocks this authority by declaring the 381 creed inviolable without ecumenical consent, classifying the Latin tradition as unilateral breach. To accept the monoprocession reading would require renouncing a millennium of Latin theological identity and subordinating papal teaching authority to a conciliar veto that Rome does not recognize as currently operative.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, roman_see, payer,
    institutional, civilizational, identity_locked, global).

% Latin theologians and hierarchs who have developed, taught, or defended the Filioque as integral to Trinitarian theology. Their work is classified as illegitimate innovation under the monoprocession reading, which denies their contributions standing in the universal Church unless ratified by ecumenical consent they do not control.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators, payer,
    moderate, generational, constrained, global).

% Theologians and church leaders who propose that the Filioque and monoprocession can be recognized as compatible regional theological expressions within a single communion, obviating the need for either side to renounce its tradition. The monoprocession reading structurally excludes this possibility by defining Filioque as breach rather than difference, making bilateral recognition impossible without abandoning the creedal wall.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, ecumenical_reunion_advocates, excluded,
    moderate, generational, constrained, global).

% Scholars who analyze the 381 creed as a historical document and boundary mechanism. They observe how the monoprocession reading coordinates Eastern ecclesial identity while generating asymmetric costs for Latin magisterial claims, and they track the divergence between the creed's original function and its later role as a communion barrier.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__monoprocession_reading, eastern_patriarchates).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__monoprocession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single episcopal see from unilaterally legislating Trinitarian doctrine for the entire Church, preserving a decentralized conciliar polity in which binding doctrinal change requires universal ecumenical consensus.
% TRANSFER_FUNCTION: Moves authority over Trinitarian clarification from the Roman See and Latin theological innovators to the collective of autocephalous Eastern churches and the ecumenical council framework; transfers legitimacy from unilateral magisterial acts to multilateral conciliar consent.
% ABSENT_VOICES: Western theologians who read the Filioque as organic development of implicit apostolic doctrine, and ecumenical reunion advocates who propose bilateral recognition of divergent pneumatologies as compatible regional expressions, are structurally excluded from this framework's negotiation space.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the Roman See could unilaterally amend or clarify the creed for its communion, the Eastern autocephalous wall against Latin magisterial supremacy would collapse, and the conciliar-ecumenical bottleneck for doctrinal change would be bypassed â global ecclesiastical power would recentralize and the communion boundary would reconfigure around magisterial rather than conciliar authority.
% FOUNDING_PROBLEM: The centralized doctrinal authority of the Roman See and the risk that unilateral theological innovation would destabilize the communion of autocephalous churches; the need for a stable, shared Trinitarian formula resistant to single-actor revision.
% FOUNDING_PROBLEM_CORROBORATION: Eastern patriarchates attest the problem remains live, citing papal supremacy and unilateral dogmatic definition as ongoing threats. Western historians and ecumenical scholars attest the problem has mutated: the Latin Church reads its amendment as organic development rather than destabilizing innovation, suggesting the arrangement now persists as structural rivalry more than protective coordination. Corroboration from outside the beneficiary set is therefore mixed and contested.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__monoprocession_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__monoprocession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__monoprocession_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__monoprocession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__monoprocession_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because the constraint blocks an entire communion's doctrinal development on pneumatology and centralizes legitimacy in Eastern conciliar structures; suppression is comparably high (0.76) because the enforcement mechanism is schism and anathema, which are severe ecclesial penalties. Theater_ratio is moderate-high (0.52): the theological commitment to the 381 creed is genuine, but a substantial share of enforcement activity has become performative boundary maintenance of Eastern identity against Latin encroachment rather than active theological negotiation. Accessibility_collapse is high (0.80) because, once inside this commitment framework, the Filioque is not experienced as a live theological option but as a breach condition. Resistance is substantial (0.72) because the Latin side has maintained a competing magisterial and theological framework for a millennium. The measurement series show extraction and suppression rising sharply after the 11th-century breach and plateauing in the modern era, while theater_ratio peaked during the early modern entrenchment period and remains elevated.
 *
 * PERSPECTIVAL GAP:
 *   The Eastern patriarchate seat experiences this constraint as a protective rope or mountain-like bulwark preserving the faith against centralized innovation. The Roman See experiences it as an extractive snare blocking legitimate magisterial function. The engine computes this divergence from the same structural data: identical scope and power atoms yield opposite computed types when beneficiary versus victim declarations are swapped. The perspectival gap is maximal because the constraint governs the same theological proposition from two irreconcilable authority frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern patriarchates sit near the beneficiary end: they structurally collect preserved autocephaly and a veto over Roman claims (low d). Their identity_locked exit actually reinforces their beneficiary position because the creed is their legitimacy anchor. The Roman See sits near the full-target end: the constraint extracts from its magisterial claims and forces a choice between identity and communion (high d). Western unilateral innovators are also high-d targets, though with lower power and more constrained exit. Ecumenical reunion advocates are excluded from the directionality calculation because they are not in the conversation, but their exclusion is the mechanism that keeps suppression high. Historical theologians occupy the analytical seat (analytical exit, analytical scope).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing Roman unilateralism from destabilizing the communion â is contested. If the problem is dead because modern ecumenism offers bilateral dialogue rather than unilateral imposition, the constraint risks mandatrophy. However, the Eastern reading holds the problem is live because papal supremacy remains structurally asserted (e.g., unilateral dogmatic definitions at Vatican I). The T17 abductive trigger would fire if measurements showed theater_ratio rising while base_extractiveness plateaued, indicating the wall persists by inertia. Currently, the metrics show both extraction and theater as stable or slightly elevated, suggesting the constraint is still actively defended rather than purely inertial, but the R5 genealogy (founding_problem_status contested) flags it as a candidate for future piton drift if ecumenical relations normalize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_ambiguity,
    'Does the 381 creed''s pneumatology clause inherently encode monoprocession, or is monoprocession one historically dominant reading among several defensible readings of the same kernel?',
    'Historical-critical and philological analysis of pre-381 pneumatic theology, plus manuscript study of the creed''s original intent and early reception.',
    'If monoprocession is a reading rather than the kernel''s inherent meaning, the constraint''s authority grounding shifts from textual fixity to interpretive tradition, altering the commitment-system classification toward practice-based authority and potentially lowering accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_ambiguity, conceptual, 'Whether monoprocession is inherent to the kernel or one reading among many.').

omega_variable(
    filioque_as_development_or_breach,
    'Is the Filioque a legitimate organic development of implicit Trinitarian doctrine, or a unilateral breach of conciliar creed?',
    'Patristic corpus analysis and causal tracing of the Filioque''s reception in the Latin West versus its rejection in the Greek East; examination of whether the development was procedurally unilateral.',
    'If the Filioque is legitimate development, the monoprocession reading''s victim structure is undermined and its extraction appears one-sided; if it is breach, the wall-function is vindicated and the victim classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(filioque_as_development_or_breach, empirical, 'Historical-theological resolution of Filioque legitimacy.').

omega_variable(
    ecumenical_consent_trigger,
    'What institutional mechanism counts as ecumenical consent sufficient to amend the 381 creed, and is that mechanism triggerable in the current ecclesial landscape?',
    'Analysis of conciliar convocation procedures, autocephalous recognition requirements, and communion-wide reception practices across Orthodox, Catholic, and ecumenical bodies.',
    'If ecumenical consent is practically impossible to convene, the without-ecumenical-consent clause functions as a de facto absolute prohibition, raising effective extraction by rendering the constraint non-revisable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecumenical_consent_trigger, conceptual, 'Whether the amendment procedure is a live mechanism or a dead letter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creed_381_mono_tr_t0, creed_381_pneumatology__monoprocession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(creed_381_mono_tr_t20, creed_381_pneumatology__monoprocession_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(creed_381_mono_tr_t40, creed_381_pneumatology__monoprocession_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(creed_381_mono_tr_t60, creed_381_pneumatology__monoprocession_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(creed_381_mono_tr_t80, creed_381_pneumatology__monoprocession_reading, theater_ratio, 80, 0.53).
narrative_ontology:measurement(creed_381_mono_tr_t100, creed_381_pneumatology__monoprocession_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(creed_381_mono_be_t0, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(creed_381_mono_be_t20, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(creed_381_mono_be_t40, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(creed_381_mono_be_t60, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(creed_381_mono_be_t80, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(creed_381_mono_be_t100, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 100, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(creed_381_mono_su_t0, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(creed_381_mono_su_t20, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(creed_381_mono_su_t40, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(creed_381_mono_su_t60, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(creed_381_mono_su_t80, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 80, 0.77).
narrative_ontology:measurement(creed_381_mono_su_t100, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 100, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the creed_381_pneumatology kernel. The colloquial label '381 pneumatology' conflates three structurally distinct claims: monoprocession (this file, wall-type CS preserving Eastern polity), filioque (hierarchy-type CS with papal magisterial authority), and ecumenical reunion (coordination-type CS with bilateral recognition). Each has different epsilon values, different beneficiary structures, different authority groundings, and different failure modes. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
