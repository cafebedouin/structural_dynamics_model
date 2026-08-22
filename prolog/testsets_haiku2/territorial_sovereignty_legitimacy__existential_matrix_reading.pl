% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Territorial Sovereignty via Existential Necessity (Ethnic/National Survival Frame)
 *   domain: political_theory/international_relations
 *
 * SUMMARY:
 *   The existential-matrix reading frames territorial sovereignty legitimacy
 *   as non-negotiable existential necessity for ethnic/national groups, not
 *   as juridical arrangement. Under this reading, each group perceives
 *   control of bounded territory as the only security against collective
 *   extinction. Compromise frameworks (two-state solutions, shared
 *   sovereignty, autonomy arrangements) are structurally unstable because
 *   they require one or both sides to accept ongoing vulnerability—an
 *   existentially unacceptable position. The reading predicts that conflict
 *   persists regardless of legal settlements, international recognition, or
 *   historical argument because the underlying existential fear and
 *   demographic competition remain. Beneficiary is whichever group achieves
 *   or maintains dominance; losers are displaced, minoritized, or stateless
 *   populations. This reading coexists with the covenant-continuity reading
 *   (grounded in divine promise and historical presence) and the
 *   self-determination reading (grounded in modern democratic principle
 *   applied to demographic majorities). The three readings share the same
 *   kernel (territorial sovereignty legitimacy) but instantiate different
 *   constraint structures with different victim sets, different framings of
 *   the problem, and different stability predictions.
 *
 * KEY AGENTS:
 *   - dominant_ethnic_majority: agenda-setter, territory-defending group (powerful/organized, identity-locked exit)
 *   - competing_ethnic_majority: reciprocal agenda-setter, territory-claiming group (powerful/organized, identity-locked exit)
 *   - displaced_population: victim, removed from territory, stateless (powerless, trapped)
 *   - minority_ethnic_groups: victim, subordinated within territory (powerless, constrained exit)
 *   - international_legal_order: observer, produces treaties and recognition (institutional, analytical exit)
 *   - third_party_states: observer, intervenes militarily or diplomatically (institutional, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.89).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.91).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Territorial Sovereignty via Existential Necessity (Ethnic/National Survival Frame)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '82d5128a-8045-40fb-923e-98dd3af9e3f6').
narrative_ontology:cs_kernel_codification('82d5128a-8045-40fb-923e-98dd3af9e3f6', distributed).
narrative_ontology:cs_authority_grounding('82d5128a-8045-40fb-923e-98dd3af9e3f6', distributed).
narrative_ontology:cs_reading_relation('82d5128a-8045-40fb-923e-98dd3af9e3f6', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('82d5128a-8045-40fb-923e-98dd3af9e3f6', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('82d5128a-8045-40fb-923e-98dd3af9e3f6', foundational, existential_security_prerequisite_to_legitimacy).
narrative_ontology:cs_axiom_status(existential_security_prerequisite_to_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('82d5128a-8045-40fb-923e-98dd3af9e3f6', existential_security_prerequisite_to_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('82d5128a-8045-40fb-923e-98dd3af9e3f6', foundational, territorial_control_only_viable_security_model).
narrative_ontology:cs_axiom_status(territorial_control_only_viable_security_model, holdable).
narrative_ontology:cs_axiom_grounding('82d5128a-8045-40fb-923e-98dd3af9e3f6', territorial_control_only_viable_security_model, empirically_contingent).
narrative_ontology:cs_reference_frame('82d5128a-8045-40fb-923e-98dd3af9e3f6', existential_ethnic_security_framework).
narrative_ontology:cs_drift_state('82d5128a-8045-40fb-923e-98dd3af9e3f6', contemporary_international_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('82d5128a-8045-40fb-923e-98dd3af9e3f6', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, minority_ethnic_groups).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, stateless_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, competing_ethnic_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frames territorial control as existential precondition for group survival and cultural continuity. Sets the terms of conflict by invoking demographic and military dominance as proof of legitimacy and survival necessity. Cannot accept territorial compromise without experiencing it as collective annihilation risk.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethnic_majority, agenda_setter,
    organized, civilizational, identity_locked, national).

% Reciprocally frames territorial control as existential precondition for its own group survival. Cannot accept subordination or minority status in shared territory without experiencing it as annihilation risk. Each side's security is the other side's threat.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, competing_ethnic_majority, agenda_setter,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, competing_ethnic_majority, payer).

% Removed from territory during conflict or in its aftermath. Cannot return because territorial control by the dominant group is treated as existentially non-negotiable. Remain stateless, in camps, or in diaspora indefinitely. Their dispossession is framed as necessity by the occupying group, not as an injustice to be remedied.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_population, payer,
    powerless, biographical, trapped, global).

% Inhabit territory claimed as existentially necessary by a dominant ethnic group. Their presence is treated as a demographic or security threat. Face expulsion, assimilation pressure, or second-class citizenship. Cannot organize politically without being framed as existential threat to the majority.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, minority_ethnic_groups, payer,
    powerless, biographical, constrained, national).

% Produces treaties, recognition frameworks, partition plans, and self-determination doctrines. These are rendered epiphenomenal by the existential-matrix reading: law is invoked instrumentally by whichever side is winning militarily, and abandoned when inconvenient. The legal order cannot enforce settlements because the underlying existential fear persists regardless of legal status.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_legal_order, observer,
    institutional, generational, analytical, universal).

% Intervene as guarantors, mediators, or military backers. Their intervention is read through the existential frame: each side interprets third-party actions as evidence of existential threat or existential protection. Legal guarantees (international treaties, peacekeeping mandates) are treated as fragile and unstable because they depend on third parties maintaining commitment in perpetuity.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, third_party_states, observer,
    institutional, generational, analytical, global).

% Exist outside the territorial system entirely. They have no claim because the existential-sovereignty frame grants legitimacy only to groups with demonstrated territorial control or the capacity to seize it. Their exclusion from the framework is structural, not accidental.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, stateless_populations, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethnic_majority).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None acknowledged by this reading. The existential-matrix frame treats territorial control as non-coordinated survival competition, not as a shared problem requiring mutual solution. Any coordination function (water access, infrastructure sharing, cross-border commerce) is subordinated to existential threat perception and rendered non-viable by it.
% TRANSFER_FUNCTION: Moves territorial control, demographic dominance, military capability, and cultural authority from one ethnic group to another. The transfer is described as inevitable consequence of existential competition, not as extraction—the victorious group experiences it as survival, the defeated group as annihilation.
% ABSENT_VOICES: Stateless and displaced populations are systematically excluded because the existential frame recognizes only groups with territorial capacity or historical claim. Internal minorities who might argue for shared sovereignty or pluralism are heard only as security threats. International legal voices arguing for compromise frameworks are overheard but treated as naive to the reality of existential fear.
% DISAPPEARANCE_RATIONALE: One reading: if the existential-sovereignty frame disappeared and were replaced by a post-national security model (interdependence rather than territorial isolation), conflict would restructure entirely—the constraint is the frame, and frame-loss is world-rearrangement. Counter-reading: the existential fear is primal and the frame merely articulates what would persist anyway—underlying material scarcity, demographic pressure, and historical trauma would regenerate the constraint independently of the current framing.
% FOUNDING_PROBLEM: Each group perceives territorial control as the only buffer against collective extinction. Historical persecution, waves of expulsion, demographic competition over bounded land, and memory of violence make shared territory appear structurally unstable. Existential sovereignty is framed as the solution to that fear.
% FOUNDING_PROBLEM_CORROBORATION: Historians and security analysts attesting to historical persecution cycles and demographic anxiety confirm the fear's historical roots. However, scholars of conflict resolution and post-conflict reconciliation, speaking from outside the ethnic groups in conflict, argue that existential sovereignty is a frame that GENERATES existential fear through securitization, rather than responding to it—they corroborate the anxiety but question whether it requires the territorial-sovereignty solution or whether it could be addressed through other institutional designs (federal arrangements, minority rights protections, economic interdependence, truth commissions). The existential-sovereignty reading takes the fear as given; external observers dispute whether the frame is necessary to that fear or amplifies it.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89 at interval end) and rising because the existential-sovereignty frame justifies unlimited assertion of control: the dominant group experiences territorial expansion, settlement, resource monopoly, and security fortification as survival necessities, not as extraction—but from the standpoint of displaced and minoritized populations, these are pure taking with no reciprocal benefit or exit. Suppression is equally high (0.91) because the constraint requires active military, demographic, and legal machinery to sustain: borders enforced, minorities managed, displaced populations kept out, competing territorial claims delegitimized. Theater is moderate (0.62 and rising) because the existential-sovereignty frame is partially performative—international law is invoked when convenient and discarded when inconvenient; legal arguments and historical claims are weaponized rather than genuinely deliberated. The rising theater ratio indicates that as the constraint consolidates, more enforcement energy goes into maintaining the narrative justification (international diplomacy, historical argumentation, legal positioning) than into the actual survival function—a sign of piton-drift (function atrophied, theatrical maintenance remains). However, the existential fear itself is real enough that pure performance cannot sustain the constraint alone; the underlying security dynamic provides material force. The measurement series shows monotonic intensification: extraction and suppression requirements grow together as the demographic/military balance stabilizes in favor of the dominant group, but theater grows faster, indicating narrative justification outpacing functional necessity. This is consistent with a snare-to-piton trajectory: pure extraction regime being dressed in survival rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The existential-sovereignty reading forecasts deep divergence between beneficiary and payer seats. The dominant-group seats will perceive the constraint as genuine rope (intra-group coordination for survival). The payer seats will perceive it as snare (pure extraction with no reciprocal benefit). International legal observers will perceive it as tangled rope (real coordination function—providing framework for state-to-state relations—plus extraction via selective enforcement and historical argument weaponization). The engine's per-seat computation should show this divergence; it is not a defect in the story but a measurement of how the existential frame produces asymmetric perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Identity-locking is the key mechanism here. Dominant ethnic majorities are locked into the territorial identity by civilizational timescale and survival stakes: they cannot imagine an alternative arrangement that does not threaten group extinction. This makes their exit analytically 'identity_locked' even though they are powerful—power does not override identity lock in the existential frame. Displaced populations are identity-locked in a different way: they are locked out of the territory by ethnic identity and cannot gain access to the dominant group's existential-security umbrella. This produces the asymmetry: both sides are identity-locked, but in opposite directions (insider/outsider), and that asymmetry is what makes the constraint a snare rather than a mutual rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The existential-sovereignty constraint shows early signs of mandatrophy: the founding problem (group survival in a competitive territorial environment) is simultaneously live (survival anxiety persists, demographic competition continues) and contested (scholars argue that existential sovereignty GENERATES rather than responds to survival anxiety, and that the frame is unnecessarily zero-sum). The theater-ratio rise (0.35 → 0.62) indicates increasing performative maintenance: international diplomacy, legal argument, historical narrative production consume more enforcement energy even as the underlying security dynamic remains tense. A true piton would show theater approaching 1.0 with functional extraction declining; here extraction is rising alongside theater, indicating a constraint that is still producing material domination but increasingly requires narrative justification. The classification remains snare (pure extraction from payer seats) rather than piton (atrophied function, theatrical persistence) as long as beneficiary seats experience genuine collective security benefit. If that perception shifts—if dominant groups begin to recognize that existential-sovereignty maintenance itself becomes the costlier path to survival than post-territorial alternatives—mandatrophy could accelerate. The omega variables document the irreducible uncertainty: whether the existential fear is foundational (existential sovereignty required) or constructed (existential sovereignty produces the fear it claims to respond to).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_fear_foundation,
    'Is the existential fear for group survival a foundational and inescapable feature of the territorial conflict, or is it constructed and amplified by the existential-sovereignty frame itself?',
    'Historical analysis of threat narratives across time; comparison with conflicts that resolved or de-escalated despite similar material vulnerabilities; psychological/sociological study of identity-threat perception in post-conflict societies; natural experiments from institutional arrangements that have successfully decoupled territorial identity from existential security (federal systems, multi-ethnic states with constitutional protections for minorities).',
    'If existential fear is foundational, the existential-sovereignty reading is necessary and the constraint cannot be classified as a false-summit piton. If constructed, the reading is a defensive ideological frame and the constraint becomes a candidate for mandatrophy—the underlying problem (shared territory, demographic competition, historical trauma) could be addressed through alternative institutional designs that do not require existential sovereignty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_fear_foundation, conceptual, 'Whether existential fear drives the constraint or the constraint generates existential fear through securitization.').

omega_variable(
    compromise_framework_stability,
    'Are territorial compromise frameworks (two-state solutions, federal arrangements, autonomy agreements) structurally unstable because both sides must accept unacceptable existential vulnerability, or are they unstable primarily because enforcement mechanisms are weak and third-party commitment wanes over time?',
    'Case analysis of failed and sustained compromise frameworks (Cyprus, Israel-Palestine, Kashmir, Northern Ireland, Spain-Catalonia); comparison of enforcement costs and breakdown triggers; examination of whether framework failure is driven by renewed existential fear or by institutional capacity degradation and third-party withdrawal.',
    'If structural incompatibility of existential-security requirements drives failure, compromise is theoretically impossible and territorial dominance is the only stable outcome. If institutional weakness drives failure, redesigned enforcement and third-party commitment could stabilize compromise frameworks, making the constraint a snare that could be transformed into rope through institutional engineering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_framework_stability, empirical, 'Whether compromise-framework failure is structural or institutional.').

omega_variable(
    alternative_security_models,
    'Could post-territorial security models (economic interdependence, supranational federation, collective security institutions, non-territorial cultural autonomy) provide equivalent security benefits to existential-sovereignty arrangements, at lower cost?',
    'Historical analysis of security transitions from territorial to post-territorial models (European integration, ASEAN, NATO); study of non-territorial identity preservation (diaspora communities, transnational professional networks, digital communities); economic modeling of interdependence-based security versus sovereignty-based security.',
    'If post-territorial models can provide equivalent security at lower cost, the existential-sovereignty frame is optional rather than necessary, and the constraint becomes candidate for frame-replacement. The existential fear might persist but could be addressed through alternative institutional designs. If post-territorial models fail to provide equivalent security (demonstrated through failure cases), existential sovereignty remains the best available security model despite its high extraction cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_security_models, empirical, 'Whether existential security can be decoupled from territorial control.').

omega_variable(
    reading_foreclosure_risk,
    'Does the existential-matrix reading foreclose the self-determination and covenant-continuity readings, or do all three remain live positions that coexist across different parties?',
    'Logical analysis of whether acceptance of existential-sovereignty necessity entails rejection of self-determination or covenant-continuity as grounds for legitimacy; examination of whether parties to the conflict actually hold multiple readings simultaneously (e.g., invoking existential security while also claiming historical right or democratic self-determination); exploration of whether the readings can be integrated into a single framework or whether they are genuinely incommensurable.',
    'If foreclosure occurs, the existential-matrix reading dominates and the conflict is fundamentally zero-sum by logical necessity, not just empirically. If coexistence is maintained, the three readings function as competing narratives that all remain available for strategic deployment, and the contest is over which frame will prevail, not which is logically true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether the existential-matrix reading logically entails rejection of competing readings or allows coexistence.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the high suppression (0.91) maintained primarily through external structural barriers (military occupation, legal restrictions, demographic engineering) or through internalized identity-threat narratives that persist even when external enforcement relaxes?',
    'Post-exit or post-agreement observation: if suppression drops sharply when external enforcement mechanisms are removed (military withdrawal, legal rights restoration), suppression is primarily structural. If suppression persists after external enforcement relaxes (communities continue to resist engagement, demographic separation persists, inter-group violence cycles despite legal peace agreements), suppression is internalized.',
    'If structural, institutional redesign and third-party enforcement could reduce suppression and enable compromise. If internalized, even agreement and enforcement cannot eliminate the underlying identity-threat perception, and conflict will recur as soon as institutional enforcement lapses—supporting the existential-sovereignty reading''s pessimism about compromise frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is structural/external or internalized/cognitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement(terr_tr_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 55, 0.6).
narrative_ontology:measurement(terr_tr_t75, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 75, 0.62).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(terr_be_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 55, 0.88).
narrative_ontology:measurement(terr_be_t75, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 75, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(terr_su_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 55, 0.9).
narrative_ontology:measurement(terr_su_t75, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 75, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.18).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% The territorial_sovereignty_legitimacy kernel decomposes into three structurally distinct readings with different epsilon values and victim sets. The existential_matrix_reading (this story) treats sovereignty as existential necessity and predicts zero-sum conflict with permanent displacement of losers (high epsilon, snare classification). The covenant_continuity_reading grounds legitimacy in divine promise and historical presence, making historical argument the conflict mechanism (moderate epsilon). The self_determination_reading grounds legitimacy in democratic principle applied to demographic majorities (moderate epsilon). All three readings share the kernel and affect each other; each should be authored as a separate constraint story with its own epsilon and stakeholder structure, linked through this network field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
