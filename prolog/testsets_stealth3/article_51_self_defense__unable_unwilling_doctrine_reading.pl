% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Unable-or-Unwilling Doctrine Reading of Article 51 Self-Defense
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the Article 51 self-defense
 *   kernel: the unable-or-unwilling doctrine, under which an actual armed
 *   attack by a non-state actor, launched from the territory of a state that
 *   is unwilling or unable to suppress the attacking group, triggers a right
 *   of unilateral cross-border self-defense. The doctrine answers a real
 *   structural gap — the Charter's state-attribution framework predates
 *   transnational armed groups, and the Security Council is frequently
 *   veto-blocked — while extracting from host states whose territorial
 *   sovereignty is bypassed without consent and from civilian populations in
 *   strike zones. The referent of epsilon is the standing arrangement under
 *   contest: the accumulated practice of unilateral force justified by
 *   unwilling-or-unable determinations, assessed by this reading's own
 *   lights. The claimed type (tangled_rope) and the metrics below are
 *   authored independently: the claim records the hybrid structure the source
 *   material asserts; the metrics record the operation as observed. Sibling
 *   readings (narrow attribution; expansive preventive) are separate
 *   constraints, linked through the network section, not averaged into this
 *   file. KEY AGENTS (by structural relationship): -
 *   intervening_counterterrorism_states: agenda-setting beneficiary
 *   (institutional/arbitrage) — asserts the doctrine, determines
 *   unwillingness or inability, conducts operations -
 *   sovereignty_bypassed_host_states: primary target (moderate/trapped) —
 *   territorial sovereignty bypassed without consent -
 *   civilian_populations_in_strike_zones: kinetic-cost bearer
 *   (powerless/trapped) - un_security_council_p5: institutional beneficiary
 *   with agenda-setting veto — blocks correction, reserves reciprocal option
 *   - coalition_partner_states: secondary beneficiaries (powerful/mobile) —
 *   basing, intelligence, diluted exposure - non_state_armed_groups:
 *   organized target of the force whose attacks acquire interstate standing -
 *   general_assembly_majority_states: excluded objectors — numbers without
 *   enforcement - international_law_community: analytical observer —
 *   documents, drafts principle sets, contests
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.64).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Unable-or-Unwilling Doctrine Reading of Article 51 Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '3fdd1332-64da-49c6-9371-06f9ab7f94a7').
narrative_ontology:cs_kernel_codification('3fdd1332-64da-49c6-9371-06f9ab7f94a7', fixed_text).
narrative_ontology:cs_authority_grounding('3fdd1332-64da-49c6-9371-06f9ab7f94a7', practice).
narrative_ontology:cs_interpretation_layer_present('3fdd1332-64da-49c6-9371-06f9ab7f94a7').
narrative_ontology:cs_reading_relation('3fdd1332-64da-49c6-9371-06f9ab7f94a7', article_51_self_defense__article_51_narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('3fdd1332-64da-49c6-9371-06f9ab7f94a7', article_51_self_defense__article_51_expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('3fdd1332-64da-49c6-9371-06f9ab7f94a7', foundational, nonstate_attack_triggers_self_defense).
narrative_ontology:cs_axiom_status(nonstate_attack_triggers_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('3fdd1332-64da-49c6-9371-06f9ab7f94a7', nonstate_attack_triggers_self_defense, conventional).
narrative_ontology:cs_axiom('3fdd1332-64da-49c6-9371-06f9ab7f94a7', foundational, host_failure_substitutes_for_consent_or_attribution).
narrative_ontology:cs_axiom_status(host_failure_substitutes_for_consent_or_attribution, holdable).
narrative_ontology:cs_axiom_grounding('3fdd1332-64da-49c6-9371-06f9ab7f94a7', host_failure_substitutes_for_consent_or_attribution, instrumental).
narrative_ontology:cs_reference_frame('3fdd1332-64da-49c6-9371-06f9ab7f94a7', inherent_right_with_nonstate_host_failure_trigger).
narrative_ontology:cs_drift_state('3fdd1332-64da-49c6-9371-06f9ab7f94a7', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3fdd1332-64da-49c6-9371-06f9ab7f94a7', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council_p5).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, coalition_partner_states).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, sovereignty_bypassed_host_states).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_strike_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the military and intelligence capacity to project force across borders. When an armed attack by a group based in another state occurs, their executive and legal-adviser apparatus determines whether the territorial state is controlling the group, drafts the justification communicated to the Security Council, and conducts the strikes. What flows to them is operational latitude: the ability to act without waiting for collective authorization. Exiting the practice would mean forgoing unilateral response options and depending on Council action or host-state cooperation.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Territorial states from whose territory attacks are launched or armed groups operate. Some formally protest incursions, some quietly cooperate, some request assistance they cannot deliver themselves. What flows from them is control over parts of their territory during operations they did not authorize; what flows to them is diplomatic friction, domestic backlash, and occasional partnership or compensation programs. Leaving the arrangement is unavailable: the norm being eroded is the principal protection they hold, and territory cannot be relocated.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, sovereignty_bypassed_host_states, payer,
    moderate, generational, trapped, regional).

% Live where armed groups are embedded. They bear casualties, displacement, and destruction during operations decided in foreign capitals, with no vote, warning, or recourse. Their exit is flight within or across borders, which carries its own severe costs.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_strike_zones, payer,
    powerless, biographical, trapped, local).

% Organized armed groups operating from host-state territory. They are the object of the operations the doctrine enables: targeted, degraded, dispersed. At the same time, a legal framing that treats their attacks as matters of interstate self-defense raises their perceived significance and supplies recruitment narratives. Their options are dispersal, absorption into host-state politics, or continued insurgency.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, beneficiary).

% Hold veto power over any Council action that would authorize, condemn, or regularize cross-border responses. They collect flexibility from the practice: each retains the option to invoke comparable justifications for its own future operations while blocking corrective measures that would constrain anyone, including rivals. They do not administer the day-to-day practice; their lever is what never comes to a vote.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council_p5, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council_p5, agenda_setter).

% Provide basing, overflight, intelligence, and occasionally forces alongside the leading intervener. They gain intelligence sharing, basing arrangements, and standing with the lead state, while diluting their legal exposure by operating inside a coalition framed by the same justification. Their exit is withdrawing support, at the cost of the relationship and access.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, coalition_partner_states, beneficiary,
    powerful, biographical, mobile, global).

% Vote as blocs in Assembly debates and resolutions against the expansion of unilateral force. They hold numbers and rhetorical standing but no enforcement mechanism; their objections register in the record without altering operations. Many are potential host states and read the doctrine as a precedent aimed at them.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, general_assembly_majority_states, excluded,
    organized, generational, constrained, global).

% Academic lawyers, special rapporteurs, and expert groups who document invocations, draft principle sets, and publish assessments of the doctrine's legality. They shape the vocabulary of justification and criticism but command no enforcement; their seat is analytical.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_law_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterrorism_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fills a real gap in the use-of-force regime: it provides a pathway for responding to transnational armed-group attacks when the Charter's state-attribution framework yields no answer and the Security Council is veto-blocked, and it coordinates expectations among states about when cross-border force will be argued as lawful self-defense rather than aggression.
% TRANSFER_FUNCTION: Moves decision authority from the collective-security machinery to individual capable states' capitals, and moves the costs of suppressing transnational violence onto host-state territorial integrity and civilian populations in strike zones, without consent or compensation.
% ABSENT_VOICES: Strike-zone civilians have no seat anywhere in the process; armed groups are addressed only as targets; future states inheriting the eroded sovereignty norm are unrepresented; and host-state legislatures rarely ratify what their executives acquiesce to. The General Assembly majority speaks but is excluded from any decision that binds.
% DISAPPEARANCE_RATIONALE: If the doctrine and its justification practice vanished overnight, intervening states would face a stark choice between the narrow attribution standard (effective paralysis against transnational attacks) and candid illegality; host states would regain a meaningful sovereignty shield; and cross-border force would reorganize around either revived collective-security authorization or open violation. Either way the current middle path, and the legal bureaucracy sustaining it, disappears.
% FOUNDING_PROBLEM: After the September 2001 attacks, the Charter's state-attribution framework offered no lawful path for responding to a transnational armed group operating from a state that could not or would not suppress it, while the Security Council proved structurally incapable of authorizing timely force in most such cases.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Security Council resolutions 1368 and 1373, adopted before any unilateral doctrine was asserted, recognized the threat of transnational armed attacks; host states themselves have repeatedly acknowledged inability to control the groups on their territory, including in partnership requests to the United Nations and African Union; and legal scholarship hostile to the doctrine nonetheless concedes the attribution gap it responds to. No corroborating source attests that this doctrine's specific remedy is required — the corroboration covers the founding problem, not the solution.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64: the doctrine transfers operational discretion from the collective-security machinery to individual capable states and lands the costs of that discretion on host-state territorial integrity and strike-zone civilians, with no consent or compensation mechanism; it is bounded below the expansive reading because an actual attack must have occurred. Suppression is authored at 0.62 as a raw structural property — the engine scales only extractiveness, never suppression: once a determination is made, host states possess no military or legal recourse that reliably stops an operation, and Assembly objection registers without effect. The suppression_requirement series is authored deliberately because the story tracks enforcement-capacity change: improvised post-2001 justifications hardened into standing national legal frameworks, pre-cleared targeting protocols, and allied adoption, so the active force needed to hold the doctrine against the restrictive counter-movement rose across the interval. Theater_ratio 0.48: Article 51 notification letters are substantially ritual — the Council has never endorsed or condemned one — while threat assessment and targeting retain functional content. Accessibility_collapse 0.40: authorized collective action, host-state consent, and narrow attribution remain visible alternatives, though veto gridlock makes the first frequently unusable. Resistance 0.60: sustained bloc objection, judicial caution, and scholarly opposition. Coalition power is considered: host-state and Assembly-majority coalitions exist and vote together, but hold no enforcement lever, which is why trapped victims do not convert numbers into exit. All three series share one seven-point grid (t=0..24); no metric is sampled on a private grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the intervening state's seat the arrangement is gap-filling necessity it built and staffs; from the host state's seat the same structure is expropriation of the one norm that protects it; from the P5 seat it is prerogative flexibility preserved behind a veto; from the strike-zone civilian seat it is violence decided elsewhere without recourse. Same-level divergence is real: coalition partners and the lead intervener hold comparable global standing but different exposure — the partner dilutes legal responsibility while the lead owns the determination and the precedent. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. intervening_counterterrorism_states sit at the beneficiary end (d near 0.05) with arbitrage-grade exit — they can reframe each operation under whichever doctrine suits it. un_security_council_p5 and coalition_partner_states sit nearby (d roughly 0.15 to 0.25), collecting flexibility and standing without administering the practice. sovereignty_bypassed_host_states sit near the target end (d roughly 0.85): trapped, because the eroding norm is their principal asset and territory cannot be relocated. civilian_populations_in_strike_zones sit nearest full-target (d roughly 0.95): powerless and immobile except by flight. non_state_armed_groups are targets of the force (d roughly 0.90) with slight damping from the standing their attacks acquire under the doctrine's framing. general_assembly_majority_states bear diffuse norm-erosion costs (d roughly 0.70) with constrained exit. Global spatial scope raises verification difficulty, so the engine scales effective extraction modestly upward for the wide-scope seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents both mislabels. Reading the doctrine as pure coordination would hide the asymmetric extraction — sovereignty bypassed without consent, costs landed on the powerless; reading it as pure extraction would erase the genuine coordination function — a lawful-response pathway where attribution fails and the Council is blocked, a gap host-state behavior itself sometimes confirms through quiet cooperation and assistance requests. The founding problem is live (transnational groups in unwilling-or-unable jurisdictions persist; veto gridlock persists), so mandatrophy_resolved is false and the R5 status-by-verdict mismatch flag should stay quiet. The lifecycle risk runs forward, not backward: if the threat recedes while invocation becomes habitual prerogative, theater_ratio keeps climbing and the structure drifts toward extraction or inertial performance — the threat_subsidence omega and the theater series are the tripwires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel article_51_self_defense; what would adoption of a sibling reading change structurally?',
    'Track which reading each significant military power''s legal-adviser apparatus adopts and how victim and beneficiary sets shift with each adoption; the sibling readings are separate constraint files whose epsilon and victim sets can be compared directly.',
    'Under the narrow reading the sovereignty-bypass victims disappear and extraction collapses toward coordination-only; under the expansive reading victims broaden to populations merely deemed potential threats and extraction rises further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: reading-indexed structure of the Article 51 kernel.').

omega_variable(
    unwilling_unable_threshold_objectivity,
    'Is ''unwilling or unable'' an adjudicable standard applied to facts, or a unilateral determination by the intervening state dressed as a standard?',
    'Comparative case analysis of unwillingness and inability determinations: who made them, on what evidence, whether independent assessors would concur, and error rates across cases.',
    'If purely unilateral, the coordination component thins and the structure sits closer to pure extraction; if adjudicable in principle, the coordination function is genuine and the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_unable_threshold_objectivity, empirical, 'Objectivity of the doctrine''s trigger standard.').

omega_variable(
    doctrinal_reciprocity_asymmetry,
    'Is the doctrine available symmetrically to all states, or only to states with power-projection capacity?',
    'Inventory invocations by capability class: has any state without projection capacity successfully invoked the doctrine, and have weak-state invocations been received differently?',
    'Confirmed asymmetry concentrates extraction on weak hosts and sharpens the hybrid-versus-extraction boundary question; demonstrated symmetry would support a coordination-heavy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_reciprocity_asymmetry, empirical, 'Availability symmetry of the self-defense justification across capability classes.').

omega_variable(
    consent_route_substitutability,
    'Could consent-based routes (host invitation, regional-organization mandate, UN partnership) have delivered comparable suppression in the doctrine''s actual cases?',
    'Counterfactual comparison of cases where consent was obtained versus bypassed: operational outcomes, host-state stability, threat recurrence.',
    'If consent routes generally work, bypass is revealed preference for discretion rather than necessity, raising attributed extraction; if they reliably fail, part of the measured extraction is the price of the gap the doctrine fills.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_route_substitutability, empirical, 'Substitutability of consent-based alternatives for sovereignty-bypassing operations.').

omega_variable(
    threat_subsidence_persistence,
    'If the transnational-attack problem recedes, does the doctrine persist as habitual prerogative rather than response to need?',
    'Track invocation rates against independent threat indicators over time; divergence (invocations flat or rising while attacks decline) indicates inertia-driven persistence.',
    'Persistence without the founding problem would date a lifecycle transition toward inertial or purely extractive operation and move the R5 founding-problem status from live toward dead-in-practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_subsidence_persistence, empirical, 'Lifecycle tripwire: doctrine persistence versus founding-problem vitality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t4, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(arti_tr_t16, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(arti_tr_t24, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(arti_be_t4, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(arti_be_t16, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(arti_be_t24, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 24, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(arti_su_t4, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(arti_su_t16, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(arti_su_t24, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, un_collective_security_authorization).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Article 51 self-defense.' The single label conflates three structurally distinct claims with different epsilon values, victim sets, and failure modes: the narrow attribution reading (coordination-dominant, negligible sovereignty extraction), this unable-or-unwilling reading (hybrid: real gap-filling coordination plus asymmetric sovereignty extraction), and the expansive preventive reading (widest victim set, highest extraction). This file is the middle member. Its success exerts downstream pressure on the expansive reading (each normalized invocation lowers the bar for preventive claims) and displaces the narrow reading within any single adopting state's framework, which is why the family edges run through this node. Sibling stories document their own epsilon and link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
