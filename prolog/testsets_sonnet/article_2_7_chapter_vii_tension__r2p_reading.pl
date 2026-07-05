% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Responsibility to Protect Reading of the Sovereignty/Intervention Tension
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This story instantiates the R2P (Responsibility to Protect) reading of
 *   the Article 2(7)/Chapter VII tension: sovereignty is conditional on a
 *   state protecting its own population, and systematic atrocity (genocide,
 *   ethnic cleansing, war crimes, crimes against humanity) converts an
 *   internal matter into a trigger for international responsibility,
 *   potentially including Chapter VII-authorized intervention. This is a
 *   distinct constraint from the sovereignty_first_reading, which treats
 *   sovereignty as foundational and limits Chapter VII intervention to
 *   inter-state aggression absent consent. The two readings are not the same
 *   constraint measured differently — they instantiate different
 *   beneficiary/victim structures, different epsilon, and different
 *   classification. This story only characterizes the R2P reading; the
 *   sibling is authored separately.
 *
 * KEY AGENTS:
 *   - persecuted_populations: primary intended beneficiary (powerless/trapped) — the doctrine exists nominally for them
 *   - targeted_state_governments: primary payer (moderate/trapped) — lose the sovereignty shield once atrocity threshold is invoked against them
 *   - intervening_coalition_states: agenda_setter and structural beneficiary (powerful/arbitrage) — control invocation, gain strategic and normative capital
 *   - non_intervened_populations_in_unaligned_crises: secondary payer (powerless/trapped) — bear the cost of the doctrine's selective application
 *   - united_nations_security_council: institutional agenda_setter — formal authorization gatekeeper, filtered through P5 veto interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.61).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect Reading of the Sovereignty/Intervention Tension").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '83937c0d-714a-442b-aede-7b71f5d4a52b').
narrative_ontology:cs_kernel_codification('83937c0d-714a-442b-aede-7b71f5d4a52b', fixed_text).
narrative_ontology:cs_authority_grounding('83937c0d-714a-442b-aede-7b71f5d4a52b', distributed).
narrative_ontology:cs_reading_relation('83937c0d-714a-442b-aede-7b71f5d4a52b', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('83937c0d-714a-442b-aede-7b71f5d4a52b', foundational, sovereignty_as_conditional_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_as_conditional_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('83937c0d-714a-442b-aede-7b71f5d4a52b', sovereignty_as_conditional_responsibility, deontological).
narrative_ontology:cs_axiom('83937c0d-714a-442b-aede-7b71f5d4a52b', foundational, atrocity_threshold_triggers_international_concern).
narrative_ontology:cs_axiom_status(atrocity_threshold_triggers_international_concern, holdable).
narrative_ontology:cs_axiom_grounding('83937c0d-714a-442b-aede-7b71f5d4a52b', atrocity_threshold_triggers_international_concern, instrumental).
narrative_ontology:cs_reference_frame('83937c0d-714a-442b-aede-7b71f5d4a52b', westphalian_absolute_sovereignty).
narrative_ontology:cs_drift_state('83937c0d-714a-442b-aede-7b71f5d4a52b', post_rwanda_srebrenica_reckoning, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('83937c0d-714a-442b-aede-7b71f5d4a52b', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_governments).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, non_intervened_populations_in_unaligned_crises).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_as_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face systematic atrocity — genocide, ethnic cleansing, war crimes, crimes against humanity — from their own state or from armed actors the state cannot or will not restrain. The R2P reading gives their situation legal salience: it names the threshold (mass atrocity) that converts an internal matter into a matter of international concern. They have no vote in whether intervention occurs, no reliable channel to trigger it, and bear the direct costs of both the atrocity and, if intervention comes, the intervention itself.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, national).

% Governments accused of failing to protect (or of perpetrating harm against) their own populations. Under this reading, their Article 2(7) domestic-jurisdiction shield is conditional, not absolute — it can be pierced once the Security Council or a sufficiently invoked international consensus finds systematic atrocity. They cannot exit the international system, and weaker states cannot deter intervention the way permanent Security Council members can deter it against themselves or their allies.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_governments, payer,
    moderate, biographical, trapped, national).

% States and coalitions (often permanent Security Council members and their allies) that invoke the R2P framework to authorize or justify intervention. They control which atrocities become 'systematic' enough to trigger action, largely because they control the Council votes and the military/diplomatic capacity to act. They gain strategic influence, resource access, and normative legitimacy from being the enforcers of the doctrine; their own conduct toward their own populations or client states is rarely subjected to the same threshold test.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states, beneficiary).

% Populations suffering comparable or worse atrocities in states where no powerful coalition has strategic interest, or where a P5 veto protects the perpetrator (its own territory or an ally's). The doctrine's selective invocation means their suffering does not trigger the same international responsibility — they pay the cost of the doctrine's inconsistency without receiving its benefit.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, non_intervened_populations_in_unaligned_crises, payer,
    powerless, immediate, trapped, national).

% The Article 2(7) non-intervention norm itself, as a stabilizing feature of the post-1945 state system, is weakened each time this reading licenses intervention on contested atrocity findings — every invocation, especially a contested or selectively applied one, erodes the norm's general deterrent force against great-power intervention on pretextual grounds. Listed as a non-agent entity for completeness; it collects nothing but is degraded by the reading's operation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm_bearers, payer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm_bearers).

% The formal gatekeeper empowered to authorize Chapter VII action; under this reading, R2P provides the normative vocabulary for framing an atrocity as a threat to international peace and security sufficient to trigger Council authorization. Its P5 veto structure means the doctrine's application is filtered through great-power interest even when the atrocity threshold is met.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, united_nations_security_council, agenda_setter,
    institutional, generational, analytical, global).

% Non-permanent members and the broader UN membership that debate and sometimes resist R2P's expansion, fearing it legitimizes great-power intervention against them in the future. They participate in General Assembly debate but have no veto and are structurally excluded from the decisive authorization moment.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, smaller_un_member_states, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an internationally legible threshold — systematic mass atrocity — at which the international community can coordinate a collective response to protect populations that their own state has failed to protect, replacing ad hoc unilateral justification with a shared normative framework.
% TRANSFER_FUNCTION: Moves protective attention, military and diplomatic resources, and normative legitimacy toward populations whose crisis has been recognized as atrocity-threshold, and correspondingly transfers sovereignty-shielding authority away from targeted governments toward the intervening coalition and the Security Council.
% ABSENT_VOICES: Populations in unaligned or geopolitically inconvenient crises (Xinjiang, parts of Yemen, Tigray at various points) would object that the threshold is applied selectively; targeted state governments would argue the doctrine is a pretext for regime change dressed as protection; smaller states debate but cannot block application of the doctrine against themselves.
% DISAPPEARANCE_RATIONALE: If the R2P reading vanished, powerful states would lose a standing normative vocabulary for authorizing intervention on humanitarian grounds, but historically they intervened before the doctrine existed (using other justifications) and would likely continue to do so under different framings — the intervening coalitions dispute that the world would rearrange much; persecuted populations and human rights advocates argue the doctrine has genuinely raised the political cost of inaction in at least some cases (Libya 2011, discourse around Darfur) and its disappearance would remove a real, if imperfect, lever.
% FOUNDING_PROBLEM: The Rwandan genocide (1994) and the Srebrenica massacre (1995) occurred while the international community, bound by strict non-intervention norms and Security Council paralysis, failed to act despite clear warning and ongoing systematic killing — R2P was built to prevent that specific failure mode by establishing that sovereignty carries a protective obligation whose breach triggers international responsibility.
% FOUNDING_PROBLEM_CORROBORATION: The 2001 ICISS report and subsequent UN World Summit endorsement (2005) attest the founding problem as genuine and unresolved absent the doctrine. Independent scholars and NGOs outside the intervening states (e.g., critiques from Global South academics, African Union commentary on Libya 2011) attest that in practice the doctrine has been invoked selectively and sometimes instrumentally, suggesting the founding problem persists for some populations while the doctrine's protective promise is not uniformly honored — corroboration exists on both the live-problem and captured-instrument readings, from sources outside the states that benefit from invoking it.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, contested).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored substantially high (0.68 by 2024) not because protecting populations is itself extractive, but because the R2P reading's operationalization has consistently produced asymmetric outcomes: the doctrine is invoked against weaker, strategically inconvenient states (Libya) while structurally shielded from application against P5 members or their allies regardless of comparable atrocity thresholds (Syria's extended civil war, Xinjiang). Suppression is authored moderate-high (0.61) reflecting the coercive dimension of Chapter VII enforcement once invoked — military intervention, sanctions regimes, and the loss of sovereign control over internal affairs for the targeted state. Theater ratio (0.42) captures a real but partial performative dimension: the doctrine is frequently invoked rhetorically in diplomatic contexts without corresponding action (Syria, Myanmar), suggesting a portion of its operation is legitimating language rather than functional protection. Accessibility collapse is moderate (0.40) — states retain some alternative avenues (regional bodies, bilateral diplomacy, non-R2P legal frameworks) even after R2P is invoked, so alternatives have not collapsed as completely as in a mountain-type constraint. Resistance is high (0.78) reflecting sustained pushback from targeted states, the Non-Aligned Movement, and scholars who contest the doctrine's legitimacy and consistency.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening coalition's seat, this reading is coordination: a shared international norm solving the Rwanda/Srebrenica failure mode, backed by genuine humanitarian purpose. From the targeted state government's seat, the identical structure operates as conditional sovereignty weaponized selectively against the geopolitically weak — a payer seat experiencing enforced extraction of sovereign authority. From the non-intervened populations' seat, the doctrine's non-application is itself a harm: the promise of protection is not delivered, yet the doctrine's existence provides rhetorical cover suggesting the international community has 'done what it could.' The engine should compute divergent seat-level types from these structural asymmetries even though the story authors a single tangled_rope claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are declared beneficiaries but sit near the powerless/trapped end — they benefit only contingently, when intervention is actually mobilized on their behalf, which the historical record shows is inconsistent. Targeted state governments are declared victims with moderate power and trapped exit — they cannot leave the international system and cannot deter intervention the way a P5 member could. Intervening coalition states are both agenda_setter and beneficiary with arbitrage-grade exit — they choose when to invoke the doctrine and bear minimal reciprocal exposure. The Security Council itself sits at institutional power with analytical exit, reflecting its structural role as gatekeeper rather than direct party. Non-intervened populations are a second victim class whose harm is the doctrine's failure to activate, not its activation — this asymmetry (harm from both action and inaction, borne by different powerless populations) is central to why this reading computes as tangled_rope rather than a clean rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Rwanda/Srebrenica-style paralysis) remains genuinely live in the sense that mass atrocities continue to occur without reliable international response — this argues against pure mandatrophy. However, the doctrine's selective invocation pattern (heavily used to legitimate the 2011 Libya intervention, essentially unused for Syria's comparable or worse atrocities, never seriously considered for Xinjiang) suggests the mandate has partially decoupled from the founding problem and now serves as a discretionary legitimation resource for interventions coalition states would consider anyway. The tangled_rope classification, rather than a clean rope, is meant to hold both truths simultaneously: genuine coordination function for at least some crises, layered with asymmetric extraction (sovereignty-piercing applied selectively) for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_invocation_vs_genuine_norm,
    'Is the R2P doctrine, as actually practiced, a genuine emerging international norm constraining state sovereignty for protective purposes, or is it a discretionary legitimation resource that powerful states invoke only when it serves independently-held strategic interests?',
    'Comparative case analysis across atrocity-threshold crises (Rwanda, Kosovo, Darfur, Libya, Syria, Myanmar, Xinjiang) coding for (a) atrocity severity, (b) doctrine invocation, (c) intervening state strategic interest, to test whether invocation correlates more strongly with atrocity severity or with strategic interest.',
    'If invocation correlates primarily with strategic interest rather than atrocity severity, the tangled_rope classification is confirmed and the coordination function is substantially cover for extraction; if correlation with severity is strong and interest is incidental, the reading is closer to a genuine rope with imperfect execution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_vs_genuine_norm, empirical, 'Whether R2P invocation tracks atrocity severity or great-power strategic interest.').

omega_variable(
    sovereignty_norm_erosion_magnitude,
    'How much has the R2P reading''s operation actually eroded the general non-intervention norm''s deterrent force, versus how much erosion is attributable to other post-Cold War developments (unilateral humanitarian interventions predating R2P, the war on terror''s expansive self-defense claims, great-power revisionism)?',
    'Longitudinal study of state invocation of Article 2(7) defenses and their success/failure rate in international fora before and after 2005 (World Summit endorsement), controlling for other intervening legal and political developments.',
    'If erosion is substantially attributable to R2P specifically, the victim status of the sovereignty norm itself is well-supported; if erosion is primarily driven by other factors, R2P''s marginal contribution to norm erosion is smaller than the story''s extractiveness score implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_norm_erosion_magnitude, empirical, 'Attribution of sovereignty-norm erosion specifically to the R2P reading versus concurrent developments.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the disagreement between the r2p_reading and sovereignty_first_reading resolvable by evidence (e.g., showing R2P interventions reduce atrocity incidence on net), or is it a foundational disagreement about the moral priority of state sovereignty versus individual protection that no empirical finding can settle?',
    'This is inherently a conceptual/preference-type ambiguity rather than a strictly empirical one; the closest resolution mechanism is sustained international legal practice (opinio juris and state practice) crystallizing one reading into customary international law, which has not yet occurred as of this writing.',
    'If the disagreement is empirical and resolvable, future evidence on intervention outcomes could shift state practice toward one reading; if it is a foundational value disagreement, the kernel remains permanently contested regardless of outcome data, and both readings persist indefinitely as live alternatives held by different coalitions of states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the r2p_reading/sovereignty_first_reading split is empirically resolvable or a permanent normative fork.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1994, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(arti_tr_t2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(arti_tr_t2011, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(arti_tr_t2019, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1994, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(arti_be_t2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(arti_be_t2011, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(arti_be_t2019, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1994, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(arti_su_t2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(arti_su_t2011, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(arti_su_t2019, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__r2p_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This story and sovereignty_first_reading are the two declared readings of the article_2_7_chapter_vii_tension kernel. They share the same underlying legal text (UN Charter Articles 2(7) and Chapter VII) but instantiate structurally distinct constraints: this reading (r2p_reading) authors high extractiveness with persecuted populations as nominal beneficiaries and targeted states plus the sovereignty norm itself as victims; the sibling (sovereignty_first_reading) would author a different beneficiary/victim structure centered on protecting weaker states from pretextual intervention, with correspondingly different epsilon. Neither reading's epsilon should be treated as a measurement-basis variant of the other — each is authored independently per the ε-invariance principle, linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
