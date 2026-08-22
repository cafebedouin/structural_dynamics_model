% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Loyalist Restoration Reading: Unmediated Imperial Sovereignty Mandate
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This story authors the loyalist restoration reading of the imperial
 *   mandate kernel: the claim that the divine mandate is only satisfied by
 *   the emperor's unmediated, personal exercise of sovereignty, rendering any
 *   intermediary governing structure — shogunate, domain lords, hereditary
 *   samurai administration — an illegitimate usurpation regardless of how
 *   long-standing or functionally effective it has been. This reading treats
 *   institutional rupture (abolition of the shogunate and the domain system)
 *   as constitutionally necessary rather than merely one policy option among
 *   several, and treats explicit imperial initiative as the sole legitimate
 *   channel for authorizing modernization and foreign engagement. The sibling
 *   reading, bakufu_delegation_reading, holds that the mandate's
 *   legitimacy-granting function is separable from its governing function and
 *   that delegated rule can be fully legitimate — that is a different
 *   constraint, not a variant measurement of this one, and is not described
 *   further here per the ε-invariance rule.
 *
 * KEY AGENTS:
 *   - imperial_court_loyalists: primary agenda-setters who articulate and enforce the unmediated-sovereignty doctrine
 *   - restorationist_domain_factions: organized military-political beneficiaries converting doctrine into state power
 *   - meiji_state_architects: institutional beneficiaries who administer the new centralized state in the emperor's name
 *   - tokugawa_shogunate_officials: primary targets, delegitimized wholesale by the reading's core premise
 *   - hereditary_samurai_retainers: diffuse victims losing stipend and status in the abolition of hereditary structures
 *   - comparative_constitutional_historians: analytical observers noting administrative continuity beneath the doctrinal rupture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.62).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Loyalist Restoration Reading: Unmediated Imperial Sovereignty Mandate").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'e3c73add-83de-4838-93c6-55c65e3c9caf').
narrative_ontology:cs_kernel_codification('e3c73add-83de-4838-93c6-55c65e3c9caf', implicit).
narrative_ontology:cs_authority_grounding('e3c73add-83de-4838-93c6-55c65e3c9caf', lineage).
narrative_ontology:cs_interpretation_layer_present('e3c73add-83de-4838-93c6-55c65e3c9caf').
narrative_ontology:cs_reading_relation('e3c73add-83de-4838-93c6-55c65e3c9caf', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('e3c73add-83de-4838-93c6-55c65e3c9caf', foundational, sovereignty_and_governance_are_inseparable).
narrative_ontology:cs_axiom_status(sovereignty_and_governance_are_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('e3c73add-83de-4838-93c6-55c65e3c9caf', sovereignty_and_governance_are_inseparable, deontological).
narrative_ontology:cs_axiom('e3c73add-83de-4838-93c6-55c65e3c9caf', secondary, delegated_rule_constitutes_usurpation_regardless_of_duration).
narrative_ontology:cs_axiom_status(delegated_rule_constitutes_usurpation_regardless_of_duration, holdable).
narrative_ontology:cs_axiom_grounding('e3c73add-83de-4838-93c6-55c65e3c9caf', delegated_rule_constitutes_usurpation_regardless_of_duration, conventional).
narrative_ontology:cs_reference_frame('e3c73add-83de-4838-93c6-55c65e3c9caf', unmediated_imperial_sovereignty_original_mandate).
narrative_ontology:cs_drift_state('e3c73add-83de-4838-93c6-55c65e3c9caf', bakumatsu_crisis_and_meiji_consolidation, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('e3c73add-83de-4838-93c6-55c65e3c9caf', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court_loyalists).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restorationist_domain_factions).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, meiji_state_architects).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, tokugawa_shogunate_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_retainers).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, domain_populations_under_bakufu_rule).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, domain_populations_under_bakufu_rule).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unified_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, imperial_restoration_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Court nobles and allied domain samurai who assert that legitimate rule requires the emperor to personally exercise sovereignty rather than delegate it. They set the ideological agenda for restoration, framing centuries of shogunal governance as usurpation to be corrected, and stand to gain court offices and administrative authority once direct rule is restored.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court_loyalists, agenda_setter,
    organized, generational, identity_locked, national).

% Domains such as Satsuma and Choshu that mobilize military and political resources behind the unmediated-sovereignty reading, converting ideological legitimacy into new positions of state power once the shogunate is dissolved. Their exit option is genuinely mobile: they can shift allegiance and are doing so strategically.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, restorationist_domain_factions, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, restorationist_domain_factions, agenda_setter).

% Officials who use the unmediated-sovereignty doctrine to justify dismantling the Tokugawa administrative apparatus and consolidating a centralized state under nominal direct imperial rule, while in practice exercising delegated administrative power themselves under the emperor's name.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, meiji_state_architects, beneficiary,
    institutional, civilizational, arbitrage, national).

% Officials of the bakufu whose entire governing authority is delegitimized overnight by this reading's core claim that delegated rule was never legitimate. They have no framework within which to argue for continued authority once the unmediated-sovereignty premise is accepted; their institutional position simply ceases to have standing.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, tokugawa_shogunate_officials, payer,
    powerful, biographical, trapped, national).

% Retainers whose stipends, status, and social role are tied to the shogunate and domain lord hierarchies that this reading brands as intermediary usurpation. Many lose stipends and social position in the abolition of the domain system that follows restoration; their exit is blocked by lack of alternative livelihood and by identity built around a hereditary military role.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_retainers, payer,
    moderate, biographical, trapped, regional).

% Peasants and townspeople under domain and shogunal administration who bear the costs of civil war, conscription, and administrative upheaval that institutional rupture requires, while also potentially benefiting from later centralizing reforms undertaken in the emperor's name. They have no voice in whether the mandate is read as unmediated or delegated.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, domain_populations_under_bakufu_rule, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, domain_populations_under_bakufu_rule, beneficiary).

% Western powers negotiating treaties who are structurally excluded from the internal legitimacy dispute but whose demands for engagement become a proof-point this reading uses: that only explicit imperial initiative, not shogunal delegation, can legitimately authorize modernization and foreign relations.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_treaty_powers, excluded,
    institutional, generational, analytical, global).

% Scholars analyzing the restoration as a case of legitimacy-claim substitution: an institutional rupture justified by a doctrine of unmediated sovereignty that, once state power was consolidated, was itself administered through new delegated bureaucracies functionally similar to what it replaced.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, comparative_constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, diffuse).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, ideologically decisive standard for resolving competing claims to legitimate authority during a period of institutional crisis: rather than negotiating power-sharing among shogunate, domains, and court, the unmediated-sovereignty claim supplies one authoritative test — direct imperial exercise — that different factions can rally around to dissolve deadlock.
% TRANSFER_FUNCTION: Moves administrative authority, stipend income, land revenue rights, and social status away from shogunal and domain-hereditary structures and toward the restorationist coalition and the new centralized state apparatus acting in the emperor's name.
% ABSENT_VOICES: Domain populations who bear the costs of civil conflict and abrupt administrative reorganization have no seat in the legitimacy debate; the debate is conducted entirely among court nobles, domain samurai elites, and shogunal officials. Foreign treaty powers, whose pressure catalyzes the crisis, are also excluded from the internal doctrinal contest despite shaping its stakes.
% DISAPPEARANCE_RATIONALE: If the unmediated-sovereignty reading had not prevailed, the bakufu delegation framework remains available and shogunal or reformed-shogunal governance could have persisted or evolved incrementally; the abolition of the domain system, the dismantling of the samurai class as a legal status, and the specific form of centralized Meiji administration were direct consequences of accepting this reading rather than the delegation reading.
% FOUNDING_PROBLEM: A crisis of governing legitimacy in the face of foreign intrusion and domestic fiscal-military strain, in which the shogunate's delegated authority appeared unable to respond decisively, created demand for a doctrine that could authorize sweeping institutional replacement rather than reform within the existing delegated structure.
% FOUNDING_PROBLEM_CORROBORATION: Restorationist domain leaders and Meiji-era official historiography attest the founding problem was real and required institutional rupture. Independent comparative historians outside the restorationist coalition — including scholars studying the actual continuity of bureaucratic personnel and administrative technique across the Tokugawa-Meiji transition — attest that much of the shogunate's administrative capacity was retained and re-deployed under the new imperial framing, suggesting the unmediated-sovereignty doctrine functioned partly as a legitimating narrative for a power transfer that did not require the degree of rupture it claimed.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.62 across the interval as the doctrine moves from rhetorical claim (early Bakumatsu agitation) to enforced state restructuring (domain abolition, samurai stipend commutation, conscription replacing hereditary military service) — the doctrine's application transfers real resources, not merely honorific status. Suppression peaks sharply at the midpoint (0.78) during the Boshin War and early domain-abolition enforcement, when resistance from shogunal loyalists and disaffected samurai required active military and administrative suppression, then eases somewhat as the new order consolidates but remains elevated relative to baseline because coercive enforcement of the unified-sovereignty framework against residual samurai privilege continues into the 1870s. Theater ratio is moderate and rises modestly (0.15 to 0.32 then settling to 0.28): considerable restoration-era ritual (imperial processions, court ceremony revival) accompanies the substantive transfer of administrative power, but the underlying transfer is real, not merely performative, which keeps theater ratio well below dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of imperial court loyalists and restorationist domains, the arrangement reads as rope or even mountain-adjacent: a correction of long-standing usurpation, restoring what divine mandate always required, with no illegitimate extraction involved. From the seat of shogunal officials and samurai retainers, the identical structural event reads as tangled rope shading toward snare: a doctrine deployed to seize their institutional position and material stipends under cover of theological necessity. The engine computes both seat-relative classifications from the same structural data; this story does not adjudicate between them but authors the metrics as the generating analyst assesses the standing arrangement under contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial court loyalists and restorationist domain factions sit near the beneficiary end: they set the doctrinal agenda and convert legitimacy claims into concrete administrative and military power. Meiji state architects likewise benefit, exercising arbitrage-grade exit because their institutional position transcends any single prior loyalty. Tokugawa officials and hereditary samurai retainers sit near the target end: trapped exit options, biographical time horizons, and total delegitimization of their institutional standing under the reading's core premise. Domain populations are dual-positioned — bearing war and reorganization costs while later drawing some benefit from centralizing reforms undertaken in the emperor's name — captured here via a secondary beneficiary role alongside payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a governance crisis under foreign pressure that the delegated shogunal structure appeared unable to resolve — is genuinely contested as to whether it required doctrinal rupture or could have been solved through reformed delegation. The mandatrophy risk here is double-edged: on one hand, treating institutional rupture as the ONLY legitimate response to a live governance crisis is itself a totalizing move that forecloses incremental reform; on the other hand, once the new centralized Meiji state stabilizes, its own administration is substantially delegated (ministries, prefectural governors, a professional bureaucracy) in ways structurally similar to what it replaced — suggesting the unmediated-sovereignty doctrine's function was largely a one-time legitimating device for a specific power transfer rather than an enduring constitutional principle the new state continued to honor in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_sovereignty_kernel_committer_structure,
    'Is the loyalist restoration reading''s demand for unmediated imperial sovereignty a genuine doctrinal recovery of an original, corrupted mandate, or a strategically constructed legitimacy claim assembled by restorationist factions to justify a power transfer they sought on other (fiscal, military, anti-foreign) grounds?',
    'Textual and historiographical analysis of pre-Bakumatsu imperial-mandate doctrine to establish whether ''unmediated exercise'' was a longstanding minority theological position activated by crisis, or a novel synthesis produced specifically to serve restorationist political needs in the 1850s-1860s.',
    'If the doctrine is a longstanding minority position, this reading has genuine continuity with earlier imperial theory and its claim to be a ''restoration'' rather than an innovation is stronger. If it is a novel synthesis assembled for the crisis, the reading is better understood as legitimating cover for a power transfer already underway on other grounds, strengthening a tangled_rope-toward-snare reading of its operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_sovereignty_kernel_committer_structure, conceptual, 'Whether the unmediated-sovereignty doctrine is doctrinal recovery or constructed legitimation, and where the disagreement with the bakufu_delegation_reading is structurally located.').

omega_variable(
    administrative_continuity_beneath_doctrinal_rupture,
    'How much of the actual governing apparatus (personnel, fiscal technique, provincial administration) persisted across the transition despite the doctrine''s claim of total institutional rupture?',
    'Prosopographical study of bureaucratic personnel and administrative technique carried over from bakufu and domain administrations into the early Meiji state.',
    'High continuity would suggest the unmediated-sovereignty doctrine functioned primarily as a legitimating narrative for what was substantially administrative continuity under new ownership — supporting the mandatrophy analysis that the doctrine''s practical function diverged from its stated theological necessity. Low continuity would support the reading''s own claim that genuine institutional rupture occurred.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_continuity_beneath_doctrinal_rupture, empirical, 'Degree of administrative continuity beneath the claimed institutional rupture.').

omega_variable(
    coercion_versus_persuasion_in_domain_abolition,
    'Was the abolition of the domain system and samurai status accepted because domain lords and samurai were persuaded of the doctrine''s legitimacy, or because they faced overwhelming military and fiscal coercion with no viable alternative?',
    'Comparative study of domains that resisted versus those that acquiesced quickly, examining whether acquiescence correlates with doctrinal conviction or with proximity to restorationist military force and fiscal dependency on the new state.',
    'If acquiescence tracks coercive proximity rather than conviction, the suppression metric is understated relative to the doctrine''s actual coercive character, and the classification should weight more heavily toward snare-like extraction from samurai retainers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_versus_persuasion_in_domain_abolition, empirical, 'Whether domain and samurai acceptance of restoration reflects genuine persuasion or coercion under military-fiscal pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(impe_tr_t8, imperial_mandate__loyalist_restoration_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(impe_tr_t16, imperial_mandate__loyalist_restoration_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(impe_tr_t24, imperial_mandate__loyalist_restoration_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(impe_tr_t32, imperial_mandate__loyalist_restoration_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(impe_tr_t40, imperial_mandate__loyalist_restoration_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(impe_be_t8, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(impe_be_t16, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(impe_be_t24, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(impe_be_t32, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(impe_be_t40, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(impe_su_t8, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(impe_su_t16, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(impe_su_t24, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(impe_su_t32, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(impe_su_t40, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% This story and imperial_mandate__bakufu_delegation_reading are siblings under the imperial_mandate kernel, not two measurements of one constraint. The delegation reading treats shogunal governance as a legitimate exercise of a mandate whose legitimacy-granting and governing functions are separable, with correspondingly low or near-zero extraction from shogunal officials (they are not usurpers under that reading). This restoration reading treats the same historical shogunal governance as illegitimate usurpation requiring rupture, with correspondingly high extraction from and suppression of shogunal officials and samurai retainers. The ε values differ by a wide margin (roughly 0.62 here versus a substantially lower value expected under the delegation reading) precisely because the two readings disagree about who the legitimate sovereign administrative actor is — this is the committer-axis disagreement the omega variables in this file document, not a parameter that could be reconciled into a single shared ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
