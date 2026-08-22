% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Article 2(7) Sovereignty-First Reading of the Non-Intervention Norm
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-first reading of the Article
 *   2(7)/Chapter VII kernel: state sovereignty is treated as the foundational
 *   norm of the UN Charter system, and any intervention into a state's
 *   internal affairs requires either that state's explicit consent or a
 *   Security Council Chapter VII authorization narrowly limited to responding
 *   to inter-state aggression or genuine threats to international peace and
 *   security. Under this reading, mass domestic atrocity — however severe —
 *   does not by itself cross the threshold for lawful external action absent
 *   Security Council authorization, which any of the five permanent members
 *   can block. The reading has genuine defensive coordination value for
 *   weaker and post-colonial states against renewed great-power domination,
 *   but as authored here, its dominant operative effect since the mid-1990s
 *   has been to shield perpetrating governments and gridlock any collective
 *   response to internal mass violence. This is the sovereignty-first reading
 *   ONLY; the r2p_reading sibling constraint authors the
 *   conditional-sovereignty alternative with its own ε and its own
 *   beneficiary/victim structure — the two are not to be averaged or blended.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.7).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Article 2(7) Sovereignty-First Reading of the Non-Intervention Norm").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '80659b3f-26ca-4b1b-9a5d-3649c2672e71').
narrative_ontology:cs_kernel_codification('80659b3f-26ca-4b1b-9a5d-3649c2672e71', formalized).
narrative_ontology:cs_authority_grounding('80659b3f-26ca-4b1b-9a5d-3649c2672e71', extraction).
narrative_ontology:cs_interpretation_layer_present('80659b3f-26ca-4b1b-9a5d-3649c2672e71').
narrative_ontology:cs_reading_relation('80659b3f-26ca-4b1b-9a5d-3649c2672e71', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('80659b3f-26ca-4b1b-9a5d-3649c2672e71', foundational, sovereignty_as_default_non_derogable_baseline).
narrative_ontology:cs_axiom_status(sovereignty_as_default_non_derogable_baseline, holdable).
narrative_ontology:cs_axiom_grounding('80659b3f-26ca-4b1b-9a5d-3649c2672e71', sovereignty_as_default_non_derogable_baseline, conventional).
narrative_ontology:cs_axiom('80659b3f-26ca-4b1b-9a5d-3649c2672e71', foundational, intervention_threshold_limited_to_interstate_aggression).
narrative_ontology:cs_axiom_status(intervention_threshold_limited_to_interstate_aggression, holdable).
narrative_ontology:cs_axiom_grounding('80659b3f-26ca-4b1b-9a5d-3649c2672e71', intervention_threshold_limited_to_interstate_aggression, conventional).
narrative_ontology:cs_reference_frame('80659b3f-26ca-4b1b-9a5d-3649c2672e71', westphalian_non_intervention_baseline).
narrative_ontology:cs_drift_state('80659b3f-26ca-4b1b-9a5d-3649c2672e71', post_rwanda_srebrenica_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('80659b3f-26ca-4b1b-9a5d-3649c2672e71', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regime_leaderships).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_governments).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, domestic_atrocity_populations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, ethnic_minority_communities_under_state_violence).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, internally_displaced_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Article 2(7) domestic-jurisdiction language to bar external scrutiny of internal violence against their own populations. They sit on or lobby within UN bodies, control the narrative of 'internal affairs,' and use the sovereignty norm as a legal shield against intervention regardless of the severity of harm inflicted.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regime_leaderships, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regime_leaderships, agenda_setter).

% Championed the strict sovereignty reading historically as protection against renewed great-power intervention and neo-colonial domination. Some of these governments genuinely rely on the norm defensively; others have folded it into cover for their own repressive practices. Their exit from the norm would mean forfeiting a hard-won post-colonial legal protection.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_governments, beneficiary,
    organized, generational, constrained, national).

% Control the Chapter VII authorization gate through veto power. They selectively invoke the inter-state-aggression threshold to permit or block intervention depending on their own strategic interests, using the sovereignty-first reading as a veto-compatible legal architecture that preserves their gatekeeping role.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Face genocide, mass killing, or systematic persecution characterized by their own government as a purely domestic matter. Under this reading, no external authority may act without either the perpetrating state's consent (which will not be given) or a Security Council authorization that a veto can block. Their only path to protection depends on the very actor causing harm, or on inter-state war breaking out around them.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, domestic_atrocity_populations, payer,
    powerless, immediate, trapped, local).

% Bear the brunt of state violence framed as counter-insurgency or internal security operations. The sovereignty-first reading treats these campaigns as beyond the threshold that triggers lawful international response, since no inter-state aggression has occurred. Fleeing across borders is often their only recourse.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, ethnic_minority_communities_under_state_violence, payer,
    powerless, biographical, trapped, regional).

% Displaced within their own state's borders by the violence the sovereignty norm shields from outside intervention. Because they have not crossed an international border, they fall outside even the more permissive refugee protection regimes, compounding their exposure.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, internally_displaced_persons, payer,
    powerless, immediate, trapped, national).

% Document atrocities and issue warnings but have no independent authority to act; their findings can be and often are dismissed as interference in domestic affairs. Their institutional voice sits adjacent to the decision but is not part of the Chapter VII gate.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_secretariat_and_human_rights_bodies, excluded,
    moderate, generational, constrained, global).

% Analyze the tension between Article 2(7) and Chapter VII, tracing how the sovereignty-first reading has been invoked selectively — asserted rigorously against weak states facing scrutiny, and relaxed when powerful states pursue their own strategic interventions.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, diffuse).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable baseline that prevents ad hoc unilateral invasions justified by shifting humanitarian pretexts, and gives newly independent and weaker states a shared legal shield against renewed external domination by former colonial or great powers.
% TRANSFER_FUNCTION: Moves protective legal cover from populations at risk of domestic atrocity to the governments committing or permitting the atrocity, and moves final say over intervention from any international body to the five veto-holding Security Council members.
% ABSENT_VOICES: The populations actually suffering the atrocities that would trigger a Chapter VII response have no seat in the Security Council chamber; UN human rights monitors and NGOs can document but cannot authorize; regional bodies with local knowledge are structurally subordinate to Security Council authorization under this reading.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading vanished overnight, humanitarian intervention would no longer require either state consent or a vetoable Security Council resolution; the entire architecture of non-intervention diplomacy, veto bargaining, and 'internal affairs' framing that authoritarian and post-colonial states rely on would need to be replaced with some conditional-sovereignty mechanism — a fundamentally different international order.
% FOUNDING_PROBLEM: Built after decades of colonial domination and great-power intervention in weaker states' internal affairs, to prevent powerful states from using humanitarian or political pretexts to invade or control less powerful ones.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and legal historians outside the current beneficiary set (e.g., historians of decolonization, non-aligned movement scholars) corroborate that the founding problem — protection from renewed great-power domination — was real and remains partly live for smaller states facing coercive diplomacy. However, human rights monitors, R2P advocates, and survivors of interventions-not-taken (Rwanda, Srebrenica, Syria) attest from outside the beneficiary set that the same norm has been repurposed to shield perpetrators of mass atrocity, a function the founding architects did not intend and that current beneficiary states now rely upon.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78) is authored high because the reading's practical operation over the post-Cold-War period has consistently blocked or delayed responses to Rwanda, Srebrenica, Darfur, and Syria — the pattern is not occasional friction but a structural veto-gated bottleneck. Suppression (0.7) reflects that alternatives (regional intervention, unilateral humanitarian action, lower-threshold Council authorization) are actively foreclosed by the doctrine's own logic, not merely discouraged. Theater ratio (0.4) captures the substantial gap between the norm's stated protective purpose (shielding weak states from domination) and its accumulated actual function (shielding perpetrating governments) — genuine defensive function remains for some post-colonial states, but a rising share of invocations since 1994 are protective cover for domestic violence rather than protection from external domination. Accessibility collapse (0.6) and resistance (0.55) reflect that alternative doctrines (R2P, regional intervention norms) exist and are actively contested rather than fully suppressed — this is not a settled mountain, it is a live, resisted legal-political battleground.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regime leaderships and permanent Security Council members are structural beneficiaries: the former gain a legal shield against outside scrutiny, the latter gain a veto-gated gatekeeping role that lets them selectively invoke or block intervention for their own strategic interests. Post-colonial state governments occupy a genuinely mixed position — real defensive benefit against renewed domination, but institutional beneficiary status when the same shield is turned toward their own domestic practices. Domestic atrocity populations, ethnic minority communities, and internally displaced persons are the targets: trapped, powerless, and structurally dependent on the very veto mechanism that can block their protection. Their directionality sits at the full-target end — no meaningful exit exists inside the state, and cross-border flight does not activate the Chapter VII authorization mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents collapsing this into either a pure Rope (ignoring how the norm now principally protects perpetrators) or a pure Snare (ignoring the genuine, historically grounded coordination value the sovereignty norm provides to weaker states against great-power domination). It is authored as Tangled Rope: a real coordination function (protecting weak states from external domination) persists alongside asymmetric extraction (protecting perpetrating governments from accountability for domestic atrocity), held together by active Security Council enforcement machinery — the veto. The founding_problem_status is authored 'contested' precisely because both readings of its current function are independently corroborated by parties outside the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_norm_genealogy_vs_capture,
    'Is the sovereignty-first reading best understood as an enduring, legitimate protection for weaker states against great-power domination that has been opportunistically captured by some perpetrating regimes, or has capture become the norm''s dominant operative function such that its protective genealogy is now largely nominal?',
    'Comparative historical analysis of Security Council invocation patterns: track the ratio of sovereignty-norm invocations that successfully blocked genuine external domination attempts versus invocations that blocked accountability for domestic mass atrocity, across the post-1945 and post-1994 periods.',
    'If genuine protective invocations still substantially outnumber protective-cover invocations, the Tangled Rope classification with strong residual coordination value holds. If cover invocations now dominate, the constraint''s effective structure would drift toward Snare notwithstanding its formal Tangled Rope architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_norm_genealogy_vs_capture, empirical, 'Whether the sovereignty-first reading remains predominantly protective or has become predominantly a shield for atrocity.').

omega_variable(
    committer_kernel_reading_choice,
    'This story authors the sovereignty_first_reading of the article_2_7_chapter_vii_tension kernel. The sibling r2p_reading authors sovereignty as conditional on population protection, with systematic atrocity itself triggering international responsibility. Which reading a given international actor or scholar adopts is itself a structural fact about their institutional position, not a resolvable empirical question.',
    'No single resolution mechanism exists across readings — this is the committer axis itself. What CAN be tracked is which institutional actors (states, UN bodies, regional organizations, scholars) hold which reading, and how that distribution shifts over time (e.g., growing R2P endorsement in UN General Assembly resolutions since 2005 versus continued sovereignty-first invocation in Security Council practice).',
    'If the r2p_reading were to become the dominant operative interpretation within Security Council practice (not merely General Assembly rhetoric), the sovereignty-first reading''s beneficiary set (authoritarian and post-colonial state governments) would lose their primary legal shield, and the veto-gating beneficiary position of permanent members would be substantially eroded for cases meeting an atrocity threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_choice, conceptual, 'The kernel-level contest between sovereignty-first and R2P readings, and which institutional actors hold which reading.').

omega_variable(
    chapter_vii_threshold_ambiguity,
    'Does ''threat to international peace and security'' under Chapter VII genuinely exclude purely domestic atrocity as a triggering condition, or has Security Council practice (e.g., authorizing action in Somalia, Rwanda''s aftermath, Libya) already established domestic atrocity as a sufficient trigger in some cases, undermining the sovereignty-first reading''s own claimed boundary?',
    'Systematic review of Security Council resolution language and voting patterns to determine whether domestic-atrocity-only situations have in practice met the Chapter VII threshold without an inter-state aggression component.',
    'If domestic atrocity alone has repeatedly met the threshold in Council practice, the sovereignty-first reading''s own textual claim (limited to inter-state aggression) would be shown as narrower than actual state practice, weakening its internal coherence and its extraction claim would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chapter_vii_threshold_ambiguity, empirical, 'Whether Security Council practice already exceeds the sovereignty-first reading''s stated inter-state-aggression limitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(arti_tr_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(arti_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(arti_tr_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(arti_be_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(arti_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(arti_be_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1994, 0.68).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(arti_su_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(arti_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(arti_su_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1994, 0.65).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, genocide_convention_enforcement_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of two sibling readings of the article_2_7_chapter_vii_tension kernel. The sovereignty_first_reading (this file) authors high extraction (0.78) with beneficiaries including authoritarian and post-colonial governments and permanent Security Council members, and victims including populations under domestic atrocity. The sibling r2p_reading authors sovereignty as conditional on population protection and would carry a distinct beneficiary/victim structure and a distinct ε reflecting the coordination value of enabling protective intervention. The two files are not to be merged or averaged; they are linked here to preserve the kernel-family structure for contamination and network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
