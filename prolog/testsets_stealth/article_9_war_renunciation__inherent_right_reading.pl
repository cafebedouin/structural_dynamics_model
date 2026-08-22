% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right Reading — Minimum Necessary Defense Threshold
 *   domain: constitutional/security/political
 *
 * SUMMARY:
 *   This story instantiates the inherent-right reading of the Article 9
 *   war-renunciation kernel: the proportionality-threshold interpretation
 *   under which Japan's Self-Defense Forces are constitutionally legitimate
 *   when limited to the minimum necessary for territorial defense, while
 *   aggressive war is renounced. This is the reading that has been operative
 *   in Japanese government practice since the early 1950s. The threshold
 *   solves a genuine coordination problem — reconciling a renunciation
 *   constitution with sovereign defense needs — while asymmetrically
 *   extracting from the pacifist constituency (whose constitutional
 *   commitment is progressively diluted with each expansion) and channeling
 *   fiscal resources to the defense establishment under a self-assessed
 *   elastic standard. The constraint requires active enforcement: the
 *   government must continuously maintain the interpretation through cabinet
 *   decisions, court standing doctrines that exclude citizen challenges, and
 *   public justification of each expansion as still within 'minimum
 *   necessary.' The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as tangled_rope (genuine coordination + asymmetric extraction) while the
 *   authored metrics describe the actual operation of the threshold, which
 *   has become progressively more elastic over the interval.
 *
 * KEY AGENTS:
 *   - japanese_cabinet: agenda_setter (institutional/mobile) — controls interpretation of 'minimum necessary' and approves all force expansions
 *   - self_defense_forces_establishment: primary beneficiary (organized/identity_locked) — gains organizational legitimacy and budget from the threshold
 *   - japanese_taxpayers: payer (moderate/constrained) — fund the SDF under the elastic standard; also receive defense services
 *   - pacifist_constituency: payer (organized/identity_locked) — bear constitutional dilution with each threshold expansion
 *   - united_states_government: secondary beneficiary (institutional/mobile) — stable, limited allied Japan
 *   - neighboring_states: affected third parties (institutional/constrained) — benefit from the cap, bear costs of an armed Japan
 *   - article_9_litigants: excluded (powerless/trapped) — denied judicial standing to challenge the interpretation
 *   - constitutional_scholars: analytical observer (analytical/analytical) — document the gap between text and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.55).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.42).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right Reading — Minimum Necessary Defense Threshold").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional/security/political").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'fd32a800-6da1-4a4f-8a45-cb1bd3cf726d').
narrative_ontology:cs_kernel_codification('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', fixed_text).
narrative_ontology:cs_authority_grounding('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', lineage).
narrative_ontology:cs_interpretation_layer_present('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d').
narrative_ontology:cs_reading_relation('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', foundational, inherent_sovereign_self_defense).
narrative_ontology:cs_axiom_status(inherent_sovereign_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', inherent_sovereign_self_defense, deontological).
narrative_ontology:cs_axiom('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', foundational, minimum_necessary_threshold).
narrative_ontology:cs_axiom_status(minimum_necessary_threshold, holdable).
narrative_ontology:cs_axiom_grounding('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', minimum_necessary_threshold, deontological).
narrative_ontology:cs_reference_frame('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', postwar_defensive_minimum_settlement).
narrative_ontology:cs_drift_state('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', contemporary_security_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd32a800-6da1-4a4f-8a45-cb1bd3cf726d', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, self_defense_forces_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, united_states_government).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, article_9_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, neighboring_states).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, neighboring_states).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, inherent_sovereign_self_defense_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, proportionality_threshold_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and revises the government's interpretation of what constitutes 'minimum necessary' defense capacity under Article 9. Approves Self-Defense Force structure, deployments, procurement, and budget requests. The cabinet's interpretation has expanded over decades — from territorial defense to peacekeeping operations, anti-terrorism deployments, and most recently counterstrike capabilities. Can shift the interpretation through cabinet decision without constitutional amendment; constrained by electoral accountability, coalition politics, and public opinion.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_cabinet, agenda_setter,
    institutional, generational, mobile, national).

% Operates as Japan's armed forces under the constitutional designation 'Self-Defense Forces.' Receives budget, personnel authority, and procurement authorization through the threshold's legitimacy — without the threshold reading, the forces would face constitutional challenge to their existence. The institution's identity is constituted by its constitutional position: its doctrine, equipment choices, and public presentation are organized around the 'self-defense' framing. Renaming or reconstitution as a conventional military would require breaking the identity frame that gives the institution its constitutional cover.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces_establishment, beneficiary,
    organized, generational, identity_locked, national).

% Fund the defense budget through general taxation — Japan ranks among the top ten defense spenders globally. Receive territorial defense and disaster-response services from the Self-Defense Forces in return. Cannot individually opt out of the defense budget; influence it only through electoral channels. The 'minimum necessary' standard caps what they pay relative to a full military establishment, but the cap is assessed by the same government that proposes the budget.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers, beneficiary).

% Constituted by commitment to the constitutional renunciation of war — includes religious organizations, peace movements, and a substantial bloc of constitutional lawyers. Bear the cost of each expansion of 'minimum necessary': every increase in Self-Defense Force capability erodes the categorical commitment they understand the constitution to embody. Their identity is bound to the pacifist reading; abandoning it would require abandoning a constitutive commitment, not merely changing a policy preference.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_constituency, payer,
    organized, generational, identity_locked, national).

% Maintains the US-Japan Security Treaty under which Japan hosts US bases and the US provides extended deterrence. Benefits from a Japan that maintains sufficient defense capability to complement US regional strategy but does not develop independent power projection that might complicate alliance management. Can adjust alliance posture and force structure; not bound by the Japanese constitutional threshold.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, united_states_government, beneficiary,
    institutional, generational, mobile, global).

% Regional states — China, South Korea, and others — face a Japan with armed forces under a constitutional cap. The threshold limits Japanese military expansion (reducing regional arms-race pressure) but they also face a Japan with any armed forces at all (a security concern given historical memory). Cannot control Japan's interpretation of its own constitution; can respond only through their own military postures and diplomacy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, neighboring_states, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, neighboring_states, payer).

% Japanese citizens who have repeatedly sought judicial review of the government's Article 9 interpretation — challenging specific deployments, budget items, and force expansions as unconstitutional. Courts have consistently declined to reach the merits, holding that Article 9's interpretation is a political question outside judicial competence. No institutional channel exists through which their reading can be adjudicated.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, article_9_litigants, excluded,
    powerless, biographical, trapped, national).

% Academic lawyers and political theorists who analyze Article 9's meaning and the government's interpretation. A majority of constitutional scholars have historically held that the Self-Defense Forces are unconstitutional under a strict reading of the text — a view that has had no effect on government practice. Provide the analytical record of the gap between constitutional text, government interpretation, and operational practice.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, self_defense_forces_establishment).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the constitutional renunciation of war with the practical requirements of national defense, allowing Japan to maintain armed forces under a proportionality limit while preserving the postwar pacifist settlement. Coordinates the domestic compromise between constituencies that demand security capability and constituencies committed to the renunciation principle.
% TRANSFER_FUNCTION: Moves fiscal resources from Japanese taxpayers to the defense establishment (personnel, procurement, bases, operations) under an elastic 'minimum necessary' standard assessed by the government itself. Also transfers interpretive authority from courts (which decline Article 9 cases) to the cabinet, concentrating constitutional interpretation in the institution with the strongest expansionary incentive.
% ABSENT_VOICES: Japanese citizens who have sought judicial review of Article 9 interpretation are denied standing — courts classify the question as political, not justiciable. Strict-pacifist constitutional scholars who argue the categorical prohibition reading have no institutional channel; their reading remains academically live but has lost all traction in government interpretation. Both would object to the threshold's current operation if they had a seat.
% DISAPPEARANCE_RATIONALE: If the inherent-right reading vanished overnight, Japan would face immediate constitutional crisis: under the strict-pacifist reading the Self-Defense Forces would be unconstitutional and face dissolution, forcing either disarmament or constitutional amendment; under the collective-self-defense reading the territorial-defense threshold would dissolve, permitting military expansion. The postwar settlement — Self-Defense Force legitimacy, US-Japan alliance burden-sharing, regional security architecture — depends on this intermediate position holding.
% FOUNDING_PROBLEM: Post-occupation Japan faced a security dilemma: the 1947 constitution renounced war, but Cold War pressures (Soviet capability, later Chinese and North Korean arsenals) and the 1950 US occupation reversal (MacArthur's authorization of the National Police Reserve) created demand for defense capability. The inherent-right reading was constructed to bridge the gap between the constitutional text and the security reality without formal revision.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by the historical record of the 1950 rearmament reversal, by US State Department archival records showing the occupation's shift, and by security analyses from outside the Japanese government. However, whether the 'minimum necessary' standard remains calibrated to that founding problem — versus to the defense establishment's institutional self-perpetuation — is contested by pacifist constitutional scholars outside the benefiting parties, who argue the threshold has outgrown its original defensive rationale.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end) — the threshold permits substantial military spending (Japan is a top-ten defense spender globally) justified by an elastic standard, but the threshold also caps that spending relative to the collective-self-defense alternative. Suppression is moderate (0.42) — pacifist dissent remains politically expressible but is institutionally foreclosed through court standing doctrines and government control of interpretation. Suppression is a raw structural property, unscaled by directionality or scope. Theater is moderate (0.42) — the 'minimum necessary' language persists but an increasing share of its function is justifying whatever budget the government proposes rather than genuinely limiting force structure. Accessibility collapse is 0.45 — once the threshold reading is understood, the alternative readings (strict pacifism, collective self-defense) remain conceptually accessible but are institutionally difficult to adopt without constitutional amendment. Resistance is 0.50 — substantial (mass protests, scholarly opposition, recurring court challenges) but channeled into forms that lack institutional traction. The measurement series run on one shared time grid (t=0, 10, 20, 30, 40, 50, 60, 75) so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently. From the Self-Defense Forces' position, the threshold is what makes their existence constitutionally possible — it is enabling, not constraining; without it they face dissolution. From the pacifist constituency's position, the same threshold is a mechanism of constitutional dilution — each expansion of 'minimum necessary' erodes the renunciation commitment they understand the constitution to embody. From the taxpayer seat, the threshold is roughly symmetric — they pay for defense they receive. From the cabinet's position, the threshold is a flexible instrument of policy — it can be calibrated to whatever security environment obtains. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Self-Defense Forces establishment sits near the beneficiary end (d low) — the threshold legitimizes their existence, funds their operations, and provides constitutional cover for their mission. The US government also benefits (d low) — a stable, limited allied Japan complements US regional strategy without independent power projection. Japanese taxpayers sit near symmetric (d ≈ 0.45) — they pay for the defense budget but receive territorial defense and disaster response in return; the threshold caps what they pay but the cap is self-assessed. The pacifist constituency sits near the target end (d high) — they bear the cost of constitutional dilution without receiving compensating benefit; their identity-locked exit means they cannot abandon the commitment without abandoning their constitutive identity. Article 9 litigants are strongly targeted (d high, trapped) — they bear the cost of denied standing with no exit. The cabinet sits at the agenda-setter position — they define the standard and are not meaningfully extracted from.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is whether the 'minimum necessary' founding problem is still live. The founding problem — reconciling renunciation with Cold War security — remains live in modified form (regional threats persist), but the threshold's calibration has drifted from the founding problem's demands toward the defense establishment's institutional preferences. The classification as tangled_rope prevents mislabeling this as pure coordination (which would ignore the extraction flowing to the defense establishment under the elastic standard) or pure extraction (which would ignore the genuine coordination function of stabilizing the domestic settlement between pacifist and security constituencies). The founding problem is live but the threshold has outgrown its original calibration — the gap between founding problem status (live) and the threshold's current operation (increasingly elastic, self-assessed) is the mandatrophy signal. The constraint has not resolved into mandatrophy because the threshold still does real limiting work (no nuclear weapons, no aircraft carriers, no conscription), but the trajectory points toward increasing theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_elasticity,
    'Is the ''minimum necessary'' standard genuinely limiting force structure, or has it become fully elastic — justifying whatever the government proposes rather than constraining it?',
    'Compare the government''s ''minimum necessary'' assessments against independent security analyses (think-tank force-structure assessments, allied comparisons) across the interval. If the government''s threshold consistently exceeds independent assessments of defensive need, the standard is elastic.',
    'If fully elastic, the theater_ratio should be revised upward and the constraint drifts toward piton — a threshold maintained theatrically while no longer binding force structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_elasticity, empirical, 'Whether the proportionality threshold genuinely limits or merely justifies').

omega_variable(
    self_assessment_legitimacy,
    'Is the government''s self-assessment of ''minimum necessary'' structurally legitimate (democratic accountability through elections and Diet oversight) or a structural conflict of interest (the assessor benefits from expansion)?',
    'Track whether the Diet has ever forced a reduction in the government''s ''minimum necessary'' assessment across the interval. If never, the self-assessment lacks a functioning check.',
    'If the self-assessment lacks a functioning check, the threshold''s extractiveness is understated — the standard is set by the party that benefits from its expansion, and effective extraction exceeds the measured base rate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_assessment_legitimacy, empirical, 'Whether self-assessment of the threshold has a functioning external check').

omega_variable(
    reading_displacement_risk,
    'Will the inherent-right reading persist as the operative interpretation, or will it be displaced by the collective-self-defense reading (expansion) or revived by strict pacifism (contraction)?',
    'Track constitutional revision politics, cabinet interpretation changes, and court decisions over the coming decade. Displacement signals: formal constitutional amendment, a cabinet reinterpretation adopting collective self-defense, or a court decision adopting strict pacifism.',
    'Displacement by collective self-defense would increase extractiveness (removing the territorial-defense cap); displacement by strict pacifism would decrease it (removing the defense establishment''s legitimacy). Either displacement would restructure the stakeholder surface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_displacement_risk, empirical, 'Whether this reading persists or is displaced by a sibling reading').

omega_variable(
    sdf_identity_lock_stability,
    'The Self-Defense Forces'' institutional identity is constituted by their constitutional position as ''Self-Defense'' forces. If the identity frame broke (renaming, reconstitution as a conventional military), would the threshold''s enforcement mechanism change?',
    'Track public opinion and institutional culture in response to proposals to rename or reconstitute the forces. If the ''Self-Defense'' designation is dropped without constitutional crisis, the identity lock is weaker than modeled.',
    'If the identity frame breaks, the threshold loses its anchoring in the pacifist national identity — the constraint becomes a purely political limit rather than a constitutional one, and its enforcement mechanism shifts from identity maintenance to explicit political enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sdf_identity_lock_stability, conceptual, 'Whether the SDF''s constitutional identity frame sustains the threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__inherent_right_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__inherent_right_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__inherent_right_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(arti_tr_t40, article_9_war_renunciation__inherent_right_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(arti_tr_t50, article_9_war_renunciation__inherent_right_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__inherent_right_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(arti_tr_t75, article_9_war_renunciation__inherent_right_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(arti_be_t40, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(arti_be_t50, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(arti_be_t75, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 75, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(arti_su_t40, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(arti_su_t50, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 50, 0.34).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement(arti_su_t75, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 75, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japan_us_security_alliance).

% DUAL FORMULATION NOTE:
% The Article 9 war-renunciation kernel decomposes into three constraint stories per the ε-invariance principle: strict_pacifist_reading (categorical prohibition, high ε for the defense establishment whose existence it denies), this inherent_right_reading (proportionality threshold, moderate ε), and collective_self_defense_reading (extension beyond territorial defense, higher ε for regional states and pacifist constituency). The inherent-right reading is the historically operative one and structurally influences the collective-self-defense reading (its precedent that defense is constitutionally permissible makes expansion easier to argue) while foreclosing the strict-pacifist reading (its core premise that minimum defense is permissible directly contradicts the categorical prohibition). The three stories share the constitutional text as kernel but have different ε values, different victim sets, and different enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
