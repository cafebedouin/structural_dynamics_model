% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Reading
 *   domain: constitutional/security
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution states: 'Aspiring sincerely to an
 *   international peace based on justice and order, the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as means of settling international disputes. In order to
 *   accomplish the aim of the preceding paragraph, land, sea, and air forces,
 *   as well as other war potential, will never be maintained.' The
 *   strict-pacifist reading interprets 'never' as categorical. The
 *   inherent-right reading permits minimum necessary self-defense. THIS
 *   READING—the collective-self-defense reading—holds that Article 9 permits
 *   military action to defend allies and protect regional interests when
 *   Japan's 'survival' is threatened, even without direct attack on Japan.
 *   This reading has become the operative constitutional interpretation
 *   despite its textual tension with renunciation language. The constraint
 *   story models the structure of this reading's operation: how it functions
 *   as real coordination (enabling alliance participation) overlaid with
 *   asymmetric extraction (transferring interpretive authority from pacifist
 *   constitutionalists to defense-policy makers, expanding military resource
 *   claims).
 *
 * KEY AGENTS:
 *   - Executive branch: interprets Article 9, authorizes collective-defense operations, benefits from discretionary authority
 *   - Alliance partners: benefit from Japan's military participation without formal constitutional amendment
 *   - Military-industrial capacity: benefits from defense expansion justified by collective-defense rationale
 *   - Strict pacifist interpreters: bear cost of interpretive displacement and constitutional override
 *   - Peace-movement constituencies: bear cost of marginalized reading and constrained political leverage
 *   - Regional populations: bear escalation risk from Japan's expanded operational scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.62).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.71).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional/security").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'aa734ae9-c6d6-49ad-a425-c6a00f3a95f5').
narrative_ontology:cs_kernel_codification('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', fixed_text).
narrative_ontology:cs_authority_grounding('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', extraction).
narrative_ontology:cs_interpretation_layer_present('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5').
narrative_ontology:cs_reading_relation('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', foundational, collective_survival_encompasses_alliance_defense).
narrative_ontology:cs_axiom_status(collective_survival_encompasses_alliance_defense, holdable).
narrative_ontology:cs_axiom_grounding('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', collective_survival_encompasses_alliance_defense, empirically_contingent).
narrative_ontology:cs_axiom('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', secondary, executive_threat_assessment_authoritative).
narrative_ontology:cs_axiom_status(executive_threat_assessment_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', executive_threat_assessment_authoritative, conventional).
narrative_ontology:cs_reference_frame('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', constitutional_alliance_participation).
narrative_ontology:cs_drift_state('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', contemporary_indo_pacific_great_power_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa734ae9-c6d6-49ad-a425-c6a00f3a95f5', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, executive_branch_defense_policy_makers).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, alliance_security_partners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, military_industrial_capacity_maintainers).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_constitutional_interpreters).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, peace_movement_constituencies).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regions_exposed_to_collective_defense_escalation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Cabinet and defense bureaucracy interpret Article 9's meaning, adjust legal opinions on what military action is permissible, and authorize overseas deployments. They benefit by retaining discretion to execute alliance commitments and respond to regional threats without explicit constitutional amendment. They actively defend this reading against strict pacifist challenges through legal reinterpretation and government-endorsed scholarship.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, executive_branch_defense_policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Japan's willingness to conduct collective military operations and forward-deploy forces depends on this reading. Alliance partners (US, South Korea, Australia) benefit from Japan's capacity to participate in regional defense architecture and joint operations. Their security architecture assumes Japan can move beyond minimum self-defense.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, alliance_security_partners, beneficiary,
    powerful, generational, constrained, regional).

% Defense contractors, military planners, and Self-Defense Force personnel benefit from a reading that justifies sustained military investment, modernization, and overseas operational capability. Collective self-defense provides a rationale for capital-intensive platforms (destroyers, transport aircraft) that narrow self-defense readings would leave underutilized.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, military_industrial_capacity_maintainers, beneficiary,
    powerful, biographical, mobile, national).

% Constitutional scholars, judges, and civil society organizations committed to the strict-pacifist reading bear the cost of interpretive displacement: their reading is treated as legally obsolete, their constitutional arguments are overridden in policy, and their capacity to contest military expansion through law is eroded. Exit (abandoning pacifist constitutionalism) would require abandoning a core identity commitment to a peace-centered reading of the nation's founding constitutional promise.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_constitutional_interpreters, payer,
    moderate, biographical, identity_locked, national).

% Broad-based peace movements and anti-militarism constituencies bear the cost of watching their preferred constitutional reading marginalized in practice. They lack the institutional power to impose their reading; their political leverage depends on maintaining public opinion alignment, which is being actively worked against through government messaging and reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, peace_movement_constituencies, payer,
    organized, biographical, constrained, national).

% Taiwan, Korea, and other Indo-Pacific regions affected by potential Japanese military involvement in regional conflicts bear escalation risk from Japan's expanded operational capacity. They cannot veto the reading or opt out of the security dynamics it enables. Their exposure is structural: Japan's willingness to conduct collective operations affects regional military balance and conflict escalation calculus.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regions_exposed_to_collective_defense_escalation, payer,
    powerless, immediate, trapped, regional).

% The Supreme Court is the formal arbiter of constitutional meaning but has historically deferred to executive interpretation on national security grounds. It observes disputes between executive reading and pacifist reading but rarely intervenes with hard constraints on what 'Article 9 permits.'
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% The San Francisco Peace Treaty and post-WWII international norms form the historical context; this reading reinterprets what the original peaceful-nation commitment entails. International observers note the reading as a progressive reinterpretation of restraint, while pacifist interpreters read it as a betrayal of founding commitments.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, international_peace_treaty_framework, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_9_war_renunciation__collective_self_defense_reading, international_peace_treaty_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, executive_branch_defense_policy_makers).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Japan's integration into regional collective-security architecture by reconciling post-war pacifist commitments with contemporary alliance obligations. Solves the coordination problem: how can a nation with constitutional war renunciation participate credibly in multi-party defense arrangements without constitutional amendment?
% TRANSFER_FUNCTION: Moves interpretive authority from the constitutional text (strict language: 'never be maintained') to the executive branch's evolving legal opinions on what 'survival threat' and 'collective self-defense' mean. Transfers from strict-pacifist constitutionalists to defense-policy makers the power to define what is permitted. Transfers from pacifist civil society to military-industrial sectors the resources and political space for defense expansion.
% ABSENT_VOICES: Strict pacifist constitutional scholars are represented but systematically overruled; international peace-treaty signatories (some) would argue the original commitment was absolute, not elastic. Taiwan and Korea are affected by the reading's security implications but not seated at reinterpretation tables. Future generations who might benefit from a strictly restrained military capability are voiceless in present-day interpretation decisions.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the strict-pacifist or narrow-inherent-right readings were restored as governing law, Japan would withdraw from forward-deployed regional operations, allied security architecture would reorganize to reduce Japan-dependent capabilities, and the defense budget would compress to minimum self-defense platforms. The regional military balance would shift; alliance burden-sharing would redistribute.
% FOUNDING_PROBLEM: After 1945, Japan renounced 'war' under international pressure and constitutional constraint. By the Cold War era, Japan needed a credible defense posture and alliance role without violating its pacifist foundation. The problem: how to honor the letter of renunciation while meeting geopolitical security needs?
% FOUNDING_PROBLEM_CORROBORATION: Executive branch, defense planners, and alliance partners attest the founding problem remains live — regional threats (China, North Korea, Russia) require Japan's integrated security participation. Strict pacifists and constitutional scholars attest the founding problem is a policy choice, not a structural necessity, and that the 'threat' framing is leveraged to justify expansions beyond genuine survival defense. International security analysts outside Japan acknowledge both positions as coherent.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the 35-year interval (0.35 → 0.62) because the reading's operation increasingly enables unilateral executive expansion of military scope under the elastic concept of 'survival threat.' Theater ratio also rises (0.28 → 0.58) because justificatory framing ('regional stability,' 'alliance necessity') grows as a share of enforcement activity—the reading's meaning must be continuously performed and reaffirmed through government messaging and reinterpretation, not just invoked once. Suppression requirement rises (0.48 → 0.71) because the reading can only persist if strict-pacifist challenges are continuously suppressed through legal reinterpretation, institutional override, and erosion of constitutional constraint on executive authority. Accessibility_collapse is moderate (0.48) because alternatives—strict pacifism, narrow self-defense—remain intellectually alive and continue to be advocated by constitutional scholars, even though they are overridden in policy. Resistance is high (0.69) because the reading meets sustained and organized opposition from peace movements, some judicial voices, and international observers.
 *
 * PERSPECTIVAL GAP:
 *   From the executive-branch and alliance seats, this reading solves a genuine coordination problem—how to integrate Japan's defense capacity into regional security without formal constitutional change. The reading is read as practical necessity and alliance loyalty. From the strict-pacifist and peace-movement seats, the same structure operates as executive overreach and constitutional erosion: the reading rewrites the founding commitment through interpretive drift, not amendment, and does so systematically to expand military scope under elastic threat criteria. From the regional seat (Taiwan, Korea), the reading has mixed character: it provides security guarantees but also raises escalation risk. The engine computes these divergent classifications from the structural data; the authored claim (tangled_rope) reflects that genuine coordination function AND asymmetric extraction coexist in one structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the structural beneficiary (d ≈ 0.15): it collects discretionary authority to define 'survival threat' and deploy forces without explicit amendment. Alliance partners benefit indirectly (d ≈ 0.25): they gain operational capacity from Japan but bear no direct extraction. Military-industrial sectors benefit substantially (d ≈ 0.10): they capture sustained resource allocation. Strict-pacifist interpreters and peace constituencies are the targets (d ≈ 0.85): they bear the cost of interpretive displacement and have constrained exit—their core identity is wrapped in the pacifist reading, making exit (abandonment of pacifist constitutionalism) identity-locked. Regional populations exposed to escalation (d ≈ 0.92) are the most trapped: they bear escalation risk and have no voice in the reading's adoption. The Supreme Court sits at the observer seat (d ≈ 0.50, analytical): it has the authority to constrain but historically defers on security grounds.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: does this reading solve the founding problem or does it solve a different problem (executive expansion) while covering it in the language of the founding problem? The founding problem was how to maintain pacifism under Cold War security pressures. This reading solves that by redefining pacifism to permit collective military operations—not by abolishing the commitment but by making it elastic. However, the escalation in extractiveness (0.35 → 0.62) and theater ratio (0.28 → 0.58) suggest the reading has drifted from its founding justification. Early in the interval, it genuinely coordinated alliance participation within pacifist framing. By the end, it is increasingly used to justify expansions (weapons systems, overseas bases, military budget growth) that exceed coordination requirements. The rising theater_ratio signals this: an increasing share of the reading's maintenance work is justificatory performance rather than actual coordination. Mandatrophy is NOT declared because the founding problem remains live—regional threats are real, and alliance participation is still necessary. But the reading has accumulated extraction beyond what the founding problem requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threat_elasticity,
    'What counts as a ''survival threat'' that justifies collective military action? Is the standard narrow (direct existential peril) or elastic (regional power shifts, economic disruption, alliance credibility)?',
    'Analyze government invocations of survival-threat language over time; examine legal opinions on which scenarios have been approved as meeting the threshold. Compare against comparable democracies'' definitions of national interest justifying military action.',
    'A narrow standard would constrain collective action to rare, high-bar scenarios. An elastic standard permits incremental expansion—each new operation redefines what counts as survival threat, making the reading progressively more extractive. Rising theater_ratio suggests elasticity is increasing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_threat_elasticity, conceptual, 'Whether ''survival threat'' remains a genuine constraint or has become elastic cover for executive discretion.').

omega_variable(
    constitutional_amendment_vs_interpretation,
    'Is redefining Article 9''s scope through executive reinterpretation a legitimate constitutional evolution, or is it a functional amendment that evades amendment procedure?',
    'Constitutional scholarship comparing Japanese practice to other democracies'' treatment of constitutional reinterpretation; examination of whether the magnitude of operational change (pre-collective-defense vs. post-) would trigger amendment requirements if done by textual revision.',
    'If reinterpretation is legitimate, the reading is an ordinary constraint; if it evades amendment procedure, it becomes a false institutional legitimacy (citizens believe the structure they think they have is operative, but executive practice has rewritten the rules). This affects whether suppression is structural (external barriers) or internalized (false consciousness about what the constitution permits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_vs_interpretation, conceptual, 'Whether the reading represents constitutional flexibility or procedural circumvention.').

omega_variable(
    identity_lock_stability_of_pacifist_interpreters,
    'For strict-pacifist constitutional interpreters, is the identity_locked exit designation stable? Could pacifist scholars and movement members exit without identity dissolution?',
    'Post-suppression trajectory analysis: if pacifist interpreters were given institutional parity (e.g., court seats, legal authority to enforce strict reading), would they maintain their constitutionalist identity, or would identity have already fused irreversibly with marginalization?',
    'If identity_locked is stable, suppression of the pacifist reading carries durable human costs; the targets cannot simply move to a new policy preference without experiencing identity fracture. If it is already compromised (pacifist identity has fused with powerlessness), the constraint''s human damage is already internalized, not reversible by institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_stability_of_pacifist_interpreters, empirical, 'Whether pacifist constitutional interpreters retain viable exit or are irreversibly identity-locked into the constraint.').

omega_variable(
    alliance_necessity_vs_reading_choice,
    'Is Japan''s alliance participation genuinely necessary to the country''s security (a live founding problem), or has the reading become primarily a vehicle for defense-industry expansion and great-power competition participation (a dead founding problem)?',
    'Counterfactual analysis: if Japan adopted a strict-pacifist or narrow-self-defense reading, would regional threats become unmanageable, or would Japanese society reorganize around lower-intensity security arrangements (diplomacy, defensive technology, non-military regional cooperation)?',
    'If necessity is live, the reading solves a genuine coordination problem and the extractiveness is the price of alliance integration. If necessity is dead (the security problem has shifted to one that collective military action cannot solve), the reading is zombified—it persists as cover for military-industrial interests and great-power alignment, not because regional security requires it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_necessity_vs_reading_choice, conceptual, 'Whether the reading''s founding problem remains the actual problem driving its operation.').

omega_variable(
    kernel_contest_structural_relation,
    'Which sibling reading—strict pacifist or inherent right—would the collective-self-defense reading foreclose if fully adopted as governing law? Is there a framework that holds all three simultaneously?',
    'Comparative institutional analysis: examine whether Japanese constitutional law has settled on one reading as dominant, or whether all three remain live (held by different actors/courts/eras). Assess whether simultaneous adoption is logically possible or structurally foreclosed.',
    'If collective-self-defense forecloses strict pacifism, the readings are truly competitive; if they coexist (different actors/courts hold different readings without forcing resolution), the kernel remains contested and no single reading has institutional authority. This affects whether the constraint''s persistence depends on suppression (forcing one reading) or on institutional fragmentation (multiple readings coexist, each authoritative in its domain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_structural_relation, conceptual, 'Whether readings are logically competitive or institutionally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t7, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 7, 0.36).
narrative_ontology:measurement(arti_tr_t14, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 14, 0.44).
narrative_ontology:measurement(arti_tr_t21, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 21, 0.51).
narrative_ontology:measurement(arti_tr_t28, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 28, 0.55).
narrative_ontology:measurement(arti_tr_t35, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arti_be_t7, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(arti_be_t14, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement(arti_be_t21, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 21, 0.57).
narrative_ontology:measurement(arti_be_t28, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(arti_be_t35, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(arti_su_t7, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 7, 0.55).
narrative_ontology:measurement(arti_su_t14, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement(arti_su_t21, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 21, 0.67).
narrative_ontology:measurement(arti_su_t28, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 28, 0.7).
narrative_ontology:measurement(arti_su_t35, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).

% DUAL FORMULATION NOTE:
% Article 9 war-renunciation kernel decomposes into three structurally distinct constraint stories, each modeling a different interpretation of the constitutional text. The strict_pacifist_reading models the most restrictive interpretation (categorical prohibition, no armed forces). The inherent_right_reading models the middle interpretation (sovereignty-based minimum self-defense, no collective action). This collective_self_defense_reading models the elastic interpretation (survival-threat-triggered collective operations). Each reading has its own ε (extractiveness from the standpoint of that reading), its own beneficiary/victim structure, and its own type. The three readings are linked by network.affects_constraints to show that each reading structurally influences the others: the collective-self-defense reading creates pressure on strict-pacifist readings by showing them as institutional artifacts that can be overridden; the inherent-right reading influences both by occupying the middle ground; the strict-pacifist reading constrains both by remaining as an institutional memory and legal argument. No single reading dominates logically—they coexist through institutional fragmentation (different courts, eras, and actors hold different readings) and active suppression (executive and military actors override pacifist readings while maintaining formal commitment to Article 9).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
