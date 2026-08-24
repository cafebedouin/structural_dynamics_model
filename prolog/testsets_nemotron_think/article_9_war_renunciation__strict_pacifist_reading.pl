% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading — Categorical War Renunciation
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   The strict pacifist reading of Article 9 treats the constitutional text
 *   'never be maintained' as a categorical prohibition on any armed forces,
 *   including defensive ones. This reading claims the status of a Mountain —
 *   an absolute, unrevisable textual command akin to natural law within the
 *   constitutional order. However, the standing arrangement under contest
 *   (the actual SDF, collective self-defense reinterpretation, and security
 *   legislation) extracts heavily from state security autonomy. The reading's
 *   adherents (pacifist advocates, hibakusha, constitutional purists) benefit
 *   from the constraint's moral and legal absoluteness, while the state, SDF
 *   personnel, and alliance partners bear the costs. The constraint requires
 *   active enforcement through judicial avoidance, political
 *   reinterpretation, and social pressure to maintain the gap between text
 *   and practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.78).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.72).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, mountain).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading — Categorical War Renunciation").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).
domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '8e877d05-e084-4ea3-9dc5-6961a9847696').
narrative_ontology:cs_kernel_codification('8e877d05-e084-4ea3-9dc5-6961a9847696', fixed_text).
narrative_ontology:cs_authority_grounding('8e877d05-e084-4ea3-9dc5-6961a9847696', lineage).
narrative_ontology:cs_interpretation_layer_present('8e877d05-e084-4ea3-9dc5-6961a9847696').
narrative_ontology:cs_reading_relation('8e877d05-e084-4ea3-9dc5-6961a9847696', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('8e877d05-e084-4ea3-9dc5-6961a9847696', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('8e877d05-e084-4ea3-9dc5-6961a9847696', foundational, war_renunciation_absolute_prohibition).
narrative_ontology:cs_axiom_status(war_renunciation_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('8e877d05-e084-4ea3-9dc5-6961a9847696', war_renunciation_absolute_prohibition, deontological).
narrative_ontology:cs_axiom('8e877d05-e084-4ea3-9dc5-6961a9847696', foundational, never_be_maintained_categorical).
narrative_ontology:cs_axiom_status(never_be_maintained_categorical, holdable).
narrative_ontology:cs_axiom_grounding('8e877d05-e084-4ea3-9dc5-6961a9847696', never_be_maintained_categorical, deontological).
narrative_ontology:cs_reference_frame('8e877d05-e084-4ea3-9dc5-6961a9847696', article_9_textual_command).
narrative_ontology:cs_drift_state('8e877d05-e084-4ea3-9dc5-6961a9847696', contemporary_security_legislation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8e877d05-e084-4ea3-9dc5-6961a9847696', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, neighboring_states_security).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, hibakusha_communities).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, constitutional_purists).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, sdf_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, alliance_partners_expecting_collective_defense).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, government_executive).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, war_renunciation_as_constitutional_absolute).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, textual_command_precludes_reinterpretation).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, pacifism_as_constitutional_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Japanese state's capacity to independently determine its security posture is constrained by the absolute textual prohibition. Security decisions require constitutional amendment (Article 96 supermajority) or creative reinterpretation that the strict pacifist reading rejects as illegitimate. The state bears the cost of forgoing military options that other sovereign states consider routine.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy, payer,
    institutional, generational, constrained, national).

% Peace movement organizations, constitutional scholars, and civil society groups that treat Article 9 as a moral and legal absolute. They benefit from the constraint's existence as a rallying point, a source of institutional legitimacy, and a barrier against remilitarization. Their exit is mobile — they could advocate other causes — but their identity is fused with this constraint.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_advocates, beneficiary,
    organized, generational, mobile, national).

% East Asian states (particularly China, Korea) that benefit strategically from Japan's constitutional demilitarization. They gain predictable non-threat posture and leverage in regional diplomacy. Their exit is arbitrage-grade — they can shift alliance structures — but they actively lobby to maintain the constraint.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, neighboring_states_security, beneficiary,
    institutional, generational, arbitrage, regional).

% Atomic bombing survivors and descendants whose moral authority anchors the pacifist reading. They are identity-locked — their self-concept and public role are constituted through the constraint. They are also excluded from formal constitutional interpretation despite being the constraint's living moral referent.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, hibakusha_communities, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, hibakusha_communities, excluded).

% Scholars and jurists who insist the text 'never be maintained' admits no exception. Their professional identity and interpretive framework depend on the constraint's absoluteness. They are identity-locked — abandoning the reading dissolves their intellectual project.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_purists, beneficiary,
    moderate, biographical, identity_locked, national).

% Self-Defense Forces members who serve in an institution the strict pacifist reading deems unconstitutional. They bear professional risk (legal status ambiguity), institutional stigma, and operational constraints (no collective self-defense, limited equipment). Exit is constrained — leaving means career loss; staying means serving in a constitutionally contested role.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, sdf_personnel, payer,
    organized, biographical, constrained, national).

% Primarily the United States, which bears disproportionate burden of Japan's defense under the alliance while Japan's constitutional constraint limits burden-sharing. They could adjust alliance terms (mobile exit) but the alliance structure creates high switching costs.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, alliance_partners_expecting_collective_defense, payer,
    institutional, generational, mobile, global).

% The Cabinet and LDP leadership that have driven reinterpretation (2014 collective self-defense, 2015 security legislation). They set the practical agenda but pay political costs: domestic opposition, coalition friction, constitutional legitimacy questions. They are constrained — they cannot openly amend Article 9 without supermajority, so they reinterpret.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, government_executive, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, government_executive, payer).

% The Supreme Court of Japan, which has avoided ruling on SDF constitutionality (political question doctrine) but whose silence enables the status quo. It sets the interpretive agenda by declining to enforce the strict reading. Its exit is analytical — it observes but does not bear the constraint's costs directly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Academics and policy analysts who argue Article 9 permits minimum necessary self-defense capacity. They are excluded from the strict pacifist reading's framework — their interpretation is treated as illegitimate by the constraint's adherents. They would object to the absolute prohibition but have no seat in the pacifist reading's constitutive structure.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, realist_security_scholars, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Renunciation of war as national policy creates a credible commitment to pacifism that coordinates regional trust, domestic identity, and international moral standing — solving the post-WWII problem of how a defeated aggressor state credibly signals permanent non-aggression.
% TRANSFER_FUNCTION: The constraint transfers security autonomy from the Japanese state (which forgoes independent military capacity) to the pacifist moral order and the U.S. alliance (which provides extended deterrence). The cost is borne by state security autonomy and SDF personnel; the benefit accrues to pacifist advocates, neighboring states, and the alliance structure that relies on Japan's constrained posture.
% ABSENT_VOICES: Realist security scholars, military planners, and alliance partners who argue for collective self-defense are structurally excluded from the strict pacifist reading's constitutive framework. They exist in Japanese policy discourse but are treated as illegitimate interpreters by the constraint's adherents. Their exclusion is maintained by the reading's textual absolutism — 'never be maintained' admits no balancing test.
% DISAPPEARANCE_RATIONALE: If the strict pacifist reading vanished overnight (i.e., the absolute prohibition was officially abandoned), Japan would rapidly normalize its military posture: SDF would become a conventional military, collective self-defense would be fully operationalized, constitutional amendment would become urgent, regional threat perceptions would shift, and the U.S.-Japan alliance would restructure around burden-sharing. The pacifist identity infrastructure (peace movement, hibakusha moral authority, constitutional purist scholarship) would lose its central referent.
% FOUNDING_PROBLEM: Post-WWII occupation imperative to demilitarize Japan and embed pacifism in its supreme law so it could never again wage aggressive war — the 'never be maintained' language was designed as an absolute, unrevisable barrier against remilitarization.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is corroborated as 'contested' by: (1) Peace movement and hibakusha groups (beneficiaries) attest it is LIVE — nuclear threat and regional tensions make war renunciation more necessary than ever. (2) LDP governments, SDF, U.S. alliance managers (non-beneficiaries) attest it is DEAD — the security environment (North Korea, China, Russia) requires capabilities the absolute prohibition forbids. (3) Constitutional scholars outside the pacifist tradition (e.g., Ashibe, Sato) attest the text was understood in 1947 as permitting self-defense; the absolute reading is a later construction. No single corroborating source outside the beneficiary set endorses 'live' unambiguously.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the standing arrangement forces the state to either violate its own constitution (by maintaining SDF) or forgo security capabilities other sovereign states consider essential. Suppression (0.72) is high because the constraint's persistence depends on suppressing the inherent_right and collective_self_defense readings — through political question doctrine, reinterpretation rather than amendment, and social stigma against 'revisionism.' Theater ratio (0.38) reflects that the SDF's actual defense function is real but increasingly draped in pacifist rhetoric. Accessibility collapse (0.88) is very high — the strict reading treats alternatives (reinterpretation, amendment) as conceptually impermissible, not merely difficult. Resistance (0.35) is moderate — the constraint meets organized political resistance (LDP, security establishment) but the strict reading's adherents treat this resistance as illegitimate, not as evidence the constraint is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the strict pacifist seat, the constraint is a Mountain — the text is absolute, alternatives are conceptually excluded, the SDF's existence is a constitutional violation. From the state security autonomy seat, the same constraint operates as a Snare — it extracts security capacity while suppressing the inherent_right reading that would legitimize minimum necessary defense. From the SDF personnel seat, it is a Tangled Rope — they coordinate national defense (real function) but under constitutional illegitimacy (extraction). The engine computes this divergence from the structural data; the strict pacifist reading's claimed_type (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State security autonomy is the primary victim (payer) — it bears the full cost of forgoing independent military capacity, with exit constrained by Article 96's supermajority requirement. Pacifist advocates and hibakusha are primary beneficiaries — they collect moral authority, institutional legitimacy, and political mobilization from the constraint's absoluteness, with identity-locked exit. Neighboring states are secondary beneficiaries with arbitrage-grade exit. SDF personnel are payers with constrained exit (career investment, legal ambiguity). The government executive is an agenda_setter that also pays political costs. The constitutional court is an agenda_setter with analytical exit. Realist scholars are excluded — their interpretive framework is ruled out by the reading's textual absolutism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permanent demilitarization of a former aggressor) is contested as live or dead. The strict pacifist reading insists it is live and the constraint remains necessary. The government and alliance partners treat it as dead and the constraint as obstructing necessary adaptation. This mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags mandatrophy: the constraint persists beyond its original justification but its adherents block adaptation. The constraint is not a Piton (theater without function) — the coordination function (credible pacifist commitment) is still valued by beneficiaries — but the extraction from state security autonomy has accumulated as the security environment changed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the strict pacifist reading''s claim to categorical absoluteness a genuine constitutional mountain (text as immutable natural law within the order) or a constructed constraint that benefits identifiable pacifist advocates and neighboring states?',
    'Trace the historical emergence of the absolute reading: was ''never be maintained'' understood in 1947 as prohibiting all self-defense forces, or did the absolute interpretation develop later as a political project? Compare GHQ draft intent, Japanese government interpretation at promulgation, and the 1954 SDF creation debate.',
    'If the absolute reading is a later construction (not original meaning), the constraint is a false summit mountain — FSM triggers reclassification to tangled_rope. If it reflects original intent, the mountain claim holds but beneficiaries (pacifist_advocates, neighboring_states) still extract from state_security_autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Whether the strict pacifist reading''s mountain claim reflects original constitutional design or later political construction.').

omega_variable(
    security_autonomy_extraction_measurement,
    'How much security autonomy does the strict reading actually extract from the Japanese state, given that the U.S. alliance provides extended deterrence?',
    'Counterfactual analysis: what security options would Japan have without the strict reading''s constraint? Compare Japan''s actual defense posture (SDF + alliance) to counterfactual postures of similarly situated states (Germany, South Korea) and to Japan''s own pre-1945 capacity.',
    'If extended deterrence substantially substitutes for independent capacity, extractiveness is lower than 0.78 — the constraint coordinates alliance dependence rather than extracting pure security loss. If substitution is incomplete (e.g., gray-zone scenarios, alliance credibility gaps), extractiveness is correctly high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_autonomy_extraction_measurement, empirical, 'Whether alliance dependence mitigates the strict reading''s extraction from state security autonomy.').

omega_variable(
    reading_relations_foreclosure_certainty,
    'Does the strict pacifist reading genuinely foreclose the inherent_right_reading and collective_self_defense_reading within a single framework, or do they coexist as competing interpretations in Japanese constitutional practice?',
    'Examine whether any coherent legal framework (court decision, party platform, government policy) has ever simultaneously upheld the strict reading''s absolute prohibition AND the siblings'' permitted defensive capacity. The 2014 Cabinet Legislation Bureau opinion and 2015 security legislation explicitly reject the strict reading — this suggests foreclosure in practice.',
    'If foreclosure is real, the kernel has genuine forecloses edges and the strict reading''s axioms are holdable. If coexistence is the actual structure (different actors hold different readings simultaneously), the relation should be coexists_with and the kernel is distributed rather than extraction-grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_foreclosure_certainty, conceptual, 'Whether the strict reading''s foreclosure of sibling readings is logical or merely political.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings structural (political question doctrine, Article 96 supermajority, LDP dominance) or internalized (pacifist identity makes reinterpretation unthinkable for adherents)?',
    'Post-exit suppression trajectory: if pacifist advocates who accept reinterpretation (rare) face social/professional ostracism, suppression is internalized. If the constraint would collapse without active political enforcement (judicial avoidance, media framing), suppression is structural.',
    'If internalized, effective suppression is higher than measured — the constraint''s adherents carry the suppression with them. If structural, suppression is vulnerable to political realignment (e.g., LDP losing power, Court ruling on SDF).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the strict pacifist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a9wrp_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(a9wrp_tr_t1954, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(a9wrp_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(a9wrp_tr_t1981, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1981, 0.28).
narrative_ontology:measurement(a9wrp_tr_t1991, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1991, 0.31).
narrative_ontology:measurement(a9wrp_tr_t2001, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2001, 0.34).
narrative_ontology:measurement(a9wrp_tr_t2014, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(a9wrp_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(a9wrp_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(a9wrp_be_t1954, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1954, 0.45).
narrative_ontology:measurement(a9wrp_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(a9wrp_be_t1981, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1981, 0.58).
narrative_ontology:measurement(a9wrp_be_t1991, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1991, 0.62).
narrative_ontology:measurement(a9wrp_be_t2001, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(a9wrp_be_t2014, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2014, 0.73).
narrative_ontology:measurement(a9wrp_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(a9wrp_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.25).
narrative_ontology:measurement(a9wrp_su_t1954, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1954, 0.55).
narrative_ontology:measurement(a9wrp_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(a9wrp_su_t1981, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1981, 0.65).
narrative_ontology:measurement(a9wrp_su_t1991, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1991, 0.68).
narrative_ontology:measurement(a9wrp_su_t2001, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(a9wrp_su_t2014, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2014, 0.71).
narrative_ontology:measurement(a9wrp_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.08).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, us_japan_security_treaty).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, japan_sdf_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the article_9_war_renunciation kernel. The strict_pacifist_reading claims mountain status with absolute textual prohibition. The inherent_right_reading claims rope/tangled_rope with permitted minimum self-defense. The collective_self_defense_reading claims scaffold/tangled_rope with alliance-conditioned military action. Their ε values differ substantially: strict_pacifist ε=0.78 (assesses standing arrangement as extractive), inherent_right ε≈0.35 (assesses same arrangement as moderate coordination), collective_self_defense ε≈0.45 (assesses arrangement as transitional coordination). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__strict_pacifist_reading, institutional, 0.85).
constraint_indexing:directionality_override(article_9_war_renunciation__strict_pacifist_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
