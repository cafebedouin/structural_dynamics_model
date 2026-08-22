% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading: Categorical Military Prohibition
 *   domain: constitutional law / security policy / institutional legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution renounces war and prohibits the
 *   maintenance of 'armed forces' (bushitsu to gunryoku). The strict pacifist
 *   reading interprets 'never be maintained' as an absolute categorical
 *   prohibition: NO military organization of any kind is permissible,
 *   including defensive forces. This reading stands in direct logical tension
 *   with the inherent-right reading (which permits minimum self-defense
 *   capacity) and the collective-defense reading (which permits military
 *   action under alliance commitment). The strict reading classifies the
 *   empirically present Self-Defense Forces as a constitutional
 *   violation—they exist but are deemed illegitimate under this
 *   interpretation. Japan's state security autonomy is the primary victim: it
 *   is foreclosed to independent military self-defense and locked into
 *   absolute dependence on the US security guarantee. The constraint persists
 *   through active enforcement: pacifist constituencies and
 *   constitutional-amendment blockers prevent the reinterpretation or
 *   amendment that would permit military flexibility. Extraction accumulates
 *   over time as regional security threats evolve (rise of China, North Korea
 *   nuclear capability) and the cost of military constraint increases
 *   relative to the initial postwar security logic.
 *
 * KEY AGENTS:
 *   - Pacifist constitutional interpreters: institutional agenda-setters who defend the strict reading
 *   - Japanese state security autonomy: the structural victim—military self-defense capacity foreclosed
 *   - Military institution personnel: moderate-power agents bearing cost of constitutional illegitimacy
 *   - Pacifism-movement supporters: beneficiaries who gain doctrine vindication
 *   - US security guarantor: institutional beneficiary gaining Japan's structural dependence
 *   - Constitutional amendment blockers: organized agenda-setters enforcing the constraint
 *   - Courts: observer seat whose interpretations determine constraint binding
 *   - Defense planners (excluded): strategists locked out of the legitimacy calculation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.78).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.72).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, snare).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading: Categorical Military Prohibition").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional law / security policy / institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '2450694a-75e7-4ba0-9f10-8f2051070281').
narrative_ontology:cs_kernel_codification('2450694a-75e7-4ba0-9f10-8f2051070281', fixed_text).
narrative_ontology:cs_authority_grounding('2450694a-75e7-4ba0-9f10-8f2051070281', lineage).
narrative_ontology:cs_interpretation_layer_present('2450694a-75e7-4ba0-9f10-8f2051070281').
narrative_ontology:cs_reading_relation('2450694a-75e7-4ba0-9f10-8f2051070281', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2450694a-75e7-4ba0-9f10-8f2051070281', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('2450694a-75e7-4ba0-9f10-8f2051070281', foundational, military_force_categorically_impermissible).
narrative_ontology:cs_axiom_status(military_force_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('2450694a-75e7-4ba0-9f10-8f2051070281', military_force_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('2450694a-75e7-4ba0-9f10-8f2051070281', foundational, state_security_via_non_military_means_only).
narrative_ontology:cs_axiom_status(state_security_via_non_military_means_only, holdable).
narrative_ontology:cs_axiom_grounding('2450694a-75e7-4ba0-9f10-8f2051070281', state_security_via_non_military_means_only, deontological).
narrative_ontology:cs_reference_frame('2450694a-75e7-4ba0-9f10-8f2051070281', absolute_war_renunciation_framework).
narrative_ontology:cs_drift_state('2450694a-75e7-4ba0-9f10-8f2051070281', contemporary_regional_security_escalation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2450694a-75e7-4ba0-9f10-8f2051070281', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, military_institution_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifism_movement_supporters).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, regional_security_competitors).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, us_security_guarantor).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, absolute_war_renunciation_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, pacifism_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal scholars, judicial activists, and peace movement organizations who defend the strict textual reading of Article 9 ('never be maintained') as an absolute prohibition on all military forces. They set the interpretive standard and challenge any expansion of military capability as constitutional violation. They benefit from the constraint's persistence as vindication of their doctrine.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% The Japanese state's capacity to defend itself militarily is structurally foreclosed by the strict reading. Self-defense is constrained to non-military means (diplomacy, economic leverage, civil resistance) or absolute dependence on alliance protection (US security umbrella). The state bears the cost of abandoning defensive military autonomy while remaining a global actor in a security-contingent world.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy, payer,
    institutional, generational, identity_locked, national).

% Active military members, defense planners, and institutional actors within the Self-Defense Forces operate in a state of structural ambiguity: they are categorically prohibited by the strict reading yet empirically maintained through administrative reinterpretation. They absorb the cost of constitutional illegitimacy (the constraint classifies them as constitutional violations) while performing state functions.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, military_institution_personnel, payer,
    moderate, biographical, constrained, national).

% Constituencies that hold deep moral commitment to pacifism and view Article 9 as Japan's unique moral contribution. They benefit from the constraint's existence as an institutional anchor for their values, even where enforcement is incomplete. They use the constraint's categorical language to shape political discourse.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifism_movement_supporters, beneficiary,
    organized, biographical, mobile, national).

% Neighboring states (China, Russia, North Korea) structurally benefit from Japan's military constraint. A strict interpretation of Article 9 reduces regional military capacity that would otherwise be balanced against them. They have no formal veto but their interests align with the constraint's persistence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, regional_security_competitors, beneficiary,
    institutional, generational, analytical, regional).

% The UN Charter's renunciation-of-war framework and global non-proliferation norms are vindicated by Japan's constitutional pacifism reading. The constraint institutionalizes a norm that the international system claims to endorse universally.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, international_peace_architecture, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_9_war_renunciation__strict_pacifist_reading, international_peace_architecture).

% The United States security umbrella becomes indispensable if Japan is constitutionally foreclosed from independent defense. The strict reading perpetuates Japan's structural dependence on US military protection, anchoring US forward deployment in East Asia and Japanese alliance commitment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, us_security_guarantor, beneficiary,
    institutional, generational, constrained, regional).

% Political actors and civil society who actively oppose any constitutional amendment that would permit military forces. They defend the strict reading through legislative obstruction, public advocacy, and litigation. They enforce the constraint by blocking escape routes.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_amendment_blockers, agenda_setter,
    organized, generational, mobile, national).

% Military strategists and security-focused policymakers who would argue for reinterpretation or amendment to permit calibrated defensive capacity (in line with the inherent-right or collective-defense readings). They are structurally excluded from the decision-framing: the strict reading does not admit their voice into the legitimacy calculation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, defense_planners_seeking_flexibility, excluded,
    powerful, biographical, trapped, national).

% The judiciary interprets and applies Article 9 in cases challenging SDF constitutionality or military operations. They occupy an interpretive seat: they can validate the strict reading or adopt a sibling reading through case law. Their jurisprudence determines enforcement and defines the boundary between lived practice and constitutional mandate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, pacifist_constitutional_interpreters).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The strict reading performs no coordination function — it is a categorical rule, not a coordination mechanism. It renounces a set of activities (organized military force) rather than solving a collective-action problem.
% TRANSFER_FUNCTION: The constraint transfers state security autonomy (military self-defense capacity) away from Japan and into structural dependence on alliance protection. It also transfers legitimacy vindication (the constraint validates pacifist doctrine) to pacifism-movement constituencies and international peace architecture.
% ABSENT_VOICES: Defense planners, military strategists, and security-policy realists who would argue for reinterpretation are structurally excluded from the legitimacy framework. Their voice enters only as a violation of the constraint, not as a party to its justification. Regional security competitors benefit from the constraint but do not participate in its adjudication.
% DISAPPEARANCE_RATIONALE: If the strict reading disappeared overnight—replaced by either the inherent-right or collective-defense reading—Japan's defense posture would restructure: military spending, doctrine, regional alliance dependencies, and geopolitical positioning would all shift. The US security guarantee would become optional rather than constitutionally necessary. The constraint's disappearance would be one of the largest institutional reorganizations in modern Japanese governance.
% FOUNDING_PROBLEM: Japan's postwar security framework was designed to prevent remilitarization after the devastation of World War II. The absolute renunciation of war was intended to embed pacifism constitutionally so that future governments could not rearm without explicit amendment, and to signal to regional neighbors (China, Korea, Russia) that Japan would not become a military threat again.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist scholars and peace-movement organizations attest the founding problem remains live: the risk of remilitarization and regional military escalation justifies the absolute constraint. Defense planners and regional security analysts attest the founding problem is substantially solved (70+ years of postwar peace, Japan is a prosperous democracy not a militarist state) and the constraint now persists as a structural lock that constrains necessary defensive adaptation. Historical scholarship on the occupation era and Japanese rearmament debates, produced outside the pacifist advocacy community, documents both the original intent AND the substantial decay of the problem it addressed.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) and rising over the 75-year interval because the constraint's cost accumulates as regional security threats grow while Japan's independent response options remain constitutionally foreclosed. The founding problem (preventing remilitarization) was real but is substantially solved by historical and institutional context; the constraint now persists as a lock extracting security autonomy cost from the state while benefiting alliance dependence and pacifist vindication. Suppression is high (0.72) because enforcement requires active blocking of amendment pathways and reinterpretation routes—the constraint does not persist through voluntary acceptance but through organized obstruction. Theater ratio is moderate (0.41): the pacifist vindication function is genuine, but an increasing share of the constraint's maintenance is performative—the SDF exists anyway in administrative limbo, and the constraint's real function is blocking formal legitimation rather than preventing military organization. Accessibility collapse is very high (0.88): once the categorical reading is understood, alternatives appear logically impossible within that frame—citizens cannot choose to 'reinterpret' their way out; exit requires constitutional amendment, which is structurally trapped. Resistance is substantial (0.69): defense planners, security strategists, and successive governments have continuously challenged or worked around the constraint; the measurement reflects real and sustained resistance, not mere disagreement.
 *
 * PERSPECTIVAL GAP:
 *   From the pacifist interpreters' seat, the constraint is vindicating a moral truth and constitutionalizing peace. From the Japanese state's seat, the constraint is a structural trap that forecloses legitimate self-defense and perpetuates unequal alliance dependence. From the defense planners' seat (excluded), the constraint is an irrational handcuff that prevents prudent security adaptation. The engine computes these gaps from the structural data: different power atoms, different exit options, different time horizons produce different effective extraction even though the same constraint text applies to all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese state occupies the victim seat (d near 1.0): it is identity-locked into pacifism by a constitutional reading it cannot unilaterally escape. Exit requires amendment (supermajority threshold, high political cost), which blockers actively prevent—exit_options = identity_locked. The pacifist interpreters occupy the beneficiary/agenda-setter seat (d near 0.0): they set the interpretive standard, their doctrine is vindicated, and the constraint's persistence serves their values. The US security guarantor occupies a beneficiary seat (d near 0.0): the constraint makes Japan structurally dependent on US protection, anchoring the alliance and perpetuating US forward deployment. Regional competitors benefit (d near 0.0) without paying: they gain security advantage from Japan's military constraint. The paradox: the state nominally 'chooses' the reading (through its courts and constitutional interpretation process) but is thereby locked into a position where it pays the cost and cannot easily escape.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding mandate (prevent remilitarization in postwar Japan, anchor pacifism constitutionally, signal non-threat to neighbors) was real and served a genuine security-ordering function. By the 75-year mark, the founding problem is substantially solved—Japan is a stable democracy, regional neighbors have adjusted to postwar Japan's role, remilitarization is not a plausible near-term threat. The constraint persists not because the founding mandate is still live but because pacifist constituencies have organized to block escape routes (amendment blockers), and because the structural trap locks the state into a position where unilateral exit is costlier than persistence. The theater ratio's rise (0.08 → 0.41) reflects this: early enforcement was substantive (blocking actual military moves); recent enforcement is increasingly performative (blocking discourse, reinterpretation, amendment pathways) because the de facto SDF exists in a state of constitutional limbo. The constraint has become a vehicle for pacifist value vindication rather than a genuine security mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_inherent_right_logical_gap,
    'Does the textual phrase ''never be maintained'' logically foreclose the inherent-right reading (that self-defense capacity is permissible), or is the gap interpretive rather than logical?',
    'Comparative constitutional analysis: do other constitutions use similar language and permit defensive interpretation? Linguistic and historical-semantics analysis of the original Japanese and English texts.',
    'If the gap is logical/textual, the strict and inherent-right readings foreclose each other—only one reading can be correct. If the gap is interpretive, they coexist as competing hermeneutic choices, and the constraint''s classification becomes a matter of which reading is adopted, not what the text requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_inherent_right_logical_gap, conceptual, 'Whether the strict reading''s categorical claim is supported by the text or is an interpretive imposition.').

omega_variable(
    extraction_vs_mandatrophy_classification,
    'Is the constraint a SNARE (extracting state security autonomy through coercive reinterpretation) or a PITON (a once-functional security mechanism now persisting theatrically)?',
    'Structural test: does any seat benefit enough to maintain the constraint actively (snare indicator), or is persistence driven by institutional inertia and enforcement blockers while the beneficiaries (pacifists, US) have already gotten their gain (piton indicator)?',
    'If SNARE: the constraint is actively extractive from Japan and benefiting alliance dependence. If PITON: the constraint is mostly theatrical and persists because fixing it is harder than maintaining the fiction. The classification differs materially in downstream analysis and remediation pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_mandatrophy_classification, empirical, 'Whether the constraint is actively extractive or theatrically maintained.').

omega_variable(
    reading_adoption_as_self_determination,
    'Is Japan''s adoption of the strict reading an authentic expression of democratic pacifist commitment, or is it a structural trap the reading imposes that prevents the state from reconsidering its security framework?',
    'Polling and political-economy analysis: does Japanese public support for Article 9 remain steady when security threats rise, or does it erode? Can amendment proposals gain democratic traction, or does the amendment-blocking structure suppress alternatives before democratic deliberation?',
    'If authentic commitment: the constraint is binding because the reading instantiates genuine values. If structural trap: the constraint persists despite eroding support because exit mechanisms are blocked—the reading forecloses a reading that would permit self-determination on defense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_adoption_as_self_determination, preference, 'Whether the strict reading reflects voluntary national commitment or enforced structural lock.').

omega_variable(
    kernel_vs_reading_stability,
    'The kernel (Article 9 text) is fixed, but the three readings are live and contested. Can a stable constitutional order persist when a single kernel is subject to three mutually exclusive readings, and enforcement favors one reading through blockers rather than judicial settlement?',
    'Constitutional law and institutional-stability analysis: does the unresolved kernel undermine rule of law? Can courts settle the reading, or is judicial restraint leaving the question to politics?',
    'If the kernel cannot stabilize: any stable order requires either judicial settlement (a court declares the correct reading binding) or democratic amendment (the people choose one reading through amendment). Absence of both leaves a constitutional vacuum where the strict reading persists by enforcement blockers, not legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_stability, conceptual, 'Whether the contested kernel can sustain a stable constitutional order given the three competing readings.').

omega_variable(
    us_alliance_dependence_as_intended_feature,
    'Does the US security guarantee benefit from Japan''s military constraint by design (deliberate postwar strategy to anchor Japan''s dependence), or is structural dependence an unintended side effect?',
    'Historical analysis of postwar US-Japan negotiations, declassified diplomatic records, and US strategic doctrine regarding Japan''s role in East Asian security architecture.',
    'If by design: the constraint serves a great-power interest (US forward deployment and alliance control) that is orthogonal to Japan''s security or pacifist vindication—an additional extraction layer. If unintended: the constraint''s benefits to the US are incidental to its pacifist purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_alliance_dependence_as_intended_feature, empirical, 'Whether US benefit from Japan''s military constraint is a designed outcome or a side effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t25, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(arti_tr_t25, observed).
narrative_ontology:measurement(arti_tr_t40, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(arti_tr_t40, observed).
narrative_ontology:measurement(arti_tr_t55, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 55, 0.39).
narrative_ontology:measurement_basis(arti_tr_t55, observed).
narrative_ontology:measurement(arti_tr_t75, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(arti_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t25, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(arti_be_t25, observed).
narrative_ontology:measurement(arti_be_t40, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(arti_be_t40, observed).
narrative_ontology:measurement(arti_be_t55, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 55, 0.76).
narrative_ontology:measurement_basis(arti_be_t55, observed).
narrative_ontology:measurement(arti_be_t75, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 75, 0.78).
narrative_ontology:measurement_basis(arti_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t25, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement_basis(arti_su_t25, observed).
narrative_ontology:measurement(arti_su_t40, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(arti_su_t40, observed).
narrative_ontology:measurement(arti_su_t55, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 55, 0.71).
narrative_ontology:measurement_basis(arti_su_t55, observed).
narrative_ontology:measurement(arti_su_t75, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(arti_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, us_japan_security_treaty).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, japanese_constitutional_amendment_threshold).

% DUAL FORMULATION NOTE:
% Article 9 war renunciation is a single contested kernel instantiated by three structurally distinct readings: strict_pacifist (this file, categorical prohibition, high extraction), inherent_right (self-defense permissible, moderate extraction), and collective_defense (alliance military action permissible, low extraction). The three readings produce different victim sets, different beneficiaries, and different extraction profiles. They are linked by network.affects_constraints in all three files. The kernel itself (the text) is immutable; the readings differ in interpretation and political enforcement. Any one reading being adopted forecloses or influences the others through institutional gate-closure (courts settle the reading; amendment is required to switch). The 'constraint family' is the set of three readings; a complete analysis requires all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__strict_pacifist_reading, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
