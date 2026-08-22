% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   A newly-drafted charter in a post-revolutionary state establishes
 *   Islamic-nationalist identity as the sovereign legitimacy ground for state
 *   authority and law. This is the 'guided nationalism' reading: religious
 *   institutional leadership interprets Islamic legitimacy; nationalist
 *   movements consolidate state identity via religious framing; secular civil
 *   society, religious minorities, and internationalist movements lose
 *   constitutional standing. The constraint does coordinate
 *   post-revolutionary state identity — solving a genuine founding problem of
 *   how to consolidate authority without fragmentation — AND it extracts
 *   substantially from those who reject Islamic nationalism as the legitimacy
 *   frame. This is the core tangled-rope structure: coordination function is
 *   real; asymmetric extraction via suppression of alternatives is also real
 *   and actively enforced. The measurement series tracks how extractiveness
 *   and theater ratio increase over the interval as religious authority
 *   crystallizes and secular alternatives are progressively delegitimized.
 *
 * KEY AGENTS:
 *   - Religious institutional leadership: interpreter and beneficiary of Islamic-nationalist frame
 *   - Nationalist political movements: consolidate state identity through religious legitimation
 *   - Secular civil society: identity-locked targets of constitutional demotion
 *   - Religious minorities: trapped targets losing equal legal status
 *   - Labor/left movements: suppressed via frame delegitimization
 *   - Military/security apparatus: enforcer of the charter's religious-nationalist boundaries
 *   - International secular actors: structurally excluded from legitimate discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '76416910-7860-4d9f-8370-a3a847ec594e').
narrative_ontology:cs_kernel_codification('76416910-7860-4d9f-8370-a3a847ec594e', formalized).
narrative_ontology:cs_authority_grounding('76416910-7860-4d9f-8370-a3a847ec594e', lineage).
narrative_ontology:cs_interpretation_layer_present('76416910-7860-4d9f-8370-a3a847ec594e').
narrative_ontology:cs_reading_relation('76416910-7860-4d9f-8370-a3a847ec594e', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_reading_relation('76416910-7860-4d9f-8370-a3a847ec594e', july_charter_sovereign_legitimacy__secular_democratic_reading, coexists_with).
narrative_ontology:cs_axiom('76416910-7860-4d9f-8370-a3a847ec594e', foundational, islamic_identity_as_state_legitimacy).
narrative_ontology:cs_axiom_status(islamic_identity_as_state_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('76416910-7860-4d9f-8370-a3a847ec594e', islamic_identity_as_state_legitimacy, conventional).
narrative_ontology:cs_axiom('76416910-7860-4d9f-8370-a3a847ec594e', secondary, religious_institutional_authority_supremacy).
narrative_ontology:cs_axiom_status(religious_institutional_authority_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('76416910-7860-4d9f-8370-a3a847ec594e', religious_institutional_authority_supremacy, deontological).
narrative_ontology:cs_reference_frame('76416910-7860-4d9f-8370-a3a847ec594e', islamic_nationalist_sovereignty).
narrative_ontology:cs_drift_state('76416910-7860-4d9f-8370-a3a847ec594e', contemporary_post_charter_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76416910-7860-4d9f-8370-a3a847ec594e', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutional_leadership).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, nationalist_political_movements).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, labor_and_left_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_and_security_apparatus).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, divine_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_law_supremacy).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, nationalist_identity_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the religious framework embedded in the charter; gains constitutional status for religious law and norms; controls legitimacy discourse around what counts as 'Islamic' governance. Frames the arrangement as restoring authentic sovereignty after colonial secular rule. Enforcement apparatus ensures secular alternatives cannot claim equal constitutional legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Gain legitimacy by anchoring nationalism in religious identity rather than civic nationalism or ethnic tribalism. The charter provides a framework that consolidates state identity and allows them to claim stewardship of both religious and national authority. Benefits from suppression of internationalist and secular-leftist alternatives.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, nationalist_political_movements, beneficiary,
    organized, generational, constrained, national).

% Faces constitutional demotion of secular legal frameworks; secular law becomes subordinate to religious authority interpretation. Professional identity in law, education, healthcare, and media becomes contested when secular expertise competes with religious authority. Exit means renouncing membership in national civic community or professional suppression. Internal debate is constrained by the charter's elevation of religious framing as authoritative.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, identity_locked, national).

% Lose constitutional status as equal citizens; their legal capacity and rights become contingent on majority-religion interpretation of the charter. Religious practice not aligned with the charter's Islamic frame faces de facto suppression through enforcement of religious law norms. Exit options are migration (often legally restricted), conversion (identity erasure), or accepting subordinate legal status.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Face suppression under nationalist identity framework: class-based solidarity is reframed as un-Islamic or counter-nationalist. International labor coordination is delegitimized. The charter provides constitutional cover for suppressing leftist organizing as incompatible with the sovereign nationalist-religious identity. Resistance requires positioning against both religious authority and nationalism simultaneously, narrowing political viability.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, labor_and_left_movements, payer,
    moderate, biographical, constrained, national).

% Gains constitutional authority to suppress dissent in the name of preserving Islamic-nationalist sovereignty; framed as protecting both religious and national integrity. Benefits from expansive interpretation of security threats to legitimacy. Enforces the charter's religious-nationalist framework against secular and leftist challenges.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_and_security_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_and_security_apparatus, beneficiary).

% Cannot operate as legitimate voices within the constitutional framework; their human-rights and liberal-democratic frameworks are externalized as foreign impositions on sovereign Islamic-nationalist identity. Diplomatic leverage is framed as neo-colonial interference. Their exclusion from legitimate internal debate is structural.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_secular_liberal_actors, excluded,
    powerful, biographical, trapped, global).

% Interpreters of the charter's religious-nationalist framework; their authority derives from applying it faithfully rather than revising it. They face pressure to align interpretations with religious institutional leadership and nationalist movements while maintaining appearance of judicial independence. Their legitimacy depends on the framework itself.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, constitutional_court_or_adjudicators, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutional_leadership).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared national identity ground via religious legitimacy — consolidates state authority by anchoring sovereignty in Islamic nationalism rather than disputed secular-democratic or ethnic-particularist grounds. Solves the coordination problem of post-revolutionary state-building: 'Who has the right to speak for this nation?' by declaring it is those who steward its Islamic-nationalist character.
% TRANSFER_FUNCTION: Transfers interpretive authority over law, education, media, and public norms from secular experts and civil society to religious institutional leadership. Also transfers political legitimacy from international democratic standards and civic inclusion to nationalist-religious authentication. Extracts political voice and legal standing from secular civil society, religious minorities, and internationalist movements; concentrates it in religious institutions and nationalist political movements.
% ABSENT_VOICES: Secular internationalist perspectives, liberal-democratic theorists, religious minorities advocating for equal status, labor movements positioning class solidarity over nationalist identity, and external human-rights monitoring are structurally excluded. They are not absent by accident — their exclusion is what the charter's enforcement machinery exists to maintain. They would argue for secular constitutionalism, minority-protective frameworks, and de-linking nationalism from religious identity, but the charter's legitimacy structure prevents them from being heard as legitimate participants.
% DISAPPEARANCE_RATIONALE: If the charter's religious-nationalist legitimacy frame vanished overnight, competing legitimacy claims would resurface immediately: military-custodian authority, secular democratic constitutionalism, and religious-plural frameworks would become live contestants. The state's identity and legal authority would be re-contested. The constraint's disappearance would not restore a prior stable state but rather reopen a fundamental question the constraint's enforcement exists to suppress.
% FOUNDING_PROBLEM: Post-revolutionary state requires a shared identity frame to consolidate authority and prevent fragmentation into military rule, secular-liberal contestation, or sectarian/ethnic partialism. Religious identity was selected as the legitimacy ground to bridge nationalist sentiment with deep cultural resonance and to marginalize secular-leftist internationalism seen as destabilizing.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutional leadership and nationalist movements affirm the founding problem is live and the charter solution is necessary for stability. Secular civil society, religious minorities, labor movements, and international observers attest the problem is reframed: the charter does not solve state fragmentation equitably but rather imposes one faction's identity as sovereign, displacing competing legitimacy grounds. Constitutional scholars outside the benefiting parties document that secular-democratic alternatives and inclusive minority-protective frameworks existed at founding and were rejected not on coordination grounds but on political power grounds.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as 0.68 at interval end: the constraint transfers legal and political authority from secular frameworks to religious authority; it suppresses alternatives through constitutional demotion rather than overt violence (initially), but enforcement machinery exists and hardens over the interval. Suppression is 0.71: the charter itself is the suppression mechanism — by constitutionalizing religious-nationalist legitimacy, it makes secular legal and political authority structurally secondary. Theater ratio of 0.42 reflects that early rhetoric emphasizes authentic national recovery and religious restoration (performative elements), but as the constraint hardens, enforcement activity increasingly targets resistance rather than maintaining the coordination narrative. The measurement series show extractiveness rising from 0.42 to 0.68 as the charter's enforcement structures mature and secular alternatives are progressively disabled — not because the coordination problem changes, but because the constraint's extractive machinery is implemented. Suppression_requirement rises steeply early (0.55 to 0.68 by t=10) as resistance from secular and minority constituencies crystallizes, then plateaus as suppression hardens into normalized practice and resistance is either suppressed or driven underground. Theater ratio rises more gradually, suggesting the legitimacy narrative holds longer than the enforcement machinery needs rhetorical support — a sign the charter has achieved cultural embedding.
 *
 * PERSPECTIVAL GAP:
 *   The religious institutional leadership and nationalist movement seats experience this as legitimate state-building and identity consolidation — the constraint solves a real coordination problem and they are the stewards. From these seats the arrangement appears as rope. The secular civil society and religious minority seats experience the same arrangement as enforced constitutional demotion and extraction of political voice — from their seats it appears as snare. The engine computes both: at the religious-leadership seat, the constraint's function is primarily coordination (low d, beneficiary directionality); at the secular-civil-society seat, the same structure operates as extraction through suppression (high d, target directionality). The military/security apparatus sits at a hybrid position: it enforces the arrangement and benefits from expanded suppressive authority, but it is also constrained by the charter's religious-nationalist frame — it cannot unilaterally revise the legitimacy ground, so it is a secondary beneficiary with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutional leadership: d ≈ 0.15 (full beneficiary) — they interpret the frame, gain constitutional authority, face no suppression from the charter itself. Nationalist movements: d ≈ 0.25 (beneficiary with constrained exit) — they consolidate state identity and suppress their rivals, but they remain bounded by the religious-nationalist frame they did not unilaterally create. Secular civil society: d ≈ 0.82 (near-full target) — they lose constitutional standing, face identity-locking (secular professional identity is delegitimized), suppression of alternatives is structural. Religious minorities: d ≈ 0.88 (full target) — trapped exit, legal subordination, no political voice, suppression is both structural (constitutional demotion) and active (enforcement). Labor/left movements: d ≈ 0.75 (high target) — constrained exit (internationalist positioning becomes delegitimized), suppression via frame-delegitimization (class solidarity reframed as un-Islamic), organized enough to resist but weak relative to nationalist-religious consolidation. The directionality values are authored from the beneficiary/victim declarations and the exit-option constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is building post-revolutionary state authority without fragmentation. At founding, the charter does solve a real coordination problem: it provides a shared legitimacy ground that bridges religious cultural authority, nationalist sentiment, and state power. This is not pure extraction — it is genuine coordination layered with asymmetric extraction of voice and legal standing. The tangled_rope classification captures this: the constraint is held together by both coordination benefit (for religious/nationalist beneficiaries) AND suppression (for those excluded from the legitimacy frame). The measurement series show that over time the coordination narrative persists (theater ratio increases but stays moderate), while extractiveness and suppression requirement both rise — the constraint becomes progressively more extractive as the charter's enforcement machinery solidifies and alternatives are disabled. Mandatrophy would arise if the founding problem (fragmentation) is solved sufficiently that the suppression becomes the only function — but for this reading, the founding problem remains live: any weakening of the religious-nationalist legitimacy frame would immediately trigger the sectarian/ethnic fragmentation the charter was built to prevent. The suppression is not theater; it is structural enforcement of the legitimacy frame itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_authority_interpretation_drift,
    'Will religious institutional interpretation of the charter''s religious-nationalist legitimacy ground converge on a stable reading, or will competing interpretations proliferate and fragment the religious legitimacy frame itself?',
    'Time-series analysis of religious institutional pronouncements and legal rulings; tracking whether religious authority consolidates around one interpretation or splinters into competing schools. Also observable through compliance patterns: do secular actors face a single clear constraint or multiple contradictory religious authority claims?',
    'If interpretation consolidates, the constraint hardens as a tangled_rope with stable suppression of alternatives. If it splinters, the religious-nationalist legitimacy ground itself becomes contested, potentially inverting the constraint into a snare controlled by military/security institutions that then arbitrate between religious factions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_interpretation_drift, empirical, 'Whether religious institutional authority consolidates or fragments over the interval.').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the suppression of secular civil society and religious minorities primarily structural (they are blocked from institutional access) or internalized (they come to believe the religious-nationalist frame is legitimate even as they are excluded)?',
    'Post-exit trajectory analysis: if secular or minority individuals leave the jurisdiction and suppress lifts immediately, suppression was primarily structural; if suppression persists (they continue avoiding public voice), suppression is partially internalized. Also: opposition movement data — are they organized around rejecting the charter or accepting its legitimacy while fighting for better terms within it?',
    'If primarily structural, suppression weakens if alternatives become available (emigration routes, parallel institutions). If partially internalized, suppression persists even after structural barriers lower — the constraint is ''carried'' by affected agents. This distinction affects whether suppression_requirement plateaus or falls if political conditions shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression is structural or internalized in affected constituencies.').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Can the guided_nationalism reading and the secular_democratic reading coexist as legitimate alternative interpretations of the same charter within a single constitutional framework, or does one reading logically foreclose the other?',
    'Jurisprudential evidence: can a court operating under the charter uphold both a judgment grounded in Islamic-nationalist legitimacy AND a judgment grounded in secular democratic principles without internal contradiction? Or does accepting one framework require rejecting the other? Also: documented positions of constitutional guardians and courts on whether multiple legitimacy grounds can coexist.',
    'If foreclosure exists (one reading rules out the other logically), the kernel is more unstable — the contest is binary, and one side must ultimately prevail. If they coexist, the kernel is more resilient — multiple interpretations can be held in parallel by different institutions or over time. This affects whether the constraint''s classification can shift (coexistence allows peaceful re-reading; foreclosure suggests the reading can only change via constitutional crisis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Whether guided_nationalism and secular_democratic readings can coexist or one forecloses the other.').

omega_variable(
    nationalist_identity_fusion_depth,
    'How deeply are nationalist political movements and ordinary citizens identity-fused with the religious-nationalist frame? Is it a strategic political construction (elite-imposed) or a genuinely resonant cultural identity?',
    'Survey and ethnographic evidence on whether people adopt the religious-nationalist identity because it aligns with existing cultural practice or because institutions enforce it; opposition discourse — do dissenters attack the religious-nationalist frame itself or only its application; generational analysis — do younger cohorts socialized under the charter''s legitimacy frame show stronger fusion than older cohorts?',
    'If identity fusion is deep and cultural, the constraint is more resilient to political shocks and suppression can be lighter (internalized). If it is elite-imposed and strategic, suppression must be heavier to maintain, and alternative framings can gain traction if political opportunity opens. The measurement series'' rising theater_ratio suggests performative maintenance may be increasing — a sign identity fusion may be incomplete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nationalist_identity_fusion_depth, empirical, 'Depth of identity fusion with the religious-nationalist frame among nationalist movements and general population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(july_tr_t5, observed).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(july_tr_t10, observed).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(july_tr_t15, observed).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(july_tr_t20, observed).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(july_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(july_be_t5, observed).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(july_be_t10, observed).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(july_be_t15, observed).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(july_be_t20, observed).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(july_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(july_su_t5, observed).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(july_su_t10, observed).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(july_su_t15, observed).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(july_su_t20, observed).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(july_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the july_charter_sovereign_legitimacy kernel. The sibling constraints (military_custodian_reading and secular_democratic_reading) derive different obligation and victim structures from the same charter text. Each reading is ε-invariant and empirically distinct. Linked via network.affects_constraints to enable cross-reading analysis of how alternative legitimacy frames affect institutional power distribution and victim identification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
