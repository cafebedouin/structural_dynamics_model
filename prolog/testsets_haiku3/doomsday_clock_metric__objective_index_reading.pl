% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock Metric — Objective Index Reading
 *   domain: science/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists,
 *   presents itself as an objective metric of existential risk — a measured
 *   translation of empirical indicators (nuclear weapons stockpiles, climate
 *   tipping points, biosecurity threats, AI development trajectories) into a
 *   symbolic time-to-midnight reading. This constraint story instantiates the
 *   OBJECTIVE_INDEX_READING: the clock's claim to track measurable risk
 *   through expert synthesis. The reading asserts that the clock functions as
 *   an epistemic instrument — its setting should correspond to an empirical
 *   risk assessment performed by qualified experts applying transparent
 *   methodology. Under this reading, extractiveness emerges from the
 *   suppression of normative framing: the clock's authority depends on
 *   appearing purely empirical while actually embedding contested value
 *   judgments (what risks count, how to weight them, what policy urgency is
 *   proportionate). The reading's core premise: the clock's legitimacy rests
 *   on demonstrable correspondence between its setting and measurable risk
 *   indicators, not on its policy impact or symbolic power. Where that
 *   correspondence breaks down, the claim to objective index fails. This
 *   reading COEXISTS WITH the performative_tool_reading (clock strategically
 *   chosen for policy impact) and the hybrid_legitimacy_reading (irreducible
 *   entanglement of science and values), held by different parties; it does
 *   not foreclose them. It INFLUENCES them by establishing an empirical
 *   benchmark against which they can be contested.
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists — agenda-setter, defines the metric, controls the weighting and interpretation
 *   - Nuclear weapons policymakers — payers, subject to accountability pressure when the clock tightens
 *   - Climate catastrophe researchers — payers, their findings filtered through the clock's weighting scheme
 *   - Democratic publics — payers and beneficiaries, receive a unified risk signal but cannot challenge its construction
 *   - Rival risk assessment frameworks — excluded, their alternative metrics do not displace the clock's authority
 *   - Empiricist critics — observers, audit the clock's epistemic status but lack power to alter it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.79).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock Metric — Objective Index Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'a98348bd-6130-4123-934b-0d82dc7bf942').
narrative_ontology:cs_kernel_codification('a98348bd-6130-4123-934b-0d82dc7bf942', formalized).
narrative_ontology:cs_authority_grounding('a98348bd-6130-4123-934b-0d82dc7bf942', expertise).
narrative_ontology:cs_interpretation_layer_present('a98348bd-6130-4123-934b-0d82dc7bf942').
narrative_ontology:cs_reading_relation('a98348bd-6130-4123-934b-0d82dc7bf942', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('a98348bd-6130-4123-934b-0d82dc7bf942', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('a98348bd-6130-4123-934b-0d82dc7bf942', foundational, existential_risk_empirically_measurable).
narrative_ontology:cs_axiom_status(existential_risk_empirically_measurable, holdable).
narrative_ontology:cs_axiom_grounding('a98348bd-6130-4123-934b-0d82dc7bf942', existential_risk_empirically_measurable, empirically_contingent).
narrative_ontology:cs_axiom('a98348bd-6130-4123-934b-0d82dc7bf942', secondary, expert_weighting_transparency_achievable).
narrative_ontology:cs_axiom_status(expert_weighting_transparency_achievable, holdable).
narrative_ontology:cs_axiom_grounding('a98348bd-6130-4123-934b-0d82dc7bf942', expert_weighting_transparency_achievable, empirically_contingent).
narrative_ontology:cs_reference_frame('a98348bd-6130-4123-934b-0d82dc7bf942', expert_consensus_empirical_grounding).
narrative_ontology:cs_drift_state('a98348bd-6130-4123-934b-0d82dc7bf942', contemporary_ai_risk_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a98348bd-6130-4123-934b-0d82dc7bf942', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority_seats).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, democratic_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, nuclear_weapons_policymakers).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, climate_catastrophe_researchers).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_publics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and announces the Doomsday Clock reading annually. Maintains interpretive authority over what the clock measures, which indicators feed the metric, how they weight, and what the reading means. Justifies authority by epistemic credentials and founding mandate as voice of scientific consensus. Controls the narrative: what moves the clock moves public and policy attention. Has complete discretion to adjust weighting, add new risk domains, or change the methodology — and faces no external review or democratic accountability for these choices.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to clock-driven accountability pressure. When the clock moves closer to midnight, they face intensified scrutiny of nuclear postures, arms control treaties, and proliferation risks. They cannot exit the clock's frame (it is a global standard) but can attempt to influence what indicators the Bulletin counts or how it interprets data. When the clock tightens, their policy space contracts and they must justify why they are not doing more to reduce risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, nuclear_weapons_policymakers, payer,
    powerful, biographical, constrained, global).

% Their empirical findings on existential climate risk are filtered through the clock's weighting scheme. If their research suggests accelerating risk, they depend on the Bulletin's board to give climate appropriate weight in the metric — but the weighting is set by the Bulletin, not by climate researchers. They must argue within the Bulletin's framework to move the needle on their domain. Dispute the clock's weighting but have no structural leverage to change it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, climate_catastrophe_researchers, payer,
    organized, generational, constrained, global).

% Receive a symbolic unified representation of collective existential risk that they cannot independently verify, challenge, or debate. They are asked to trust expert judgment on what the clock measures and what it means. They cannot contest the weighting or the interpretation. They benefit from having a risk signal at all (better than ignorance of existential threats); they bear the cost of outsourcing existential-risk interpretation to an expert monopoly and accepting whatever policy implications the clock's setting drives.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_publics, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, democratic_publics, beneficiary).

% Alternative existential-risk frameworks (Global Challenges Foundation, Future of Humanity Institute models, AI safety research communities with different weighting schemes, climate-focused organizations) produce different risk assessments that diverge from the clock's reading. They are structurally excluded from the Bulletin's authority — their frameworks do not set global policy attention or media narratives the way the clock does. The clock monopolizes the symbolic authority to define existential risk in policy and public discourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, rival_risk_assessment_frameworks, excluded,
    moderate, generational, constrained, global).

% Epistemologists, philosophers of science, and empirical risk analysts who examine whether the clock's reading actually tracks measurable risk or performs a normative function dressed as measurement. They have no power to alter the clock but can publish analyses that deconstruct its epistemic status and highlight the value judgments embedded in its weighting. Their work is auditable but circulates in academic channels and does not displace the clock's authority in policy discourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, empiricist_epistemology_critics, observer,
    moderate, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, bulletin_of_atomic_scientists).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared global metric for existential risk that allows scientists, policymakers, and publics to orient toward a common reference point. Solves the coordination problem of translating disparate expert assessments (nuclear weapons stability, biosecurity, climate tipping points, AI development) into a single actionable signal without requiring consensus on precise probabilities or causal mechanisms.
% TRANSFER_FUNCTION: Transfers interpretive authority over existential risk from distributed expert communities and democratic deliberation to the Bulletin of the Atomic Scientists' appointed board. The clock moves policy attention and shapes what risks dominate public discourse; the power to set the clock's reading is the power to define what counts as existential risk and at what urgency level.
% ABSENT_VOICES: Democratic publics and scientific experts outside the Bulletin's board cannot challenge the weighting or methodology; rival risk assessment frameworks are structurally excluded from the authority structure; indigenous and global-south voices on risk, resilience, and adaptive capacity are absent from the decision-making body. Those whose risk tolerance, risk assessment, or risk priorities differ from the Bulletin's reading have no seat at the table that sets the clock.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock disappeared, existential risk would not disappear, but the unified symbolic metric that routes all existential risk signals through a single epistemic gate would collapse. Policymakers would have to reassemble disparate expert assessments from IPCC, Future of Humanity Institute, AI safety communities, and other frameworks; publics would lose a visceral, unified reference point; media narratives and policy attention would fragment across competing risk frames. The coordination function would dissolve, and the interpretive monopoly would shatter. Policy responses to existential risk would reorganize around multiple signals rather than one.
% FOUNDING_PROBLEM: In the early nuclear age (1940s-1950s), scientific experts possessed urgent knowledge about weapons-induced existential risk but had no standardized way to communicate urgency to policymakers and publics. Competing risk assessments and technical jargon obscured the stakes. A unified, visceral, publicly comprehensible metric was needed to translate specialized expertise into actionable policy signal.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists attests the founding problem is still live: nuclear weapons remain an existential threat, new risk domains (AI, pandemics, climate) have emerged and require unified signaling, and the clock is essential for coordinating global response. Policymakers and competition authorities increasingly attest that the founding problem is substantially solved by other means (peer review systems, IPCC assessments, AI governance forums, UN risk assessments) and the clock persists as authority maintenance and institutional rent. Published critiques by risk governance experts, historians of science, and epistemologists document the shift from founding function to authority function. Legislative testimony from policymakers indicates they have multiple risk signals available and do not depend exclusively on the clock.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the clock's authority to define existential risk is unilateral and unaccountable: the Bulletin sets the metric without public input, democratic review, or mandatory transparency in its weighting decisions. The extraction is not financial (the Bulletin is a non-profit) but epistemic and political — the power to frame which risks dominate global attention is a form of authority rent. Suppression is highest among the metrics (0.79) because the clock's persistence depends actively on suppressing alternative framings: rival risk assessment frameworks are excluded from authority; the normative choices embedded in the weighting are presented as empirical facts; critics' deconstructions are circulated in academic channels but do not reach policy or media discourse with the clock's penetration. Theater (0.42) is moderate: the annual announcement ceremony and the symbolic midnight metaphor are performative, but the underlying claim to track measurable risk is genuinely empirical work, not pure theater. The measurement series show extractiveness and suppression rising together from 1945 to 2024, indicating that as new risk domains (climate, AI) were added to the clock, the interpretive authority required to integrate them grew — the clock's gate-keeper function intensified even as its empirical scope expanded. This is not a natural law (accessibility_collapse = 0.72, not 0.85+) and it faces real resistance (0.58) from critics who deconstruct its epistemic basis and from policymakers who resist the implied urgency.
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin and democratic publics should compute different seats. From the Bulletin's position, the clock is a genuine coordination mechanism translating dispersed expert knowledge into actionable policy signal — the beneficiary seat, low directionality. From the publics' and policymakers' positions, the clock is an enforced interpretive monopoly that constrains their ability to debate what risks are existential and at what urgency — the target seat, high directionality. The empiricist critics occupy an observer seat: they can audit the correspondence between the clock's setting and measurable risk, but they have no power to alter the clock or to prevent it from shaping policy discourse. The engine computes this divergence from the structural data: the Bulletin holds institutional power, arbitrage-grade exit (can set the clock however it chooses), and faces negligible enforcement cost; the publics and policymakers are constrained by the clock's global authority and cannot exit the frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure is straightforward: the Bulletin of the Atomic Scientists benefits from the clock's authority — it gains institutional legitimacy, policy influence, and media attention by being the keeper of the existential risk metric. The victims are less obvious but structural: democratic publics and distributed risk researcher communities bear the cost of an interpretive monopoly. They cannot contest the weighting, cannot propose alternative risk framings with equivalent authority, and are constrained to argue within the Bulletin's metric. Climate researchers pay by having their empirical findings filtered through a weighting scheme they did not set. Policymakers pay by being subject to clock-driven accountability pressure that may not correspond to their own risk assessments. The excluded rivals (alternative frameworks) are not victims exactly — they are structurally barred from the authority game the clock dominates. Suppression operates at two levels: structural (the clock's monopoly on the authority slot) and internalized (policymakers and publics internalize the clock as the legitimate risk metric even when they privately disagree with its weighting).
 *
 * MANDATROPHY ANALYSIS:
 *   The claim is tangled_rope: the clock genuinely coordinates global attention on existential risks (rope function) AND it suppresses alternative risk framings and democratic contestation of the weighting (extraction function). The classification prevents mislabeling it as pure coordination (rope) because the suppression is active and structural, not incidental. It also prevents mislabeling it as pure snare because the coordination function is real — publics and policymakers genuinely benefit from having a unified reference point. The mandatrophy question: has the founding problem (translating expertise into urgency) outlived the clock's function? The contested reading of founding_problem_status answers this: the Bulletin says the problem is still live (nuclear weapons + new existential risks require constant signaling), while critics say the problem is substantially solved (IPCC, peer review systems, AI governance forums all translate expertise without a unilateral metric). This mismatch — founding_problem_status=contested + disappearance_verdict=world_rearranges — routes to the zombie flag: the clock persists despite its founding problem being contestably solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correspondence_degradation,
    'Does the clock''s setting actually track measurable existential risk levels, or has the correspondence between setting and empirical indicators degraded into a maintained appearance?',
    'Reconstruction of the Bulletin''s weighting methodology across historical period (1945-2024); comparison of the explicit weighting scheme against the published decision minutes; econometric analysis of whether clock movements correlate with measurable risk indicators or with political/media cycles.',
    'If correspondence is substantial and transparent, the objective_index reading holds — the clock is a genuine empirical instrument with embedded values but traceable to data. If correspondence is weak or the weighting is undisclosed, the objective_index reading collapses and the clock reclassifies as performative (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(correspondence_degradation, empirical, 'Whether the clock''s empirical basis is real or performed.').

omega_variable(
    alternative_coordination_modes,
    'Is the clock''s monopoly on existential risk framing structurally necessary for expert-to-policy translation, or are alternative frameworks (IPCC, Future of Humanity Institute, AI governance forums) capable of the same coordination function without the authority suppression?',
    'Comparative analysis of policy responsiveness to different risk signal sources; interviews with policymakers about which frameworks actually shape their decision-making; natural experiment from jurisdictions that adopt rival metrics and observe whether policy outcomes diverge.',
    'If alternatives can coordinate without the monopoly, the extraction component becomes isolated and the constraint reclassifies toward snare. If the clock''s unified symbolic authority is genuinely irreplaceable, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_modes, empirical, 'Whether the clock''s exclusive authority is functionally necessary.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative risk framings structural (the clock''s institutional monopoly blocks rival frameworks from reaching policy discourse) or internalized (policymakers and publics have internalized the clock as legitimate even when they privately disagree with its weighting)?',
    'Post-exposure analysis: if the Bulletin lost institutional authority (e.g., a major scandal undermined it), would alternative frameworks immediately surface in policy discourse, or would the suppression persist through internalized norms and expectations?',
    'If suppression is primarily structural, it could be remedied by opening the authority structure to multiple voices or transparent weighting. If primarily internalized, the constraint''s extraction would persist even after institutional changes because the targets have fused their risk-thinking with the clock''s framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates at the institutional level or through internalized legitimacy.').

omega_variable(
    kernel_contest_foreclosure,
    'Does the objective_index_reading''s foundational premise (the clock can and should be empirically grounded) logically foreclose the hybrid_legitimacy_reading''s premise (empirical and normative are irreducibly entangled in existential risk assessment)?',
    'Philosophical analysis of whether ''empirically grounded with embedded values'' (objective_index) is a coherent position distinct from ''irreducibly entangled'' (hybrid). Can one hold both — that the clock is grounded in empirical indicators AND that the empirical/normative boundary is irreducible?',
    'If they are logically incompatible (objective_index forecloses hybrid), then the kernel has a genuine logical structure and the readings are not coexistent. If they can both be true (both describe different aspects of the same phenomenon), then they coexist, and different parties can legitimately hold them in different framings of what the clock IS.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Logical relationship between the objective index and hybrid readings at the level of their foundational premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1945, doomsday_clock_metric__objective_index_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(doom_tr_t1965, doomsday_clock_metric__objective_index_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(doom_tr_t1985, doomsday_clock_metric__objective_index_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__objective_index_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement(doom_tr_t2012, doomsday_clock_metric__objective_index_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__objective_index_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t1945, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(doom_be_t1965, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(doom_be_t1985, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(doom_be_t2012, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1945, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement(doom_su_t1965, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1965, 0.63).
narrative_ontology:measurement(doom_su_t1985, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(doom_su_t2012, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2012, 0.77).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.08).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel admits three structurally distinct constraint stories: the objective_index_reading (this story) asserts the clock's authority rests on empirical correspondence; the performative_tool_reading asserts authority rests on policy impact; the hybrid_legitimacy_reading asserts empirical and normative are irreducibly entangled in existential risk assessment. Each story has its own ε (extractiveness differs across readings because they measure different referents: the empirical claim, the performative function, and the value entanglement), its own beneficiary/victim structure, and its own classification. The stories are linked via network.affects_constraints because they contest the same kernel and each reading's epistemic premises create pressure on the others' validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
