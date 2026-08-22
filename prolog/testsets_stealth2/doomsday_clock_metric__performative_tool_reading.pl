% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock Strategic Setting Practice (Performative-Tool Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   Since 1947 the Bulletin of the Atomic Scientists has set the Doomsday
 *   Clock — a published 'minutes to midnight' figure renewed at an annual
 *   press event — as the public's most recognizable summary of proximity to
 *   civilizational catastrophe. This file instantiates the
 *   performative_tool_reading of the doomsday_clock_metric kernel: the
 *   setting is a strategic communication act, selected to maximize policy
 *   impact and mobilize collective action, and the standing arrangement under
 *   examination is that practice of strategic setting itself. Assessed by
 *   this reading's own lights, the arrangement performs a real coordination
 *   service — it solves the attention-synchronization problem of
 *   existential-risk politics — while continuously spending a resource its
 *   operators do not own: the epistemic credibility of the scientific
 *   community and the public's baseline trust in expert alarm. The reading
 *   endorses the spend as justified by the stakes; the spend is nonetheless
 *   real, compounding, and borne substantially by parties never present at
 *   the setting. The sibling readings of the same kernel are separate
 *   constraint files linked through the network block: the
 *   objective_index_reading authors a low-epsilon constraint (a descriptive
 *   index with negligible extraction), and the hybrid_legitimacy_reading
 *   authors a moderate-epsilon one (an entangled judgment practice it holds
 *   could not be otherwise). This file's epsilon is authored only for the
 *   strategic-setting arrangement as the performative reading sees it — the
 *   reading's endorsement of the practice does not zero the cost it
 *   acknowledges the practice imposes.
 *
 * KEY AGENTS:
 *   - bulletin_atomic_scientists: agenda-setting institution and primary capture seat (institutional / identity_locked) — owns the kernel, converts credibility into funding, media access, and relevance
 *   - science_security_board: operative agenda-setter (institutional / identity_locked) — deliberates and sets the clock each year
 *   - existential_risk_advocacy_networks: secondary beneficiary (organized / mobile) — consumes the annual alarm as a mobilization hook
 *   - policy_public_audiences: primary payer (powerless / constrained) — absorbs strategically shaped risk signals
 *   - scientific_advisory_community: payer (institutional / constrained) — lends the credibility that gets spent
 *   - future_policy_audiences: payer (powerless / trapped) — inherits the depleted trust environment
 *   - journalists_and_media_outlets: dual beneficiary/payer (organized / mobile) — cheap story gained, informational product degraded
 *   - national_policymakers: payer with arbitrage exit (powerful) — the persuasion target that can discount the signal
 *   - clock_skeptics_dissenting_experts: excluded (moderate / constrained) — would reform or retire the clock; no seat in the process
 *   - science_communication_researchers: analytical observer (analytical / analytical) — documents the attention-for-credibility trade
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.62).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.48).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Strategic Setting Practice (Performative-Tool Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, 'c4556a08-0e61-4158-b082-c140e67f4ca3').
narrative_ontology:cs_kernel_codification('c4556a08-0e61-4158-b082-c140e67f4ca3', formalized).
narrative_ontology:cs_authority_grounding('c4556a08-0e61-4158-b082-c140e67f4ca3', lineage).
narrative_ontology:cs_interpretation_layer_present('c4556a08-0e61-4158-b082-c140e67f4ca3').
narrative_ontology:cs_reading_relation('c4556a08-0e61-4158-b082-c140e67f4ca3', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('c4556a08-0e61-4158-b082-c140e67f4ca3', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('c4556a08-0e61-4158-b082-c140e67f4ca3', foundational, mobilization_justifies_strategic_setting).
narrative_ontology:cs_axiom_status(mobilization_justifies_strategic_setting, holdable).
narrative_ontology:cs_axiom_grounding('c4556a08-0e61-4158-b082-c140e67f4ca3', mobilization_justifies_strategic_setting, instrumental).
narrative_ontology:cs_axiom('c4556a08-0e61-4158-b082-c140e67f4ca3', foundational, attention_over_accuracy_priority).
narrative_ontology:cs_axiom_status(attention_over_accuracy_priority, holdable).
narrative_ontology:cs_axiom_grounding('c4556a08-0e61-4158-b082-c140e67f4ca3', attention_over_accuracy_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('c4556a08-0e61-4158-b082-c140e67f4ca3', legitimate_mobilization_instrument).
narrative_ontology:cs_drift_state('c4556a08-0e61-4158-b082-c140e67f4ca3', post_expansion_strategic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c4556a08-0e61-4158-b082-c140e67f4ca3', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_networks).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, policy_public_audiences).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_advisory_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_policy_audiences).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, journalists_and_media_outlets).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, journalists_and_media_outlets).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, national_policymakers).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, mobilization_efficacy_doctrine).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, strategic_ambiguity_communication_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes the clock, convenes the Science and Security Board, and stages the annual announcement. Each setting converts the scientific community's standing into a news cycle the organization trades on for funding, media access, and relevance. It cannot retire or descope the clock without dissolving the public identity the organization has built around it; every alternative use of its convening power is worth less to it than the clock it already owns.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_atomic_scientists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, bulletin_atomic_scientists, beneficiary).

% The invited experts who deliberate and set the clock each year. Participation confers platform and affiliation with the founding lineage of the atomic scientists. A member who believes a setting is strategically inflated faces a choice between public dissent (burning standing inside the community that invited them) and continued participation (lending their name to the next setting). Exit is quiet non-renewal, and the seat is then filled by someone more aligned.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_security_board, agenda_setter,
    institutional, biographical, identity_locked, global).

% Arms-control, climate, and biosecurity campaign organizations that synchronize funding drives, lobbying pushes, and media appearances to the annual setting. A move toward midnight hands them a ready-made urgency frame at no cost of their own credibility. They can and do use other hooks — treaty anniversaries, incident-driven news — and would shift within a cycle or two if the clock stopped.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Citizens who encounter the clock through coverage and absorb its setting as a summary of expert judgment on how close catastrophe is. They cannot audit the deliberation, opt out of the information environment, or recalibrate; where the setting is strategically shaped rather than descriptively derived, their risk perception is shaped the same way. Repeated visible moves erode their baseline trust in expert alarm generally.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_public_audiences, payer,
    powerless, biographical, constrained, global).

% The broad body of scientific institutions whose collective standing backs the clock's claim to authority. Each strategically shaped setting spends a portion of that standing. The community as a whole cannot disown the clock without a public fratricide it has shown no appetite for, and no individual body gains by being first to object. Silence is the default posture, which is exactly what keeps the standing available to spend.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_advisory_community, payer,
    institutional, generational, constrained, global).

% People not yet born or not yet attentive who will inherit whatever baseline of trust in expert risk assessment remains after decades of visibly strategic alarm-setting. They are never present at the setting, cannot decline the inheritance, and will later have to evaluate genuine warnings inside the discounted-credibility environment this practice leaves them.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_policy_audiences, payer,
    powerless, generational, trapped, global).

% Get an annually renewed, pre-packaged urgency story with a built-in visual; the January setting is cheap, reliable coverage. The same dynamic degrades their informational product: a setting moved for effect is a story they transmit without the caveats a descriptive index would carry, and sophisticated readers increasingly discount their risk coverage accordingly. They could drop the clock at low cost but keep it because the attention economics favor it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, journalists_and_media_outlets, beneficiary,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, journalists_and_media_outlets, payer).

% Governments and their advisors are the alarm's intended target: the setting is aimed at moving their decisions on arsenals, treaties, and risk budgets. They receive a strategically shaped signal but hold strong exits — internal risk assessments, intelligence estimates, rival expert channels — and increasingly treat the clock as advocacy to be weighed rather than measurement to be consumed. The distortion cost lands on them lightly; their discounting is part of what devalues the signal for everyone else.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, national_policymakers, payer,
    powerful, biographical, arbitrage, national).

% Risk scholars, some former Bulletin contributors, and science-communication researchers who argue the clock conflates measurement with advocacy, that its moves are not reproducible from any stated method, and that it should be retired or converted into a transparent index. They publish outside the setting process; the process has no seat for them, and their critiques are absorbed as attention the annual announcement monetizes.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, clock_skeptics_dissenting_experts, excluded,
    moderate, biographical, constrained, global).

% Study the clock's effects on public risk perception, trust in expertise, and mobilization outcomes. They neither collect from nor bear the annual setting; they document the trade the arrangement makes between attention and credibility, and supply the longitudinal data any resolution of the efficacy question would draw on.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_communication_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, bulletin_atomic_scientists).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the attention-and-synchronization problem of existential-risk politics: diffuse, slow-moving, abstract catastrophes do not generate their own deadlines, so advocacy campaigns, media cycles, and policy windows lack a shared urgency signal. The annual setting supplies one focal point that dispersed actors can coordinate around without negotiating a common metric themselves.
% TRANSFER_FUNCTION: Moves epistemic credibility — the standing of the scientific community and the public's baseline trust in expert alarm — out of the credibility commons and into the advocacy sector, where the Bulletin and campaign networks convert it into media attention, agenda-setting leverage, and mobilization, renewed at each annual setting.
% ABSENT_VOICES: Dissenting experts and risk-communication scholars who would retire or reform the clock have no seat in the deliberation; future audiences whose trust is spent cannot appear; ordinary citizens whose risk perceptions are shaped have no channel into the process; and rival risk-communication formats — transparent indices, direct expert testimony — are crowded out of the January attention window the clock occupies.
% DISAPPEARANCE_RATIONALE: Advocacy networks would lose their synchronization point and reorganize around substitutes within a cycle or two; the Bulletin would lose its flagship asset, funding engine, and much of its public identity; January news cycles would lose a fixture; and the credibility commons would stop being drawn down and begin slow repair — rearrangement concentrated on the organized beneficiaries, repair accruing to the diffuse payers.
% FOUNDING_PROBLEM: In 1947 the scientists who had built the atomic bomb faced a public that could not read technical risk assessments but needed to grasp that nuclear weapons had made catastrophe a standing possibility; the clock was built as a vivid, legible symbol of proximity to danger that required no technical literacy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the atomic age corroborate the 1947 founding problem from outside the benefiting parties, and risk-communication research corroborates that attention-capture for abstract catastrophic risk was a real unsolved problem. No one outside the beneficiary set attests that the problem still requires this instrument in its present strategic form: risk scholars and several former Bulletin-affiliated experts explicitly dispute it, and the Bulletin's own annual statements are the main source attesting continued necessity.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62: the practice converts credibility it holds in trust into mobilization leverage, and the cost lands on the credibility commons and on audiences rather than on the operators — but the clock still performs some descriptive work (its deliberations track real developments in arsenals, climate, and biosecurity), so the spend is substantial rather than total. Suppression is 0.48: there is no legal coercion; the suppressive force is agenda control — gatekeeping the setting process, absorbing critics as attention, crowding alternative risk indices out of the annual attention window — and it required deliberate machinery to build (the annual doomsday-statement apparatus, board expansion, media pre-briefing), which is why suppression_requirement is tracked and rises across the interval rather than left as a static scalar. Theater_ratio is 0.60: the announcement event, record-proximity framing, and social campaigns are staged communication by design; the underlying risk assessment is real but increasingly subordinated to the communicative arc. Accessibility_collapse is 0.42: transparent indices and direct expert testimony persist and are strengthened by criticism of the clock. Resistance is 0.50: recurring, visible criticism from risk scholars and former insiders, absorbed rather than accommodated. All three series run on one shared grid (1947-2025 at 13-year steps) so every metric is authored at every examined point; the trajectories show the early clock (1947-1973) operating close to a descriptive gauge with low theater and little enforcement machinery, and the post-Cold-War clock (1999-2025) operating as an openly strategic instrument with rising extraction, staging, and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the Bulletin and board seats the arrangement is a legitimate instrument they steward — the coordination function is vivid and the credibility spend reads as justified expenditure, so those seats compute rope-flavored. From the payer seats — public audiences, the scientific advisory community, future audiences — the same annual event is a transfer drawn from accounts they hold in trust, so those seats compute extraction-heavy. The dual-positioned media seat sees a cheap story and a degraded product simultaneously. Across readings the divergence is sharper still: a seat holding the objective_index_reading classifies this same practice as a corrupted measurement (snare-flavored, since the coordination story would be cover), while a seat holding the hybrid reading sees a legitimate entangled practice. Same-level divergence: national policymakers and public audiences are both addressed by the alarm, but the policymakers' arbitrage exit (rival assessments, internal intelligence estimates) lets them discount the signal, so the distortion cost concentrates almost entirely on the constrained public seat — the signal degrades fastest exactly where exit is worst.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structural positions: the Bulletin (agenda-setter and capture seat) and the advocacy networks sit near the beneficiary end (d near 0) — the practice subsidizes them with attention and mobilization frames. The credibility payers sit near the target end (d near 1): public audiences and future audiences are constrained or trapped in the information environment, and the scientific advisory community's standing is what is spent. Journalists sit near symmetric (cheap story gained, informational product degraded). National policymakers carry high nominal directionality as the alarm's target, but their arbitrage-grade exit damps their effective extraction — they can re-price the signal, which is precisely how their discounting devalues it for the constrained seats. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making nuclear-era catastrophe legible to a non-technical public — was genuinely live in 1947 and is contested now. The classification guards against two opposite errors. Reading the arrangement as a pure snare would deny the real mobilization coordination it performs and mispredict the behavior of its sincere participants (board members who believe the stakes justify the spend); reading it as a pure rope would erase the asymmetric credibility transfer the expected structural delta names and the receipt surface confirms (gains capture at the Bulletin seat; fixing is prohibitive for the only agent who could fix it). The tangled_rope claim holds both facts in view. The R5 mismatch check does not fire a zombie flag here: founding_problem_status is contested (not dead) and the disappearance verdict is world_rearranges — the arrangement persists because organized arrangements depend on it, not because it outlived a dead mandate. The live risk is not mandatrophy but efficacy decay: if credibility depletion outruns mobilization gain (see omegas), the coordination leg atrophies and the same structure re-derives as extraction with a theatrical residue — the drift the measurements already show in miniature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates the performative_tool_reading of the doomsday_clock_metric kernel: the setting is strategically chosen to maximize policy impact and mobilize collective action. The sibling readings change the constraint structurally: the objective_index_reading treats the setting as a measurement (low epsilon, zero strategic-manipulation tolerance, and the strategic-setting practice itself becomes the violation), and the hybrid_legitimacy_reading treats the setting as irreducible entanglement of science and values (moderate epsilon, no separable victim). Which reading governs determines the beneficiary set, the victim set, and the type entirely.',
    'Seat-level reading-adoption analysis: author the same annual event once per reading as separate constraint stories and compare computed types; no within-story averaging over readings.',
    'Under the objective reading this arrangement is a corrupted measurement (snare-shaped); under the hybrid reading it is a legitimate entangled practice (rope-shaped); under this reading it is a hybrid coordination-extraction structure. Cross-reading comparison is the point of the family, not a defect to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the doomsday_clock_metric kernel governs, and what each sibling reading would change.').

omega_variable(
    substitutability_of_mobilization_function,
    'If the clock-setting practice vanished overnight, would the advocacy networks re-synchronize around substitutes within a cycle or two, or does the clock supply an urgency focal point that substitutes cannot replicate?',
    'Natural experiments: years when the announcement was disrupted or delayed, and campaign-coordination behavior around treaty anniversaries and incident-driven news that substitute for the clock; measure whether mobilization timing re-coordinates without it.',
    'If fully substitutable, the coordination leg is weak and the structure re-derives closer to pure extraction; if not substitutable, the coordination function is genuine and the hybrid coordination-extraction reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutability_of_mobilization_function, empirical, 'Whether the mobilization coordination the clock supplies has real substitutes.').

omega_variable(
    credibility_spend_replenishment_balance,
    'Does the mobilization gained per annual setting exceed the long-run cost of the trust it depletes, or is the credibility commons being drawn down faster than any mobilization benefit replenishes it?',
    'Longitudinal trust-in-expertise series correlated with clock-coverage exposure and setting salience; campaign outcome tracking against baseline mobilization without clock hooks.',
    'If depletion dominates, the reading''s own justification (catastrophic stakes justify the spend) fails and the arrangement degrades toward pure extraction even on this reading''s own terms; if gains dominate, the spend is a defensible investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_spend_replenishment_balance, empirical, 'Whether the credibility expenditure is net-positive for the epistemic commons that bears it.').

omega_variable(
    strategic_tolerance_boundary,
    'Does this reading''s tolerance for strategic setting have a boundary — settings chosen to serve the Bulletin''s institutional fundraising and survival rather than mobilization for risk reduction — and if so, where does it sit?',
    'Revealed-preference analysis: correlate setting decisions and their public rationales with Bulletin funding cycles and organizational needs; internal deliberation records if ever disclosed.',
    'If settings track institutional self-maintenance, the mobilization justification collapses and the arrangement is pure extraction even by this reading''s own lights; a clean mobilization rationale sustains the hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_tolerance_boundary, empirical, 'Whether strategic setting serves mobilization or institutional self-maintenance.').

omega_variable(
    future_audience_standing,
    'Do future audiences who inherit the depleted trust count as present victims with standing in the cost accounting, or is their exclusion from the present calculus legitimate?',
    'A values question: resolvable only by an explicit normative stance on intergenerational standing in epistemic-commons accounting; empirical work can size the inherited trust deficit but cannot settle standing.',
    'Counting future audiences raises the victim set''s weight and pushes effective extraction higher; excluding them concentrates the cost on present audiences and lowers measured extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_audience_standing, preference, 'Intergenerational standing of the trust-depletion cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(doom_tr_t1960, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(doom_tr_t1973, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1973, 0.27).
narrative_ontology:measurement(doom_tr_t1986, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1986, 0.35).
narrative_ontology:measurement(doom_tr_t1999, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1999, 0.45).
narrative_ontology:measurement(doom_tr_t2012, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2012, 0.53).
narrative_ontology:measurement(doom_tr_t2025, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.22).
narrative_ontology:measurement(doom_be_t1960, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1960, 0.27).
narrative_ontology:measurement(doom_be_t1973, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1973, 0.33).
narrative_ontology:measurement(doom_be_t1986, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1986, 0.39).
narrative_ontology:measurement(doom_be_t1999, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1999, 0.46).
narrative_ontology:measurement(doom_be_t2012, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(doom_be_t2025, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.12).
narrative_ontology:measurement(doom_su_t1960, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1960, 0.17).
narrative_ontology:measurement(doom_su_t1973, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1973, 0.24).
narrative_ontology:measurement(doom_su_t1986, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1986, 0.31).
narrative_ontology:measurement(doom_su_t1999, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1999, 0.38).
narrative_ontology:measurement(doom_su_t2012, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(doom_su_t2025, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Doomsday Clock' covers at least three structurally distinct claims — a descriptive risk index (objective_index_reading, low epsilon), an entangled judgment practice (hybrid_legitimacy_reading, moderate epsilon), and a strategic mobilization instrument (this file, higher epsilon with a named credibility victim). The label's ambiguity was the measurement problem; the family decomposes it. This story is the upstream pressure source for the hybrid reading (each visibly strategic setting erodes the objective account and feeds the entanglement account) and forecloses the pure objective account within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
