% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index (Expert Synthesis Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists' Doomsday Clock converts a
 *   portfolio of empirical indicators — deployed warhead counts, arms-control
 *   treaty viability, climate trajectories, biological and disruptive
 *   technology developments — into a single annual public setting expressed
 *   as minutes to midnight. This story authors the constraint as the
 *   objective-index reading holds it: a measurement produced by expert
 *   synthesis, whose authority rests on the empirical character of its
 *   inputs. Read by that reading's own lights, the standing arrangement
 *   nonetheless carries a real extraction structure: presenting the setting
 *   as the output of value-neutral synthesis suppresses the normative framing
 *   on which any risk threshold depends, concentrating interpretation of
 *   civilization-scale risk in the expert board and removing the weighting
 *   choices from democratic contest. Epsilon's referent is the standing clock
 *   arrangement, never the transparent, auditable index this reading endorses
 *   as its ideal; the 0.62 measures how far the actual arrangement falls
 *   short of that ideal, borne by democratic accountability. The claim
 *   (tangled_rope) and the metrics are authored independently: the metrics
 *   describe the arrangement's operation as this reading assesses it, and the
 *   engine computes per-seat classifications from the structural data. KEY
 *   AGENTS (by structural relationship): - science_and_security_board:
 *   agenda-setting expert body (institutional/identity_locked) — selects and
 *   weighs indicators, issues the setting, controls methodology; its
 *   authority is constituted by custody of the clock -
 *   existential_risk_expertise_community: secondary beneficiary
 *   (organized/constrained) — collects authority and resource spillover from
 *   the clock's epistemic anchor - informed_lay_public: primary target
 *   (moderate/trapped) — receives the setting as settled measurement, unable
 *   to audit or contest the weighting choices -
 *   civil_society_policy_movements: target (organized/constrained) — must
 *   defer to the index or be cast as unscientific -
 *   policy_elites_and_legislators: dual-positioned consumer (powerful/mobile)
 *   — gains a justification anchor, cedes independent assessment capacity -
 *   normative_ethicists: excluded voice (moderate/trapped) — holds that risk
 *   thresholds are value choices; outside the indicator process -
 *   science_journalists_and_media: incidental beneficiary (organized/mobile)
 *   — collects the annual news event without administering it -
 *   sts_and_epistemic_authority_scholars: analytical observer
 *   (analytical/analytical) — documents the gap between the objective frame
 *   and the judgment-laden practice
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.62).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.72).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index (Expert Synthesis Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '7dde1208-438b-4363-9233-14ba57688d15').
narrative_ontology:cs_kernel_codification('7dde1208-438b-4363-9233-14ba57688d15', formalized).
narrative_ontology:cs_authority_grounding('7dde1208-438b-4363-9233-14ba57688d15', expertise).
narrative_ontology:cs_interpretation_layer_present('7dde1208-438b-4363-9233-14ba57688d15').
narrative_ontology:cs_reading_relation('7dde1208-438b-4363-9233-14ba57688d15', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7dde1208-438b-4363-9233-14ba57688d15', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('7dde1208-438b-4363-9233-14ba57688d15', foundational, existential_risk_empirically_quantifiable).
narrative_ontology:cs_axiom_status(existential_risk_empirically_quantifiable, holdable).
narrative_ontology:cs_axiom_grounding('7dde1208-438b-4363-9233-14ba57688d15', existential_risk_empirically_quantifiable, empirically_contingent).
narrative_ontology:cs_axiom('7dde1208-438b-4363-9233-14ba57688d15', secondary, expert_synthesis_value_neutral).
narrative_ontology:cs_axiom_status(expert_synthesis_value_neutral, holdable).
narrative_ontology:cs_axiom_grounding('7dde1208-438b-4363-9233-14ba57688d15', expert_synthesis_value_neutral, empirically_contingent).
narrative_ontology:cs_reference_frame('7dde1208-438b-4363-9233-14ba57688d15', objective_empirical_risk_index).
narrative_ontology:cs_drift_state('7dde1208-438b-4363-9233-14ba57688d15', contemporary_sts_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7dde1208-438b-4363-9233-14ba57688d15', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, science_and_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, existential_risk_expertise_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, informed_lay_public).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, civil_society_policy_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, policy_elites_and_legislators).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, science_journalists_and_media).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, policy_elites_and_legislators).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, measurable_existential_risk_premise).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, value_free_expertise_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes twice yearly to select and weigh indicators of civilization-scale hazard, issues the annual setting with a supporting statement, and controls the methodology, press protocol, and access through which the setting reaches the public. Its standing as the authoritative voice on global catastrophic risk is constituted by its custody of the clock; presenting the output as empirical synthesis is integral to how it works, and stepping outside that frame would dissolve the basis of its own authority.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_and_security_board, agenda_setter,
    institutional, generational, identity_locked, global).

% Researchers, think tanks, and former officials whose grant lines, citation networks, and advisory seats rest on the premise that expert bodies can quantify global catastrophic risk. The clock anchors the field's public legitimacy and gives quantitative risk-talk a recognizable public face; declining that anchor would mean forgoing the authority premium the whole epistemic ecosystem draws from it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, existential_risk_expertise_community, beneficiary,
    organized, generational, constrained, global).

% Receives the annual setting as a settled measurement of how close humanity stands to catastrophe. It cannot audit which indicators were chosen, how they were weighted, or what thresholds were assumed; questioning the number tends to read as hostility to science rather than participation in a value-laden choice. It bears the democratic cost of decisions framed as technical findings rather than choosable tradeoffs, and it cannot exit exposure to a signal that saturates news cycles every year.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, informed_lay_public, payer,
    moderate, biographical, trapped, global).

% Disarmament, anti-nuclear, and climate justice organizations whose own risk judgments must either defer to the index or be cast as unscientific advocacy. Their participation is confined to endorsing or contesting a number they had no hand in constructing; organizing an independent assessment would cost credibility and resources they do not have.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, civil_society_policy_movements, payer,
    organized, generational, constrained, national).

% Cite the setting to anchor positions and shift the burden of justification onto recognized experts, which relieves them of defending contested tradeoffs in their own voice. At the same time they surrender independent risk-assessment capacity and inherit whatever weightings and thresholds the board embedded. They could commission rival assessments but rarely do, since nothing else matches the clock's publicity.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_elites_and_legislators, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, policy_elites_and_legislators, beneficiary).

% Scholars of risk ethics who hold that any threshold of acceptable catastrophe is a value choice about whose lives, which time horizons, and how much uncertainty to tolerate. They sit entirely outside the indicator process; their contributions surface only as after-the-fact commentary on a setting already issued, with no channel into the weighting itself.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, normative_ethicists, excluded,
    moderate, generational, trapped, global).

% Receive a ready-made annual news event: dramatic imagery, a quotable number, an authoritative quote. They collect attention and copy from the unveiling without administering anything, and have little incentive to interrogate how the number was constructed.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_journalists_and_media, beneficiary,
    organized, immediate, mobile, global).

% Researchers of expertise, authority, and public science who study how the clock's annual ritual manufactures epistemic closure around judgment-laden choices. They take no part in setting the clock and bear none of its costs; their role is documenting the distance between the announced empirical character of the setting and the deliberative practice behind it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, sts_and_epistemic_authority_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, science_and_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed empirical indicators of civilization-scale hazard — nuclear force postures, arms-control treaty viability, climate trajectories, biological and disruptive technology developments — into one publicly legible annual signal, solving the problem that no citizen or legislature can synthesize these streams independently.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk from publics and legislatures to the expert board; moves agenda-setting power toward the hazards the board selects; and returns to the board and the wider expertise community the epistemic prestige and resource access that attach to being the recognized measure of the risk.
% ABSENT_VOICES: Normative ethicists, participatory-assessment advocates, and the movements whose risk judgments the index subordinates are outside the room. They deliberate in philosophy faculties, activist networks, and the science-and-technology-studies literature, surfacing only as after-the-fact commentary on a setting they had no hand in weighting.
% DISAPPEARANCE_RATIONALE: The expert risk-communication ecosystem is organized around the clock as its flagship signal: media cycles, funding narratives, and the board's own authority would rearrange within a few annual cycles, and rival indices or open-assessment arrangements would compete to fill the vacated anchor role. The underlying hazards would not change; the arrangement of who speaks for them would.
% FOUNDING_PROBLEM: After 1945, the scientists who built the bomb held knowledge of a new scale of danger that democratic publics and leaders could not absorb from ordinary indicators; the clock was founded in 1947 to translate specialist judgment into a form the public sphere could act on.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science document the 1947 founding context independently of the Bulletin; civic-epistemic research attests the persistent expert-lay translation gap; and the reading's own critics — science-and-technology-studies scholars and risk ethicists writing outside the beneficiary set — concede the translation problem is real while disputing whether this arrangement solves it. Corroboration for the problem's liveness therefore exists outside the benefiting parties; corroboration for the clock being the right answer to it does not.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.62: the transfer is interpretive authority rather than money — the objective frame converts contestable value choices (acceptable-risk thresholds, time horizons, indicator salience) into settled technical output, and the surplus of authority this creates accrues to the board. Suppression 0.72: the operative mechanism is framing control — methodology opacity, press protocol, and the epistemic penalty attached to questioning a 'scientific' finding — which suppresses normative counter-framing more effectively than overt coercion would; suppression is authored as a raw structural property and is not scaled by power or scope. Theater 0.30: the indicator synthesis is real work, but the annual unveiling ceremony and the seconds-to-midnight precision carry a performative load the measurement does not require. Accessibility_collapse 0.45: alternatives (participatory assessment, rival indices, open deliberation) survive but are discounted once the authoritative number lands. Resistance 0.55: sustained critique from STS scholars, ethicists, and movements keeps the frame contested rather than naturalized. Claim and metrics are independent: tangled_rope is asserted from the structure — a real synthesis function, asymmetric authority extraction through the same structure, and active enforcement of the frame — not tuned to the metric profile. All three series share one time grid (t=0..78, seven points, 1947–2025); trajectories are monotonic consolidation of the objective frame, not cyclical, so no oscillation mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat the arrangement is faithful instrumentation: it experiences the constraint as the discipline that keeps its judgment honest and its authority legitimate. From the payer seats the same structure operates as closure — a number they cannot audit, issued under a frame that recodes their objections as anti-scientific. The dual-positioned legislative seat experiences both at once: anchoring convenience and surrendered assessment capacity. The engine computes these per-seat classifications from the structural data; the divergence between the board's self-experience and the payer experience is the perspectival gap this story encodes, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the board sits near the beneficiary end (the arrangement subsidizes its authority) and the expertise community slightly above it (spillover prestige, constrained exit). Victims: the lay public sits near the full-target end (trapped recipient of an unauditable signal) and civil society movements high (constrained to defer-or-be-dismissed). The legislative seat is genuinely mixed — it collects the justification anchor and pays in surrendered capacity — so a directionality override sets the powerful atom to d=0.5 rather than letting the derivation collapse it to either pole; the derivation cannot distinguish this dual position from a pure payer at the same power level. Media sits outside the declared beneficiary/victim arrays: its collection is incidental attention, recorded in the stakeholder situation rather than forced into the structural declarations, so its directionality falls to the derivation chain's handling of undeclared seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabels. Reading the clock as pure coordination would erase the monopoly extraction the objective frame performs on democratic accountability; reading it as pure extraction would erase the genuine synthesis service no other institution currently provides — the coordination and the extraction run through the same structure, which is precisely the tangled-rope signature. Mandatrophy is not resolved: the founding problem (democratic systems cannot absorb specialist risk knowledge) is live and corroborated from outside the benefiting parties. The characteristic drift risk runs toward piton rather than snare: if indicator synthesis were opened or automated, the annual unveiling could persist as ritual while the measurement function migrated elsewhere — theater_ratio rising past the level at which ceremony still serves dissemination is the early indicator to watch, and the current 0.30 with a slow upward slope is consistent with early-stage ceremonial accretion, not yet degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the objective_index_reading of the doomsday_clock_metric kernel; which structural facts would change under the sibling readings?',
    'Re-author the arrangement under each sibling reading as separate constraint stories: performative_tool_reading relocates epsilon onto advocacy effectiveness and recasts the agenda-setter as a strategic communicator managing mobilization; hybrid_legitimacy_reading dissolves the objective-frame referent and counts the suppressed normativity as constitutive of the domain rather than extractive overlay.',
    'Classification is reading-relative: this file computes as tangled_rope under the objective reading; the performative reading trends toward snare (coordination story as cover for mobilization management); the hybrid reading reframes the extraction as an unavoidable property of existential-risk communication rather than a removable defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    disagreement_location_setting_determinant,
    'Where is the kernel disagreement located: what actually determines the clock setting — empirical indicators (this reading), strategic impact calculus (performative reading), or irreducibly entangled judgment (hybrid reading)?',
    'Process-trace the actual setting deliberations: board minutes, dissent records, and the sensitivity of announced outcomes to variation in the underlying indicators versus stability across changed rhetorical circumstances.',
    'If deliberations show outcome-first reasoning, this reading''s epsilon understates the extraction and the performative reading gains factual ground; if outcomes track indicator variation, the performative reading loses its factual basis and this reading''s low-normative-load claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_setting_determinant, empirical, 'Locates the inter-reading dispute in the determinant of the annual setting.').

omega_variable(
    indicator_selection_normativity,
    'Can the board''s indicator selection and weighting be audited as value-free, or do the thresholds encode normative choices about acceptable risk, time horizon, and whose survival counts?',
    'Publication of the full methodology with explicit weights and sensitivity analysis, plus replication by independent panels holding divergent value commitments to test whether their settings converge.',
    'Demonstrated normative load-bearing in the weights raises effective extraction toward the snare boundary and confirms the victim declaration; demonstrated convergence under value diversity supports the objective reading''s claim that the measured extraction is largely the price of the synthesis service itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indicator_selection_normativity, empirical, 'Whether the index''s construction imports unacknowledged value judgments.').

omega_variable(
    public_deference_mechanism,
    'Is the public''s deference to the clock setting structural (no access to indicators or methodology) or internalized (diffuse authority deference that would persist even under open methodology)?',
    'Natural experiment across jurisdictions or cohorts with open-methodology access and risk-literacy programs: does informed contest of the setting rise where the structural barrier is removed?',
    'If deference is internalized, suppression persists after transparency remedies and the structural suppression measure understates the constraint''s hold on the payer seats; if structural, opening the methodology collapses much of the measured suppression and the arrangement migrates toward ordinary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_deference_mechanism, empirical, 'Structural versus internalized deference to expert risk authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(doom_tr_t13, doomsday_clock_metric__objective_index_reading, theater_ratio, 13, 0.15).
narrative_ontology:measurement(doom_tr_t26, doomsday_clock_metric__objective_index_reading, theater_ratio, 26, 0.19).
narrative_ontology:measurement(doom_tr_t39, doomsday_clock_metric__objective_index_reading, theater_ratio, 39, 0.23).
narrative_ontology:measurement(doom_tr_t52, doomsday_clock_metric__objective_index_reading, theater_ratio, 52, 0.26).
narrative_ontology:measurement(doom_tr_t65, doomsday_clock_metric__objective_index_reading, theater_ratio, 65, 0.28).
narrative_ontology:measurement(doom_tr_t78, doomsday_clock_metric__objective_index_reading, theater_ratio, 78, 0.3).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(doom_be_t13, doomsday_clock_metric__objective_index_reading, base_extractiveness, 13, 0.4).
narrative_ontology:measurement(doom_be_t26, doomsday_clock_metric__objective_index_reading, base_extractiveness, 26, 0.46).
narrative_ontology:measurement(doom_be_t39, doomsday_clock_metric__objective_index_reading, base_extractiveness, 39, 0.52).
narrative_ontology:measurement(doom_be_t52, doomsday_clock_metric__objective_index_reading, base_extractiveness, 52, 0.57).
narrative_ontology:measurement(doom_be_t65, doomsday_clock_metric__objective_index_reading, base_extractiveness, 65, 0.6).
narrative_ontology:measurement(doom_be_t78, doomsday_clock_metric__objective_index_reading, base_extractiveness, 78, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(doom_su_t13, doomsday_clock_metric__objective_index_reading, suppression_requirement, 13, 0.33).
narrative_ontology:measurement(doom_su_t26, doomsday_clock_metric__objective_index_reading, suppression_requirement, 26, 0.42).
narrative_ontology:measurement(doom_su_t39, doomsday_clock_metric__objective_index_reading, suppression_requirement, 39, 0.52).
narrative_ontology:measurement(doom_su_t52, doomsday_clock_metric__objective_index_reading, suppression_requirement, 52, 0.6).
narrative_ontology:measurement(doom_su_t65, doomsday_clock_metric__objective_index_reading, suppression_requirement, 65, 0.67).
narrative_ontology:measurement(doom_su_t78, doomsday_clock_metric__objective_index_reading, suppression_requirement, 78, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Doomsday Clock' per the epsilon-invariance principle. The label conflates three structurally distinct claims: that the setting tracks measurable risk through expert synthesis of empirical indicators (this file, objective_index_reading); that the setting is strategically chosen to maximize policy impact and mobilization (performative_tool_reading); and that the setting embodies irreducible entanglement of scientific judgment and normative stakes (hybrid_legitimacy_reading). Each reading is a separate constraint with its own epsilon, beneficiary/victim structure, and classification; forcing one story to span them would make epsilon observer-dependent. This file authors the objective reading: its epsilon referent is the standing clock arrangement assessed by the objective reading's own lights, so the extraction it must count is the interpretive monopoly its own objectivity claim produces. The objective reading is upstream: its epistemic authority claim is the resource the performative reading exploits and the hybrid reading contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
