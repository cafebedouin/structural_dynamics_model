% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Separation of Powers: Flexible Framework with Intelligible-Principle Delegation
 *   domain: constitutional law/political theory/administrative law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the functionalist settlement of
 *   American separation of powers: Congress enacts broad statutory mandates
 *   carrying an intelligible principle, agencies convert those mandates into
 *   binding rules and adjudications, and courts uphold the delegations while
 *   policing only their outer margins. This story instantiates ONE reading of
 *   the constitutional text and evaluates THAT arrangement by the
 *   functionalist reading's own lights; the formalist and unitary-executive
 *   readings are separate constraints in separate files, linked through the
 *   network. The claimed type and the metrics are independent authored facts:
 *   from the authoring seat the arrangement is a tangled rope, because it
 *   simultaneously solves a real coordination problem (no modern legislature
 *   can specify technical regulation in detail) and runs a real transfer
 *   through the same structure (interpretive authority moves from courts to
 *   agencies, compliance costs onto regulated parties, political risk from
 *   legislators to administrators), held in place by active judicial
 *   enforcement. The metrics describe that dual character without being tuned
 *   to any predicted engine output.
 *
 * KEY AGENTS:
 *   - federal_administrative_agencies: primary agenda-setter and beneficiary (institutional/identity_locked) — administers the delegated regimes; their existence is the framework's product
 *   - congressional_delegating_majorities: agenda-setter and beneficiary (institutional/constrained) — delegate costly decisions, retain credit and oversight levers
 *   - presidential_administrations: beneficiary (institutional/mobile) — inherit flexibility and control levers fresh each term
 *   - article_iii_courts: payer and enforcer (institutional/constrained) — police the framework's boundaries while bearing reduced interpretive primacy
 *   - regulated_industries: payer (organized/constrained) — bear compliance costs, partially offset by lobbying access
 *   - regulated_individuals: payer (powerless/trapped) — subjects of agency adjudication with no meaningful exit
 *   - state_governments_preempted: payer (organized/constrained) — absorb displaced policy space from outside the interbranch bargain
 *   - administrative_law_scholarship: analytical observer (analytical/analytical) — maps the structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.41).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.56).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Separation of Powers: Flexible Framework with Intelligible-Principle Delegation").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional law/political theory/administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '7d181e24-80b7-407f-ba3c-b13365e4185d').
narrative_ontology:cs_kernel_codification('7d181e24-80b7-407f-ba3c-b13365e4185d', fixed_text).
narrative_ontology:cs_authority_grounding('7d181e24-80b7-407f-ba3c-b13365e4185d', lineage).
narrative_ontology:cs_interpretation_layer_present('7d181e24-80b7-407f-ba3c-b13365e4185d').
narrative_ontology:cs_reading_relation('7d181e24-80b7-407f-ba3c-b13365e4185d', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d181e24-80b7-407f-ba3c-b13365e4185d', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('7d181e24-80b7-407f-ba3c-b13365e4185d', foundational, delegation_with_intelligible_principle_valid).
narrative_ontology:cs_axiom_status(delegation_with_intelligible_principle_valid, holdable).
narrative_ontology:cs_axiom_grounding('7d181e24-80b7-407f-ba3c-b13365e4185d', delegation_with_intelligible_principle_valid, instrumental).
narrative_ontology:cs_axiom('7d181e24-80b7-407f-ba3c-b13365e4185d', foundational, liberty_secured_by_internal_checks_not_sealed_boundaries).
narrative_ontology:cs_axiom_status(liberty_secured_by_internal_checks_not_sealed_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('7d181e24-80b7-407f-ba3c-b13365e4185d', liberty_secured_by_internal_checks_not_sealed_boundaries, instrumental).
narrative_ontology:cs_reference_frame('7d181e24-80b7-407f-ba3c-b13365e4185d', functional_allocation_for_effective_governance).
narrative_ontology:cs_drift_state('7d181e24-80b7-407f-ba3c-b13365e4185d', post_loper_bright_term, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7d181e24-80b7-407f-ba3c-b13365e4185d', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congressional_delegating_majorities).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, presidential_administrations).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_individuals).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_industries).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, state_governments_preempted).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, article_iii_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the delegated regimes: they draft binding rules under broad statutory mandates, adjudicate disputes in their domains, and prosecute violations. Their budgets, personnel, and institutional purpose exist only because the delegation framework validates their authority; an agency exiting the framework would be dissolving itself. Institutional identity and delegated function have fused.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_administrative_agencies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, federal_administrative_agencies, beneficiary).

% Enact the broad statutes that hand technical and politically costly decisions to agencies, then claim credit for the resulting protections while blaming the agency for unpopular specifics. Reclaiming the delegated authority would require drafting detailed rules across thousands of technical domains that the committee process cannot staff, and would force votes on every contested parameter the delegation currently conceals.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congressional_delegating_majorities, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congressional_delegating_majorities, beneficiary).

% Inherit the framework fresh each term: appointment power, executive orders, and budget leverage over agencies let an administration pursue policy goals without new legislation. Each administration rotates out on a fixed clock, so no administration bears the long-run cost of the discretion it exercises; exit from the framework is meaningless because every successor inherits the same levers.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, presidential_administrations, beneficiary,
    institutional, immediate, mobile, national).

% Adjudicate challenges to agency action and police the outer boundaries of the delegation framework. Deference doctrines required courts to accept reasonable agency interpretations of ambiguous statutes even where the judges would have read differently, transferring interpretive primacy away from the bench. Life tenure insulates judges from retaliation, but the framework binds them doctrinally: they enforce the settlement while bearing its cost to their own constitutional role. Recent tightening of review shows they retain partial capacity to adjust the terms.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, article_iii_courts, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, article_iii_courts, agenda_setter).

% Bear the compliance costs of agency rulemaking: reporting duties, equipment mandates, licensing regimes. They participate in notice-and-comment proceedings, lobby Congress and the Office of Information and Regulatory Affairs, and litigate with mixed success under deference standards. Large firms absorb costs and sometimes shape the rules to their advantage; smaller firms and market entrants carry the same rules without the influence.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_industries, payer,
    organized, biographical, constrained, national).

% Encounter the framework as subjects of agency adjudication: benefit determinations, immigration proceedings, professional licensing, environmental exposures. They did not choose the agency that decides their case, cannot shop for a different decisionmaker, and typically lack the resources to sustain appellate litigation against the government. Outcomes turn on agency discretion and internal review structures they cannot meaningfully contest.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_individuals, payer,
    powerless, biographical, trapped, national).

% Federal agency rules displace state policy choices in areas from environmental standards to insurance regulation. States litigate preemption, petition for waivers, and implement federal programs they did not design, but they hold no seat in the interbranch bargain that allocates the power being exercised over them: the framework divides authority among federal branches and leaves the states to absorb the result.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, state_governments_preempted, payer,
    organized, generational, constrained, regional).

% Maps the framework's operation across cases and decades: documents the gap between the stated tests and actual outcomes, tracks how delegation volume and deference intensity move together, and supplies the vocabulary in which reform proposals are argued. Collects nothing from the arrangement and pays none of its costs.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_law_scholarship, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the capacity problem of modern governance: a bicameral, presentment-bound legislature cannot specify technical rules for pharmaceutical safety, air quality, financial markets, or telecommunications in usable detail, so the framework routes policy direction through Congress, technical specification through expert agencies, and boundary policing through courts. Each branch contributes the function it performs at lowest cost.
% TRANSFER_FUNCTION: Moves legislative discretion from Congress to agencies via open-ended statutory mandates; moves statutory interpretive authority from courts to agencies via deference doctrines; moves compliance costs onto regulated firms and individuals; moves political risk for unpopular specifics from legislators to unelected administrators.
% ABSENT_VOICES: State governments, whose policy space is displaced by federal agency rules, were never given a seat in the interbranch bargain that allocates the power exercised over them. Individual subjects of agency adjudication lack the resources to appear in notice-and-comment dockets or fund appeals, so the consent structure records organized industry voices and is nearly silent on theirs. Future generations bearing long-tail regulatory commitments have no representative at all. All three stand outside the branch-to-branch negotiation the framework consists of.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, tens of thousands of existing agency rules would rest on invalidated delegations, the regulatory state would lose its operating license, and the governing system would rearrange around either congressional attempts to legislate in detail it cannot produce, judicial redrawing of every interbranch boundary, or presidential assertion of unreviewable discretion. Food safety approvals, environmental permits, and financial supervision would not continue on autopilot; the entire administrative apparatus would need re-legitimation.
% FOUNDING_PROBLEM: Preventing the accumulation of governing power in a single branch, on the theory that concentrated power threatens liberty: the division of legislative, executive, and judicial functions was the structural answer to tyranny articulated in the founding generation and inherited from Montesquieu.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians corroborate the founding problem from the ratification record itself: the Federalist essays and state ratification debates treat dispersed power as the central design requirement, and neither the text nor the history is the product of the arrangement's modern beneficiaries. Critics positioned entirely outside the settlement's beneficiary set, including scholars and judges who press for stricter boundaries, attest that the anti-concentration problem remains live, which is precisely why they object to the flexible reading. No party inside the arrangement is needed to establish that the founding problem existed or that disagreement about its current urgency is genuine.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.41, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.41 at interval end) rather than high because the functionalist reading prices the arrangement's transfers as substantially legitimate: delegation to expertise and deference to administered judgment are, by this reading's lights, the coordination working, with the residual extraction lying in decoupling between the breadth of discretion granted and the accountability attached to it. Suppression is moderate-high (0.56) because the framework binds challengers doctrinally — a regulated party cannot opt out of the agency's jurisdiction, and courts were themselves bound by deference precedents — while leaving real alternatives short of exit: notice-and-comment participation, congressional override, electoral turnover, and litigation. Theater is moderate (0.36): the intelligible-principle test has invalidated essentially no delegation since 1935, so a growing share of separation-of-powers adjudication is ritual affirmation, though the framework continues to resolve genuine interbranch disputes, and the ratio dips late in the interval as the Court resumed substantive boundary-drawing. Accessibility collapse is low (0.35): the competing readings remain fully litigable, Congress can reclaim authority statute by statute, and the Court demonstrated it can tighten review — alternatives have narrowed but nowhere near collapsed. Resistance is moderate-high (0.55): sustained scholarly critique, recurring judicial opinions pressing stricter boundaries, and legislative proposals to constrain agencies have kept the settlement continuously contested rather than normalized. The temporal series share one seven-point grid (interval years 0-90, anchored at 1935) so every metric is authored at every examined time point. Extractiveness climbs through the expansion of the regulatory state, peaks in the mature deference era, and eases as major-questions doctrine and the withdrawal of the strongest deference vehicle bite. The suppression series is U-shaped and is the story's tracked enforcement dynamic: the settlement required heavy active defense during its contested first decades, relaxed during mid-century consolidation when its legitimacy went largely unchallenged, and has been ratcheting back up as challenges intensified — enforcement capacity rebuilt in response to renewed attack, not extraction shifting.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the agency seat, the arrangement is the condition of its own existence — a coordination framework it staffs and operates, experienced as legitimate structure. From the congressional and presidential seats it is a convenience that exports cost and risk. From the regulated-individual seat it is close to pure subjection: a decisionmaker they never chose, applying rules they could not shape, with no exit. The court seat is genuinely dual-positioned — enforcer and cost-bearer at once — which the engine should register as intermediate directionality rather than collapsing to either pole. The state seat experiences extraction without representation in the allocating bargain. None of these divergences is adjudicated by the authored claim; they fall out of the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: agencies collect authority and deference directly (damped chi, near-subsidy at the extreme), congressional majorities collect risk transfer, presidential administrations collect discretionary levers. Victims derive high directionality, amplified by exit conditions: regulated individuals are trapped (no alternative decisionmaker, no realistic appeal), so they sit nearest the full-target end; regulated industries are constrained but organized, moderating their effective position; states are constrained and outside the bargain; courts are constrained cost-bearers whose enforcement role pulls their derived directionality below that of ordinary payers, which is the honest description of a branch that administers a settlement that taxes its own prerogatives. Scope is national throughout, which modestly amplifies effective extraction for targets by raising verification costs; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing concentration of power — is contested rather than dead: the parties genuinely dispute whether branch concentration or governmental incapacity is the operative danger, and the corroboration record shows outsiders attesting the problem remains live. Because the status is contested rather than dead, the mismatch consumer finds no dead-problem-plus-world-rearranges signature and no zombie flag fires. The arrangement is emphatically not a piton: it has concentrated beneficiaries (agencies whose existence depends on it) who actively maintain it, so the piton cost-asymmetry test fails on the maintenance side. It is equally not a snare: the coordination function is real and independently verifiable — detailed technical regulation does get produced and does solve problems no alternative institution currently solves — so the coordination story is not mere cover. The tangled-rope classification is what prevents both mislabels: it forces the analysis to price the genuine coordination (against treating the whole arrangement as extraction) while keeping the transfer visible (against treating the whole arrangement as benign flexibility). Mandatrophy resolution here is therefore a live-monitoring posture, not a resolved verdict: the omega variables track whether the deference component survives its doctrinal retreat and whether the expertise rationale tracks observable error-correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the functionalist reading of the separation_of_powers_text kernel; what would the sibling readings change structurally if adopted as the governing reading?',
    'Doctrinal displacement rather than data: adoption of the formalist reading would invalidate broad classes of delegation and reclassify the standing arrangement as an illegitimate transfer with sharply higher epsilon; adoption of the unitary-executive reading would strip independent agencies of legitimacy and concentrate removal power in the presidency. Either sibling becomes a separate constraint story with its own referent; this file''s classification holds only within the functionalist reading.',
    'The computed type and epsilon of THIS story are conditional on the functionalist reading remaining the operative settlement. A formalist or unitary victory does not tune these metrics down or up; it replaces the constraint under evaluation with a different one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one of three live readings of a fixed-text kernel; sibling readings instantiate different constraints.').

omega_variable(
    deference_scope_instability,
    'How far does interpretive deference actually extend once its strongest doctrinal vehicle is withdrawn, and does interpretive authority return to courts or migrate informally to agencies?',
    'Track the post-2024 appellate record: rates at which courts adopt agency readings unprompted, rates of reversal of agency interpretations, and whether agencies rewrite rules to litigation-proof specificity. Five to ten years of case-level data resolves the trajectory.',
    'If authority genuinely returns to courts, the extraction component falls toward 0.30 and the arrangement moves toward rope; if deference persists as informal practice, extraction holds near current levels and the enforcement burden stays elevated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_scope_instability, empirical, 'Whether the deference component of the arrangement survives its doctrinal vehicle''s repeal.').

omega_variable(
    accountability_diffusion_valence,
    'Is the diffusion of accountability inherent in delegation a cost the arrangement imposes (extraction) or a designed feature that disperses blame productively (coordination)?',
    'Comparative institutional analysis: measure electoral punishability of regulatory outcomes before and after major delegations, and whether blame dispersion correlates with policy stability valued by voters or with unremovable administrator power.',
    'If diffusion is primarily a designed feature, part of the measured extraction is the price of the coordination itself and epsilon sits at the low end of its band; if it primarily shields administrators from correction, the extraction component is rent and epsilon sits at the high end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_diffusion_valence, conceptual, 'Whether diffuse accountability is a bug the arrangement imposes or a feature it sells.').

omega_variable(
    expertise_or_entrenchment,
    'Do deference doctrines coordinate genuine technical expertise, or do they entrench agency self-interest against correction?',
    'Error-correction audit: compare rates at which agency scientific and economic judgments are later reversed by courts, successor administrations, and the agencies themselves, against comparable reversal rates for judicial and legislative technical judgments.',
    'High self-correction supports the coordination framing and the lower-epsilon reading; low self-correction indicates the deference structure protects the extractor rather than the expertise, pushing the arrangement toward the snare boundary of the tangled-rope band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_or_entrenchment, empirical, 'Whether the expertise rationale for deference tracks observable error-correction performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__functionalist_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(sepa_tr_t45, separation_of_powers_text__functionalist_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__functionalist_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(sepa_tr_t75, separation_of_powers_text__functionalist_reading, theater_ratio, 75, 0.39).
narrative_ontology:measurement(sepa_tr_t90, separation_of_powers_text__functionalist_reading, theater_ratio, 90, 0.36).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__functionalist_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(sepa_be_t45, separation_of_powers_text__functionalist_reading, base_extractiveness, 45, 0.43).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__functionalist_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(sepa_be_t75, separation_of_powers_text__functionalist_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(sepa_be_t90, separation_of_powers_text__functionalist_reading, base_extractiveness, 90, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__functionalist_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(sepa_su_t45, separation_of_powers_text__functionalist_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__functionalist_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(sepa_su_t75, separation_of_powers_text__functionalist_reading, suppression_requirement, 75, 0.51).
narrative_ontology:measurement(sepa_su_t90, separation_of_powers_text__functionalist_reading, suppression_requirement, 90, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one member of a three-story constraint family decomposing the colloquial label 'separation of powers' per the epsilon-invariance principle. The label conflates three structurally distinct claims about the same constitutional text: the formalist reading (strict impermeable boundaries, delegation invalid), this functionalist reading (flexible overlap, intelligible-principle delegation valid), and the unitary-executive reading (all executive power in the President, independent agencies invalid). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification; this file authors only the functionalist instantiation, whose referent is the standing flexible-delegation arrangement assessed by the functionalist reading's own lights. The sibling files carry their own referents and epsilons; no averaging across readings occurs here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
