% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Separation of Powers (Functionalist Reading): Flexible Framework with Intelligible Principle Delegation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the FUNCTIONALIST READING of separation of
 *   powers doctrine: the U.S. Constitution's separation of powers is a
 *   flexible framework permitting overlapping authority, agency independence,
 *   and Congressional delegation of legislative-like authority to agencies
 *   under intelligible principle standards. This reading competes with two
 *   siblings: the formalist reading (which would treat delegation as
 *   constitutionally impermissible and agencies as ultra vires) and the
 *   unitary executive reading (which would subordinate independent agencies
 *   to the President's removal authority). The functionalist reading has been
 *   dominant in constitutional law since Chevron U.S.A., Inc. v. Natural
 *   Resources Defense Council (1984) and the Administrative Procedure Act's
 *   passage in 1946, but faces sustained doctrinal and political challenge.
 *   The constraint's extractiveness is moderate (0.38) because the reading
 *   legitimates a substantial regulatory state that redistributes costs and
 *   benefits, but the extraction is bounded by Congressional check authority,
 *   judicial review, and Presidential oversight rather than unlimited agency
 *   discretion. The claimed type is ROPE because the reading coordinates
 *   Congress, President, and agencies around a workable governance framework;
 *   the metrics reflect that genuine coordination coexists with measurable
 *   extraction and growing theatrical maintenance of the delegation fiction.
 *
 * KEY AGENTS:
 *   - Administrative Agencies: institutional beneficiaries, operate under delegated authority, constrained exit (must be legislatively dissolved)
 *   - Congress: agenda-setter, retains control through statute, appropriations, oversight; arbitrage-grade exit (can recalibrate or withdraw delegation)
 *   - President: agenda-setter and secondary beneficiary, exercises control through appointment, removal, and executive orders; arbitrage exit via constitutional reinterpretation
 *   - Judiciary: beneficiary through Chevron deference and APA review authority; arbitrage exit via doctrinal reinterpretation (e.g., rejection of Chevron)
 *   - Regulated Private Parties: payers bearing compliance costs; constrained exit (some can relocate, others cannot escape regulatory jurisdiction)
 *   - Formalist Doctrinal Community: excluded advocates for strict non-delegation doctrine; mobile exit (can argue before courts, write law review articles, seek judicial appointments)
 *   - Unitary Executive Advocates: excluded advocates for full presidential control; mobile exit (similar to formalists)
 *   - Congressional Oversight Community: beneficiaries whose authority and legitimacy depend on the functionalist frame; constrained exit (oversight authority derives from functional necessity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.38).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.22).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers (Functionalist Reading): Flexible Framework with Intelligible Principle Delegation").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'b4914ddb-99bf-4b18-9493-43f296118970').
narrative_ontology:cs_kernel_codification('b4914ddb-99bf-4b18-9493-43f296118970', fixed_text).
narrative_ontology:cs_authority_grounding('b4914ddb-99bf-4b18-9493-43f296118970', lineage).
narrative_ontology:cs_interpretation_layer_present('b4914ddb-99bf-4b18-9493-43f296118970').
narrative_ontology:cs_reading_relation('b4914ddb-99bf-4b18-9493-43f296118970', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4914ddb-99bf-4b18-9493-43f296118970', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('b4914ddb-99bf-4b18-9493-43f296118970', foundational, separation_of_powers_is_structural_not_categorical).
narrative_ontology:cs_axiom_status(separation_of_powers_is_structural_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('b4914ddb-99bf-4b18-9493-43f296118970', separation_of_powers_is_structural_not_categorical, deontological).
narrative_ontology:cs_axiom('b4914ddb-99bf-4b18-9493-43f296118970', foundational, delegation_under_intelligible_principle_is_permissible).
narrative_ontology:cs_axiom_status(delegation_under_intelligible_principle_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('b4914ddb-99bf-4b18-9493-43f296118970', delegation_under_intelligible_principle_is_permissible, empirically_contingent).
narrative_ontology:cs_reference_frame('b4914ddb-99bf-4b18-9493-43f296118970', flexible_authority_sharing_framework).
narrative_ontology:cs_drift_state('b4914ddb-99bf-4b18-9493-43f296118970', contemporary_originalist_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b4914ddb-99bf-4b18-9493-43f296118970', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress_executive_coordination_apparatus).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulatory_implementation_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, judiciary).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congressional_oversight_community).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_private_parties).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, adaptive_governance_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, delegation_under_intelligible_principle).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, structural_checks_over_categorical_separation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Agencies operate under delegated authority from Congress, filling gaps in legislative specificity through rulemaking, adjudication, and enforcement. The functionalist reading legitimates their existence and broad discretion by treating separation of powers as a flexible framework focused on checking arbitrary power rather than maintaining rigid boundaries. They benefit from the reading because it preserves their legal authority and operational scope in the absence of strict non-delegation doctrine. Exit means agency dissolution, which is structurally difficult without legislative consent.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Congress delegates legislative-like authority to agencies through framework statutes with intelligible principles rather than detailed specifications. The functionalist reading permits this delegation while still claiming constitutional legitimacy for Congress's core legislative role. Congress retains oversight authority and can recalibrate delegation through statute. The reading coordinates Congress and executive agencies rather than foreclosing the arrangement.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Presidents exercise control over agencies through appointment, removal, and executive orders, claiming managerial authority over the executive branch. The functionalist reading permits this while acknowledging independent agencies' statutory protections (e.g., removal restrictions). The president benefits from the reading's flexibility in exercising executive power through agency delegation rather than formal legislative authority, but faces constraints from independent agency status and Congressional oversight.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, president, beneficiary).

% Courts apply Chevron deference (in the functionalist framework) or arbitrary-and-capricious review under the Administrative Procedure Act, treating agency action as legitimate if it rests on intelligible principles and reasoned deliberation. The functionalist reading grants courts a checking function without requiring them to void vast swaths of regulatory activity on non-delegation grounds. Courts benefit from this reading by maintaining a workable role in administrative law.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, judiciary, beneficiary,
    institutional, generational, arbitrage, national).

% Academics, jurists, and advocates holding the formalist reading are excluded from this functionalist framework's preferred legitimacy path. They argue that agencies and delegations violate the original Constitution's structural design. They would advance a different constraint (the formalist reading) if they occupied the interpretive authority seat; their exclusion reflects the current doctrinal consensus favoring functionalism.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_doctrinal_community, excluded,
    organized, generational, mobile, national).

% Scholars and some executive branch officials who hold the unitary executive theory are excluded from the functionalist framework's compromise. They argue that independent agencies violate presidential power. Their argument would push toward a different constraint (unitary_executive_reading) if adopted as the binding interpretation; they remain outside the current consensus.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, unitary_executive_advocates, excluded,
    organized, generational, mobile, national).

% Corporations, individuals, and organizations subject to agency regulation face compliance costs, reporting burdens, and enforcement action. They bear the cost of the regulatory state enabled by the functionalist reading's legitimacy of agency authority. Their exit options are limited (some can relocate or exit markets; others cannot). Some benefit from regulatory protections (environmental, labor, consumer groups); others perceive pure extraction.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_private_parties, payer,
    powerful, biographical, constrained, national).

% Congressional staff, inspectors general, and Government Accountability Office investigators conduct oversight of agencies. The functionalist reading legitimates this oversight activity as a constitutional check on delegated power. They benefit from the reading's framework because it frames their oversight as essential to the separation-of-powers balance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congressional_oversight_community, beneficiary,
    moderate, biographical, constrained, national).

% Legal scholars, historians, and analysts examine the constraint's operation and the divergence between the functionalist reading and competing readings. They take no institutional position on the constraint's legitimacy but track how the reading shapes doctrine, agency behavior, and regulatory stability.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, constitutional_observer_seats, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits Congress and the President to coordinate on governing problems too complex and technically demanding for legislative specificity: Congress sets intelligible principles and policy objectives; agencies operationalize them through expertise-driven rulemaking and enforcement. The coordination solves the problem of translating broad constitutional mandates (e.g., 'protect the environment,' 'regulate interstate commerce') into workable rules without paralyzing government through endless legislative detail.
% TRANSFER_FUNCTION: Transfers implementation authority and discretionary power from Congress to administrative agencies, with presidential oversight authority. The flow also transfers compliance costs onto regulated parties and distributes benefits of regulation (safety, environmental protection, consumer protection) across the public. The functionalist reading authorizes this transfer without requiring legislative specificity that would render many statutes unenforceable.
% ABSENT_VOICES: Formalists arguing for strict non-delegation doctrine and unitary executive theorists arguing for full presidential control over agencies are structurally excluded from the current functionalist consensus. They would contest both the legitimacy of delegation itself and the independence of agencies from presidential removal authority. Indigenous peoples and historical communities bearing the costs of 'regulatory state' benefits are often not present in administrative proceedings designed for formal notice-and-comment. Regulated entities sometimes claim their voices are excluded by agency capture or insufficient participation in rulemaking.
% DISAPPEARANCE_RATIONALE: If the functionalist reading vanished overnight and formalist or unitary executive readings took its place, the Administrative State would face immediate crisis. Thousands of agency rules would face legal challenge as ultra vires delegations or unconstitutional encroachments on presidential power. Environmental Protection Agency, Securities and Exchange Commission, Food and Drug Administration, and other major agencies would lose doctrinal foundation. Congress would either have to abandon vast regulatory domains or write laws so detailed that legislative process would become paralyzed. The world would reorganize around either a drastically reduced administrative state or a shift to unitary presidential control—either way, a radical restructuring of governance.
% FOUNDING_PROBLEM: The Constitution's text grants powers to separate branches but does not specify how to handle modern problems (public health, environmental protection, financial regulation, telecommunications) that did not exist in 1789 and cannot be solved by any single branch acting alone. Strict categorical separation forecloses necessary coordination; delegation under intelligible principles permits expertise-driven governance while preserving presidential oversight and Congressional check authority.
% FOUNDING_PROBLEM_CORROBORATION: The functionalist reading's proponents (administrative law scholars, appellate judges applying Chevron doctrine, agency leadership) attest the founding problem remains live and unsolvable under strict formalism. Formalist jurists (originalist scholars, some Supreme Court justices) contest this, arguing the problem is solved by Congressional specificity and that intelligible principle delegation simply abandons constitutional constraint. Unitary executive advocates contest the presidential oversight framing. Independent scholars from law, history, and political science confirm that modern regulatory problems cannot be solved within strict categorical separation; some also confirm that the functionalist reading has permitted unchecked agency growth not originally contemplated. The corroboration is split—the founding problem's persistence is attested by non-benefiting parties (regulated industries, critics of regulatory expansion), but those parties disagree on whether functionalism solves or exacerbates the problem.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).

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
 *   Extractiveness at 0.38 (moderate) reflects the reading's core compromise: agencies extract discretionary power and compliance costs, but Congress and courts retain checking authority rather than granting unlimited agency rule. The measurement series shows stable extractiveness (0.28→0.40 peak at t=40, then slight decline to 0.38) with a plateau phase (t=50-70) indicating the reading has reached a stable doctrinal equilibrium. Theater ratio (0.18) is relatively low because the functional legitimacy story (expertise-driven governance solving problems that categorical separation cannot) is substantively plausible; theatrical maintenance is modest—the reading genuinely permits workable coordination. However, the rise from 0.12 to 0.20 (t=0 to t=40) and subsequent plateau reflects growing Congressional and public skepticism about agency capture and regulatory overreach, driving increased performative oversight and transparency theater without substantive governance restructuring. Suppression requirement rises from 0.16 to a plateau at 0.22, indicating that enforcing delegation under the functionalist reading requires modest but stable suppression of alternative framings (formalist and unitary executive readings) and constant doctrinal work to prevent their ascendance in courts. The reading's stability (extractiveness and suppression plateau in latter half of interval) suggests it has reached institutional equilibrium—agencies are established, courts apply Chevron deference, Congress legislates within the delegation framework, and the primary tension is rhetorical and political rather than structural.
 *
 * PERSPECTIVAL GAP:
 *   The agency and Congressional seats compute ROPE from their perspective: genuine coordination solving an otherwise intractable governance problem. Regulated parties compute this as SNARE or TANGLED_ROPE from theirs: extraction of compliance costs and regulatory power without offsetting benefits, enabled by the reading's legitimacy of agency discretion. Formalists compute this as SNARE (unconstitutional delegated authority masquerading as coordination). Unitary executive advocates compute this as TANGLED_ROPE (coordination between Congress and agencies that violates Presidential authority). The engine computes a per-seat classification; this divergence is exactly the measurement the apparatus exists to take. The authored claim (rope) and the authored metrics (moderate extraction, low theater) represent the functionalist reading's own self-presentation; the divergence from other seats' computations is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative agencies are beneficiaries (d near 0.2-0.3): they gain institutional legitimacy, budgets, personnel, and implementation authority. Congress and President are partial beneficiaries with secondary extraction (d near 0.4-0.5): they coordinate on governance but also extract authority from each other (Congress delegates to avoid statutory detail; President controls agencies). Judiciary is beneficiary (d near 0.2): gains checking authority without wholesale invalidation of the administrative state. Regulated parties are targets (d near 0.7-0.8): bear compliance costs, constrained exit, identity-locked into regulatory subject positions. The directionality derivation from beneficiary/victim declarations maps cleanly: agencies and oversight institutions are beneficiaries; regulated parties and excluded formalist/unitary advocates are victims. No overrides needed; the structural data yields coherent directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to govern problems the Constitution did not anticipate within a separation-of-powers framework) remains LIVE. The functionalist reading solves it through delegation under intelligible principles; formalists argue it should be solved through legislative detail; unitary executives argue it should be solved through Presidential control. No party claims the founding problem is solved and the reading is now inert (which would signal mandatrophy). However, CONTESTED-status mandatrophy risk exists: as formalist and unitary executive challenges gain traction (especially in recent Supreme Court decisions limiting Chevron deference), the reading's functional legitimacy could erode. The rising theater ratio (t=0 to t=40) hints at this: increased performative oversight without substantive governance change is a symptom of mandatrophy incubation—the reading is sustained by theatrical maintenance rather than genuine functional necessity. Current status: live and contested, not yet mandatrophic, but trajectory warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegated_discretion_boundary,
    'Where is the boundary between permissible agency discretion under intelligible principles and impermissible legislative delegation? Can an agency rewrite a statute''s meaning, or only fill technical gaps?',
    'Supreme Court doctrinal development (post-Chevron: compare Chevron Step One with major-questions doctrine). Compare agency interpretation that extends statutory meaning with interpretation that merely operationalizes existing meaning. Empirical observation of which agency actions courts uphold as legitimate delegation-fulfillment versus which courts void as ultra vires.',
    'A narrow boundary (only technical gaps permitted) would lower ε and shift toward formalism. A wide boundary (agency can substantially reinterpret statutes) would maintain high ε and strengthen functionalist legitimacy. Current doctrine is contested, with some justices pushing toward narrower boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegated_discretion_boundary, empirical, 'Unresolved question of how much interpretive discretion intelligible principle delegation permits.').

omega_variable(
    independent_agency_constitutionality,
    'Are independent agencies (with removal protections limiting Presidential authority) constitutionally legitimate under separation of powers, or do they violate Presidential executive power?',
    'Supreme Court ruling on removal restrictions and Presidential control. Observed compliance with or challenge to independent agency status. Political pressure for or against agency independence in statute drafting.',
    'If independent agencies are ruled unconstitutional, the functionalist reading collapses toward unitary executive reading. ε would rise sharply (less coordination between Congress and President, more Presidential extraction of control). If removal restrictions are upheld, functionalism is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independent_agency_constitutionality, empirical, 'Direct test of functionalist framework''s constitutional validity regarding agency independence.').

omega_variable(
    intelligible_principle_revival,
    'Will courts revive non-delegation doctrine to strike down statutes as failing the intelligible principle test, or will intelligible principle remain a nearly-toothless standard?',
    'Supreme Court invalidation of federal statute on non-delegation grounds (has not occurred since 1935). Legislative or judicial reaction to major-questions doctrine as a quasi-non-delegation check. Originalist judicial expansion of delegation constraints.',
    'Revival of non-delegation doctrine would foreclose the functionalist reading entirely—agencies would lose doctrinal legitimacy, and statutes would be required to specify detailed rules. This would shift the constraint toward the formalist reading, sharply lowering ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_revival, empirical, 'Whether non-delegation doctrine returns as a limiting principle on agency authority.').

omega_variable(
    agency_capture_measurement,
    'To what extent does the regulatory state systematically benefit regulated industries (regulatory capture) versus serving public interest? Does the functionalist reading''s promise of coordination actually deliver neutral expertise-driven governance?',
    'Empirical study of agency rulemaking outputs: do regulations reflect Congressional intent, agency professional judgment, or industry preferences? Revolving-door employment patterns. Campaign finance flows to politicians shaping agency statute. Post-hoc economic analysis of regulatory benefits and costs distribution.',
    'High capture would reframe the constraint from ROPE (coordination) to SNARE (extraction by captured agencies on behalf of regulated industry). Would elevate ε and suppress alternative readings. Would suggest mandatrophy: the reading persists because it legitimates agency capture, not because coordination requires it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capture_measurement, empirical, 'Whether the administrative state serves neutral expertise-driven governance or industry interests.').

omega_variable(
    formalist_vs_functionalist_kernel_interpretability,
    'Is the separation-of-powers text genuinely ambiguous between formalist and functionalist readings, or does the text more clearly support one reading than the other?',
    'Constitutional historian analysis of founding-era intent, textual exegesis, and originalist jurisprudence. Comparison of textual arguments each side deploys. Assessment by neutral constitutional scholars.',
    'If the text more clearly supports formalism, the functionalist reading is vulnerable to claims of misreading the kernel. If the text is genuinely ambiguous, both readings are defensible as interpretations of the same material. Would affect the legitimacy standing of each reading but not its structural operation (ε remains the same under either finding).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formalist_vs_functionalist_kernel_interpretability, conceptual, 'Whether separation-of-powers text permits or forecloses functionalist interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(sepa_tr_t0, projected).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__functionalist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(sepa_tr_t10, observed).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__functionalist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(sepa_tr_t20, observed).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(sepa_tr_t30, observed).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__functionalist_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(sepa_tr_t40, observed).
narrative_ontology:measurement(sepa_tr_t50, separation_of_powers_text__functionalist_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(sepa_tr_t50, observed).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__functionalist_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(sepa_tr_t60, observed).
narrative_ontology:measurement(sepa_tr_t70, separation_of_powers_text__functionalist_reading, theater_ratio, 70, 0.18).
narrative_ontology:measurement_basis(sepa_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(sepa_be_t0, projected).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__functionalist_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(sepa_be_t10, observed).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__functionalist_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(sepa_be_t20, observed).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(sepa_be_t30, observed).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__functionalist_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement_basis(sepa_be_t40, observed).
narrative_ontology:measurement(sepa_be_t50, separation_of_powers_text__functionalist_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement_basis(sepa_be_t50, observed).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__functionalist_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(sepa_be_t60, observed).
narrative_ontology:measurement(sepa_be_t70, separation_of_powers_text__functionalist_reading, base_extractiveness, 70, 0.38).
narrative_ontology:measurement_basis(sepa_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(sepa_su_t0, projected).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__functionalist_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(sepa_su_t10, observed).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__functionalist_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(sepa_su_t20, observed).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(sepa_su_t30, observed).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__functionalist_reading, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(sepa_su_t40, observed).
narrative_ontology:measurement(sepa_su_t50, separation_of_powers_text__functionalist_reading, suppression_requirement, 50, 0.23).
narrative_ontology:measurement_basis(sepa_su_t50, observed).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__functionalist_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(sepa_su_t60, observed).
narrative_ontology:measurement(sepa_su_t70, separation_of_powers_text__functionalist_reading, suppression_requirement, 70, 0.22).
narrative_ontology:measurement_basis(sepa_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, presidential_removal_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the separation-of-powers kernel. The formalist reading (separation_of_powers_text__formalist_reading) interprets the same constitutional text as prohibiting legislative delegation and requiring strict categorical separation. The unitary executive reading (separation_of_powers_text__unitary_executive_reading) interprets it as concentrating all executive power in the President. All three readings share the same kernel (the constitutional text) but have structurally distinct ε values, beneficiary/victim sets, and institutional implications. The functionalist reading has lower ε (moderate extraction via coordination framework) and broader beneficiaries (agencies, Congress, courts, regulatory communities). The formalist reading would have higher ε (unconstitutional delegated extraction). The unitary executive reading would concentrate extraction in the Presidency. The three constraints form a family linked by network effects: each reading's institutional dominance affects the others' structural operating conditions. The functionalist reading currently dominates doctrine (Chevron deference, APA framework), but formalist originalism and unitary executive theory create competitive pressure. Failure of the functionalist reading (e.g., Supreme Court adoption of non-delegation doctrine or unitary executive principle) would shift institutional equilibrium toward a sibling reading's constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
