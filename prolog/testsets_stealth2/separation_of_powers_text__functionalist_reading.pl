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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Separation of Powers — Functionalist Reading (Flexible Framework and Intelligible-Principle Delegation)
 *   domain: constitutional law/political theory/administrative law
 *
 * SUMMARY:
 *   This story instantiates the functionalist reading of the
 *   separation-of-powers kernel: the operative constitutional framework under
 *   which overlapping authority among the branches is permissible and
 *   Congress may delegate rulemaking authority to agencies so long as it
 *   enunciates an intelligible principle. The extractiveness referent is the
 *   standing arrangement under contest — the modern administrative state
 *   built on delegation and overlap — assessed by this reading's own lights:
 *   the reading sees the arrangement as substantially legitimate coordination
 *   (deference doctrines divide interpretive labor among Congress, agencies,
 *   and courts) carrying bounded, acknowledged costs (accountability
 *   diffusion, capture risk). Values are reading-indexed over that fixed
 *   referent; the formalist sibling file assesses the same arrangement under
 *   different lights and authors substantially higher extractiveness. Claim
 *   and metrics are authored independently: this reading claims a hybrid
 *   structure — genuine coordination function plus identifiable asymmetric
 *   costs, actively policed by courts — and the metrics describe that
 *   operation descriptively. Assumptions stated: the interval opens at 1937
 *   (the functionalist consolidation; the intelligible-principle test itself
 *   dates to 1928) and closes at 2025 (post-Loper Bright, post-Jarkesy).
 *
 * KEY AGENTS:
 *   - supreme_court: agenda-setting seat — defines the framework's content through doctrine (Mistretta, Chadha, Bowsher, Gundy, Jarkesy); pays enforcement costs, collects arbitral authority; near-symmetric position
 *   - congressional_majorities: primary beneficiary seat — enacts broad statutes, claims credit for aims, shifts blame for administrative detail; pays by ceding policy control
 *   - presidential_administrations: beneficiary seat — directs the administrative apparatus under flexible boundaries; horizon bounded by electoral cycles
 *   - administrative_agencies: identity-fused beneficiary seat — delegated authority constitutes the agency; exit from the arrangement would dissolve it
 *   - national_electorate: primary payer seat — bears diffused accountability; cannot trace administrative choices to an answerable principal; no exit from the constitutional order
 *   - regulated_industries: payer seat — bear compliance costs and multi-agency overlap; partially recoup through participation channels and occasional capture
 *   - state_governments: excluded seat — governed extensively by federal administrative action with essentially no voice in the inter-branch settlement
 *   - constitutional_law_scholars: analytical observer — maps the formalist/functionalist/unitary contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.32).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.28).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers — Functionalist Reading (Flexible Framework and Intelligible-Principle Delegation)").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional law/political theory/administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'ec4f6054-35f7-463f-a00f-9de61cac9656').
narrative_ontology:cs_kernel_codification('ec4f6054-35f7-463f-a00f-9de61cac9656', fixed_text).
narrative_ontology:cs_authority_grounding('ec4f6054-35f7-463f-a00f-9de61cac9656', lineage).
narrative_ontology:cs_interpretation_layer_present('ec4f6054-35f7-463f-a00f-9de61cac9656').
narrative_ontology:cs_reading_relation('ec4f6054-35f7-463f-a00f-9de61cac9656', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec4f6054-35f7-463f-a00f-9de61cac9656', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('ec4f6054-35f7-463f-a00f-9de61cac9656', foundational, separation_is_flexible_equilibrium).
narrative_ontology:cs_axiom_status(separation_is_flexible_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('ec4f6054-35f7-463f-a00f-9de61cac9656', separation_is_flexible_equilibrium, instrumental).
narrative_ontology:cs_axiom('ec4f6054-35f7-463f-a00f-9de61cac9656', foundational, intelligible_principle_suffices_for_delegation).
narrative_ontology:cs_axiom_status(intelligible_principle_suffices_for_delegation, holdable).
narrative_ontology:cs_axiom_grounding('ec4f6054-35f7-463f-a00f-9de61cac9656', intelligible_principle_suffices_for_delegation, conventional).
narrative_ontology:cs_axiom('ec4f6054-35f7-463f-a00f-9de61cac9656', secondary, administrative_agencies_are_legitimate_governors).
narrative_ontology:cs_axiom_status(administrative_agencies_are_legitimate_governors, holdable).
narrative_ontology:cs_axiom_grounding('ec4f6054-35f7-463f-a00f-9de61cac9656', administrative_agencies_are_legitimate_governors, instrumental).
narrative_ontology:cs_reference_frame('ec4f6054-35f7-463f-a00f-9de61cac9656', flexible_equilibrium_framework).
narrative_ontology:cs_drift_state('ec4f6054-35f7-463f-a00f-9de61cac9656', post_chevron_major_questions_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ec4f6054-35f7-463f-a00f-9de61cac9656', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congressional_majorities).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, presidential_administrations).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, national_electorate).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, congressional_majorities).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, mistretta_functionalist_line).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, judicial_deference_doctrines).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which inter-branch arrangements the Constitution tolerates: it articulates the flexible-equilibrium standard, strikes arrangements that aggrandize one branch at another's expense (the legislative veto in Chadha, one-house vetoes), and polices the line between permissible delegation and self-aggrandizement. It spends adjudicative resources enforcing the framework and collects interpretive authority as the framework's arbiter. Its exit would be abandoning functional balancing for categorical rules — possible only at the cost of overturning a century of precedent.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Enact broad statutory frameworks and delegate rulemaking detail to agencies. The arrangement lets them claim credit for statutory aims while administrative failures attach to the agencies and the President; they pay by ceding control of policy detail, by oversight burdens, and by watching executive agencies implement statutes in ways the enacting majority did not anticipate. Exit would mean writing the detail themselves — feasible for any single statute, prohibitive as a general practice.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congressional_majorities, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congressional_majorities, payer).

% Direct the administrative apparatus under boundaries flexible enough to shape national rulemaking through appointments, executive orders, and supervision. They gain the capacity to move policy through agencies without new statutes; they pay when courts trim their reach or when Congress reclaims detail. Exit would mean forgoing the administrative lever — costly to any administration's agenda.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, presidential_administrations, beneficiary,
    institutional, biographical, constrained, national).

% Exist as the recipients of delegated authority: their mandates, budgets, and expert staffs are constituted by the delegations the framework permits. They gain policy autonomy and institutional persistence across administrations; they pay in litigation risk, political oversight, and the standing threat that a court will read their authorizing statute narrowly. Leaving the arrangement would dissolve the agency itself — its identity is its delegated function.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, identity_locked, national).

% Bears the accountability cost of delegation: administrative choices are made by bodies no one voted for directly, under statutes whose vagueness was chosen by majorities seeking flexibility. Voters can reach the President every four years and members of Congress every two, but the chain from a specific administrative rule to an answerable principal is long and frequently broken. There is no exit from the constitutional order that allocates power this way.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, national_electorate, payer,
    moderate, biographical, trapped, national).

% Bear compliance costs, multi-agency overlap, and the uncertainty of rules that can shift with each administration. They recoup part of the burden through notice-and-comment participation, lobbying, and occasionally by capturing the agencies that regulate them; the remainder is a net cost they cannot avoid while operating in the national market.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_industries, payer,
    powerful, biographical, constrained, national).

% Are governed extensively by federal administrative action — preempted fields, conditional-spending terms, mandated implementation of federal programs — but hold essentially no seat in the inter-branch settlement that allocates the power exercised over them. They litigate at the margins and lobby through political channels; the framework itself was designed without a state voice.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, state_governments, excluded,
    institutional, generational, constrained, regional).

% Map the contest among the formalist, functionalist, and unitary-executive readings of the constitutional text; they collect neither the arrangement's benefits nor its costs, and hold an analytical seat from which the full structure is visible.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, congressional_majorities).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides the labor of governing a complex continental society among the branches: Congress sets statutory aims and boundaries, expert agencies supply the rule detail Congress cannot specify, the President directs execution, and courts police the boundaries functionally. Deference doctrines coordinate who interprets what, so the branches and agencies produce coherent governance without requiring Congress to legislate every rule.
% TRANSFER_FUNCTION: Moves rulemaking discretion from Congress to agencies and to the executive that directs them; moves accountability for administrative choices from Congress to agencies and the President; moves compliance costs and regulatory uncertainty onto regulated parties and, through prices, the public. Credit moves toward enacting majorities; blame moves toward the administrators who implement.
% ABSENT_VOICES: State governments bear extensive federal administrative governance — preemption, conditional spending, mandated implementation — with essentially no seat in the inter-branch settlement that allocates the power governing them. The national electorate is present only episodically through elections and has no institutional voice in the framework's design. Both would demand a formal accountability chain and a guaranteed state voice in the allocation.
% DISAPPEARANCE_RATIONALE: If the flexible framework vanished overnight and impermeable boundaries took its place, the administrative state would lose its constitutional foundation: agencies could hold no delegated rulemaking authority, thousands of programs — environmental, securities, labor, health — would lose their operative rules, and Congress would face the impossible task of specifying every rule itself. The world would rearrange around either a drastically smaller federal government or a formally rebuilt one; no stakeholder's arrangements survive unchanged.
% FOUNDING_PROBLEM: The New Deal crisis: whether an industrial, continental society could be governed at all within strictly separated powers, given that a part-time legislature cannot specify rules for a complex economy. The flexible framework was built to reconcile the administrative state with a constitutional text that assigns legislative power to Congress.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the beneficiary set document the 1937 crisis and the framework's origin in it; formalist dissenters — the seats that most contest the arrangement — attest that the underlying problem (complexity outstripping legislative capacity) is real while disputing the flexible solution; the persistence of the complexity itself is uncontested across the literature. No party with standing in the debate denies that the founding problem exists; what is disputed is whether this arrangement remains a defensible answer to it.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

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
 *   Metrics are authored descriptively for the standing arrangement as the functionalist reading assesses it. Extractiveness 0.32: the framework's costs are real but bounded and acknowledged within the reading — accountability diffusion to voters, compliance costs shifted to regulated parties, capture risk — against a coordination function (division of governing labor) the reading regards as genuinely necessary. Suppression 0.28: the framework's coercive apparatus is adjudication, not administration — it excludes rival constitutional theories from operative law (nondelegation challenges fail; Chadha struck the legislative veto) while leaving those theories fully live in discourse; suppression is authored as a raw structural property and is not scaled by power or scope. Theater 0.30: the intelligible-principle test has been announced but never enforced against a delegation since 1935 — pure doctrinal theater — while functional balancing (Chadha, Bowsher, removal cases) does real work; roughly a third of the framework's operative activity is performative. Accessibility collapse 0.28: understanding the framework does not foreclose the formalist or unitary alternatives — both are litigated constantly and recently won ground. Resistance 0.55: formalist and unitary-executive coalitions contest the framework in every branch, and the current Court majority is actively trimming it. The measurement series run on one shared eight-point grid (1937–2025) with all three metrics authored at every point; the series show extraction and theater rising with the administrative state through 2015 and declining after 2020 as the major-questions doctrine, Jarkesy, and Loper Bright trim the framework's reach. Coordination type enforcement_mechanism: the framework is a governance structure maintained by dedicated adjudicative enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the congressional seat the framework is credit-and-blame management — a tool that lets majorities govern without owning administrative failure. From the electorate's seat the same framework is untraceable government — power exercised by principals no one elected, answerable only through long and broken chains. From the Court's seat it is a workable adjudicative equilibrium it administers at real cost and for real authority. From an agency's seat it is the condition of its own existence. All four actors hold institutional-level power; what differentiates their experience is exit structure and role, not global standing — the electorate is trapped in the constitutional order, agencies are fused to their delegated mandates, Congress could exit only at prohibitive cost, and the Court can shift frameworks only by overturning its own precedent.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (congressional_majorities, presidential_administrations, administrative_agencies) derive directionality near the beneficiary end; agencies sit deepest because their exit is identity-locked — the delegated mandate constitutes them, so no arbitrage exists. Declared payers (national_electorate, regulated_industries) derive directionality near the target end; the electorate sits at the extreme because exit from the constitutional order is impossible, while regulated industries sit slightly back because participation channels and occasional capture recoup part of what they bear. The supreme_court seat is near-symmetric: it pays enforcement costs and collects arbitral authority in roughly equal measure. No directionality overrides are used — the derivation from declared roles, power atoms, and exit options produces the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problem — governing complexity through delegation — is live, so the arrangement is not mandatrophy-resolved, and the tangled_rope classification keeps both faces visible: the genuine coordination function that formalist critics would erase, and the asymmetric costs that pure-coordination readings would deny. The atrophy that does exist is component-level: the intelligible-principle test has decayed into announced-but-unenforced theater, and the theater_ratio series tracks exactly that decay and its partial post-2020 reversal. If the founding problem ever dissolved — a simplification of governance no one anticipates — the framework would persist by inertia; the theater series is the early-warning channel for that drift. The recent formalist pushback is contestation, not atrophy: the framework is being trimmed by an active rival reading, which is the signature of a contested hybrid, not a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel separation_of_powers_text — what would the sibling readings change structurally, and where is the disagreement located?',
    'Adoption of a sibling reading as operative doctrine: the formalist_reading would render broad delegation unconstitutional and strip the arrangement of legitimacy; the unitary_executive_reading would subordinate or dismantle independent agency authority. The disagreement is located in whether the constitutional text fixes impermeable boundaries or establishes a flexible functional equilibrium.',
    'Under formalist adoption the standing arrangement''s authored extractiveness would be far higher (unauthorized aggregation of power); under unitary adoption the independence structure becomes the contested element. This file''s values are valid only within the functionalist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer frame: one reading of the separation-of-powers kernel; sibling readings are separate constraints.').

omega_variable(
    epsilon_reading_indexed_referent,
    'The extractiveness value is authored from the functionalist seat over the fixed referent of the standing administrative arrangement — how much of the authored value is the reading''s index rather than the arrangement''s structure?',
    'Cross-reading comparison: compile the formalist and unitary-executive files for the same arrangement and diff the per-seat classifications; the spread across readings over one referent isolates the reading-index component.',
    'A large cross-reading spread would mean separation-of-powers classification is reading-indexed throughout the family; a small spread would mean the arrangement''s structure dominates and the readings differ only at the margins.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_indexed_referent, conceptual, 'Reading-indexed epsilon over a fixed referent (OQ-26 pattern).').

omega_variable(
    accountability_diffusion_magnitude,
    'How much accountability do voters actually lose under delegation, given that presidential elections remain a traceable principal for agency action?',
    'Political-science attribution studies: do voters punish presidents for administrative failures, and does blame attach to enacting congressional majorities for vague statutes?',
    'If presidential accountability largely restores traceability, the electorate''s effective position moves toward the beneficiary end and the authored extractiveness is too high; if diffusion is severe, the extraction borne by voters is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_diffusion_magnitude, empirical, 'Magnitude of the accountability transfer borne by the electorate under delegation.').

omega_variable(
    intelligible_principle_revivability,
    'Can the intelligible-principle test be revived as enforced review (as both the Gundy plurality and dissent gestured toward), or is it permanently theatrical?',
    'Watch for a Court majority actually invalidating a delegation on intelligible-principle grounds, or a Gundy-style plurality hardening into a stricter doctrine.',
    'Revival would collapse the test''s theatrical component into real review, lowering the theater ratio and trimming the framework''s tolerated extraction; permanent theater would push that component toward inertial persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_revivability, empirical, 'Whether the delegation test is revivable or permanently performative.').

omega_variable(
    post_chevron_trajectory,
    'Do Loper Bright, Jarkesy, and the major-questions line represent a durable migration toward the formalist sibling''s structure, or a rebalancing inside the functionalist frame?',
    'Track whether the Court applies categorical boundary rules (formalist signature) or continues functional balancing with tighter review across the coming decade of separation-of-powers cases.',
    'Durable migration would shift this constraint''s family position — the functionalist reading would lose operative force and the formalist file''s classification would become the live one; rebalancing leaves this file''s structure intact with a lower extraction ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_chevron_trajectory, empirical, 'Direction and durability of the contemporary rollback of deference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sopt_functionalist_tr_t1937, separation_of_powers_text__functionalist_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t1937, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t1955, separation_of_powers_text__functionalist_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t1955, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t1973, separation_of_powers_text__functionalist_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t1973, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t1991, separation_of_powers_text__functionalist_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t1991, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t2005, separation_of_powers_text__functionalist_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t2005, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t2015, separation_of_powers_text__functionalist_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t2015, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t2020, separation_of_powers_text__functionalist_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t2020, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t2025, separation_of_powers_text__functionalist_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sopt_functionalist_be_t1937, separation_of_powers_text__functionalist_reading, base_extractiveness, 1937, 0.22).
narrative_ontology:measurement_basis(sopt_functionalist_be_t1937, observed).
narrative_ontology:measurement(sopt_functionalist_be_t1955, separation_of_powers_text__functionalist_reading, base_extractiveness, 1955, 0.26).
narrative_ontology:measurement_basis(sopt_functionalist_be_t1955, observed).
narrative_ontology:measurement(sopt_functionalist_be_t1973, separation_of_powers_text__functionalist_reading, base_extractiveness, 1973, 0.3).
narrative_ontology:measurement_basis(sopt_functionalist_be_t1973, observed).
narrative_ontology:measurement(sopt_functionalist_be_t1991, separation_of_powers_text__functionalist_reading, base_extractiveness, 1991, 0.33).
narrative_ontology:measurement_basis(sopt_functionalist_be_t1991, observed).
narrative_ontology:measurement(sopt_functionalist_be_t2005, separation_of_powers_text__functionalist_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement_basis(sopt_functionalist_be_t2005, observed).
narrative_ontology:measurement(sopt_functionalist_be_t2015, separation_of_powers_text__functionalist_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement_basis(sopt_functionalist_be_t2015, observed).
narrative_ontology:measurement(sopt_functionalist_be_t2020, separation_of_powers_text__functionalist_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement_basis(sopt_functionalist_be_t2020, observed).
narrative_ontology:measurement(sopt_functionalist_be_t2025, separation_of_powers_text__functionalist_reading, base_extractiveness, 2025, 0.32).
narrative_ontology:measurement_basis(sopt_functionalist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(sopt_functionalist_su_t1937, separation_of_powers_text__functionalist_reading, suppression_requirement, 1937, 0.2).
narrative_ontology:measurement_basis(sopt_functionalist_su_t1937, observed).
narrative_ontology:measurement(sopt_functionalist_su_t1955, separation_of_powers_text__functionalist_reading, suppression_requirement, 1955, 0.25).
narrative_ontology:measurement_basis(sopt_functionalist_su_t1955, observed).
narrative_ontology:measurement(sopt_functionalist_su_t1973, separation_of_powers_text__functionalist_reading, suppression_requirement, 1973, 0.3).
narrative_ontology:measurement_basis(sopt_functionalist_su_t1973, observed).
narrative_ontology:measurement(sopt_functionalist_su_t1991, separation_of_powers_text__functionalist_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement_basis(sopt_functionalist_su_t1991, observed).
narrative_ontology:measurement(sopt_functionalist_su_t2005, separation_of_powers_text__functionalist_reading, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement_basis(sopt_functionalist_su_t2005, observed).
narrative_ontology:measurement(sopt_functionalist_su_t2015, separation_of_powers_text__functionalist_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement_basis(sopt_functionalist_su_t2015, observed).
narrative_ontology:measurement(sopt_functionalist_su_t2020, separation_of_powers_text__functionalist_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement_basis(sopt_functionalist_su_t2020, observed).
narrative_ontology:measurement(sopt_functionalist_su_t2025, separation_of_powers_text__functionalist_reading, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(sopt_functionalist_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'separation of powers' decomposes into at least three structurally distinct readings of one kernel (the constitutional allocation of power among branches). This file is the functionalist reading — flexible equilibrium, delegation permissible, extractiveness authored low-moderate over the standing administrative arrangement by this reading's own lights. The formalist sibling authors high extractiveness for the same referent (impermeable boundaries, delegation illegitimate); the unitary-executive sibling targets the independence structure specifically. Each file is epsilon-invariant within its reading; the family links make the cross-reading spread measurable. Structural edges: the functionalist reading's institutional success built the administrative state that defines the unitary sibling's operating environment (influences); the formalist sibling coexists as a live judicial coalition applied in different doctrinal domains (coexists_with).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
