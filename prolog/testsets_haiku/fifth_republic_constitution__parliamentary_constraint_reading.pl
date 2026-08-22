% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitution: Parliamentary Constraint on Executive Authority
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic Constitution (1958) establishes a dual executive: a
 *   directly-elected president with reserved domain powers (foreign policy,
 *   defense, emergency decrees) and a prime minister appointed by the
 *   president but requiring and retaining the confidence of the National
 *   Assembly. This constraint story instantiates the parliamentary-constraint
 *   reading: the constitutional structure requires that the president's
 *   day-to-day governance and major legislation be coordinated with and
 *   authorized by the legislative majority. When the Assembly opposes the
 *   president's policy preferences, the president faces cohabitation — ceding
 *   the prime minister choice and policy direction to the legislature, or
 *   attempting executive overreach with constitutional crisis risk. The
 *   reading asserts that the constraint's core function is democratic: the
 *   requirement for legislative authorization distributes power, prevents
 *   unilateral executive governance, and grounds legitimacy in a broader
 *   consent structure. This reading is one of three contested interpretations
 *   of the same constitutional text (the kernel); the sibling readings —
 *   hyper-presidential and cohabitation-equilibrium — frame the same
 *   constitutional language differently.
 *
 * KEY AGENTS:
 *   - legislative_majority: institutional beneficiary of the authorization requirement; controls prime minister appointment and government confidence
 *   - president_when_opposed: institutional payer when Assembly withholds support; faces cohabitation constraint; constrained exit (must remain within constitutional bounds)
 *   - prime_minister: mediator between presidential powers and legislative oversight; appointed by president but answerable to Assembly
 *   - constitutional_judicial_review: safeguard ensuring legislative authorization enforcement; structural beneficiary
 *   - opposition_parties: payers when out of majority; mobile exit (electoral competition); flip to beneficiaries if they capture the Assembly
 *   - constitutional_architects: analytical observer; framers' intent grounds this reading's legitimacy claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.31).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.19).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution: Parliamentary Constraint on Executive Authority").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '6a654306-8542-429a-9f0a-b63c56402d1b').
narrative_ontology:cs_kernel_codification('6a654306-8542-429a-9f0a-b63c56402d1b', formalized).
narrative_ontology:cs_authority_grounding('6a654306-8542-429a-9f0a-b63c56402d1b', lineage).
narrative_ontology:cs_interpretation_layer_present('6a654306-8542-429a-9f0a-b63c56402d1b').
narrative_ontology:cs_reading_relation('6a654306-8542-429a-9f0a-b63c56402d1b', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a654306-8542-429a-9f0a-b63c56402d1b', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('6a654306-8542-429a-9f0a-b63c56402d1b', foundational, democratic_coordination_requirement).
narrative_ontology:cs_axiom_status(democratic_coordination_requirement, holdable).
narrative_ontology:cs_axiom_grounding('6a654306-8542-429a-9f0a-b63c56402d1b', democratic_coordination_requirement, deontological).
narrative_ontology:cs_axiom('6a654306-8542-429a-9f0a-b63c56402d1b', foundational, legislative_authorization_for_governance).
narrative_ontology:cs_axiom_status(legislative_authorization_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('6a654306-8542-429a-9f0a-b63c56402d1b', legislative_authorization_for_governance, conventional).
narrative_ontology:cs_reference_frame('6a654306-8542-429a-9f0a-b63c56402d1b', dual_executive_with_legislative_oversight).
narrative_ontology:cs_drift_state('6a654306-8542-429a-9f0a-b63c56402d1b', contemporary_fractured_assembly_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6a654306-8542-429a-9f0a-b63c56402d1b', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_judicial_review).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_when_opposed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, opposition_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the appointment of the Prime Minister, can dismiss the government via no-confidence vote, and must authorize major legislation and constitutional amendments. Benefits from the constitutional arrangement's requirement that executive power align with legislative consent. Can impose its policy agenda by withholding support or confidence.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    institutional, generational, analytical, national).

% Holds formal executive powers (commander-in-chief, treaty ratification, emergency decree capacity) but cannot implement major policy or sustain government without legislative majority support. When the Assembly opposes the president's policy preferences, the president faces cohabitation dynamics: cede prime ministerial choice and policy direction to the legislature, or attempt executive overreach with constitutional crisis risk. Exit from the constraint means unconstitutional seizure of power — a cost so high it functions as entrapment.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president_when_opposed, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, president_when_opposed, agenda_setter).

% Appointed by the president but requires and retains the confidence of the legislative majority to govern. Mediates between the president's constitutional powers and the legislature's control over the government's day-to-day operation and policy execution. Subject to recall by the Assembly, making legislative alignment the primary constraint on executive action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).

% The Constitutional Council can review legislation before enactment and certify constitutional conformity. Acts as a structural safeguard maintaining the constraint by invalidating executive attempts to circumvent legislative authorization requirements. Exists as an institutional beneficiary in the sense that the constraint's persistence depends on its willingness to enforce constitutional limits.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_judicial_review, beneficiary,
    institutional, generational, analytical, national).

% When not holding the legislative majority, face constraints on executive power they cannot directly control. Must appeal to electoral outcomes and public opinion to shift the majority balance. Have exit in the form of electoral campaigns and coalition-building; are not structurally trapped. Can become the legislative majority and flip from payer to beneficiary.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, opposition_parties, payer,
    moderate, generational, mobile, national).

% The framers of the Fifth Republic Constitution intended a mixed system: a strong presidency with reserved domain powers, but subordination of day-to-day governance to parliamentary confidence. Their framing, as interpreted by this reading, grounds the legitimacy claim that executive power must be coordinated with legislative authorization.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_architects, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures executive policy implementation must be authorized and sustained by legislative majority consent, preventing unilateral executive governance and distributing power between elected branches. Solves the collective-action problem of preventing any single branch from dominating the system unilaterally.
% TRANSFER_FUNCTION: Transfers de facto policy-setting authority from the executive to the legislative majority whenever they diverge. When president and legislature align, executive agency is amplified; when they oppose, legislature redirects executive implementation to legislative priorities or forces cohabitation compromise.
% ABSENT_VOICES: Anti-democratic forces seeking to abolish legislative authorization requirements are excluded from the constitutional conversation by the constraint itself — they would argue for presidentialist supremacy but are prevented from effective advocacy by the structural requirement that major changes require super-majorities or constitutional amendment. Extra-constitutional actors (the military, security services) are also absent as formal parties, though their implicit power to interrupt the constraint looms.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if the president could implement policy without legislative authorization, appoint prime ministers unilaterally, and sustain governments without assembly confidence — the Fifth Republic's constitutional order would collapse into presidentialism. Power distribution would concentrate in the executive; the assembly would become ceremonial; the separation-of-powers foundation would be destroyed.
% FOUNDING_PROBLEM: The Fourth Republic suffered from executive instability, cabinet crises, and immobilizing party fragmentation. Citizens sought a strong executive capable of decisive action, but haunted by Vichy memories, they also feared concentrated power. The solution was a dual structure: a strong president with reserved powers (foreign policy, defense, emergency), but executive day-to-day governance coordinated with and dependent on legislative confidence.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-government scholars outside the beneficiary set attest that legislative constraint remains essential to prevent democratic backsliding: France's own cohabitation history (1986–1988, 1993–1995, 1997–2002) demonstrates the mechanism's persistence. The Constitutional Council's enforcement of legislative authorization requirements is documented in constitutional jurisprudence independent of government claims. International democratic-monitoring organizations (European Commission, Venice Commission) affirm the constraint as a democratic safeguard structure.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 endpoint, ranging 0.18–0.38 over interval) because the constraint's primary function is democratic coordination, not unilateral extraction: the legislative majority captures authorization-gate authority, but this is a design feature, not an externality. The measurement series shows a rise from 0.18 (early interval, 1958–1975) to 0.38 (peak at t=50, ~1995–2000, corresponding to cohabitation-phase institutional learning), then a decline to 0.31 by endpoint, reflecting normalization after the 2000 constitutional amendment shifting the election cycle. Theater_ratio rises from 0.08 to 0.28 (peak at t=50, cohabitation era when constitutional brinkmanship was most performative), then stabilizes at 0.22, indicating that a modest but steady share of enforcement activity is theatrical posturing rather than substantive legislative-executive negotiation. Suppression is consistently low (0.12–0.24), reflecting the constraint's reliance on constitutional legitimacy and electoral incentives rather than coercion: the legislature withholds confidence or blocks legislation through constitutional procedure, not extrajudicial force. Accessibility_collapse is high (0.78): once the legislative authorization requirement is understood as constitutional law, alternatives (unilateral executive governance, parliamentary abolition) become effectively inaccessible without constitutional amendment or democratic breach. Resistance is moderate-to-high (0.64): the constraint meets real resistance from presidents seeking greater autonomy and from executives frustrated by legislative checks, but the resistance operates within constitutional bounds (litigation, constitutional interpretation disputes, rhetorical appeals) rather than through violent resistance or democratic illegitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The legislative-majority seat and the president-when-opposed seat experience sharply different types. From the legislative seat, the constraint is genuine coordination with embedded beneficiary status: the legislative majority authorized the structure (through electoral support for the constitutional order) and captures the gate (prime minister appointment, confidence vote control). From the president's seat (when opposed), the constraint is a binding limit: executive agency is curtailed, policy implementation must be negotiated, and cohabitation is a form of shared governance the president did not choose. The engine computes both perspectives from the structural data: beneficiary role + control over authorization → low d (legislative seat); payer role + constrained exit → high d (presidential seat when opposed). The claim that this is a rope (genuine coordination) is sustained when the president's party holds the Assembly majority; it appears as tangled_rope when cohabitation forces both branches to coordinate against their preference, extracting compromise at cost to both. This reading frames the constraint as fundamentally a rope because the coordination function (preventing unilateral executive governance) serves the democratic system's stability, not an institutional beneficiary's private interest.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority benefits from the authorization gate: they control prime minister appointment, can dismiss governments via no-confidence, and can block legislation. Their directionality is low (d~0.1–0.2, beneficiary end). The president, when opposed by the Assembly, bears the constraint's costs: policy cannot be implemented unilaterally, the prime minister must be drawn from the majority coalition, and any executive overreach risks constitutional crisis. Directionality for opposed-president is high (d~0.75–0.85, target end). When the president's party holds the Assembly majority, directionality is symmetric (d~0.5) because legislative authorization is coordinated rather than coercive — the president controls both branches. Opposition parties, when out of power, have moderate directionality (d~0.6): they must accept executive implementation they oppose, but they retain electoral exit and can campaign to flip the majority. Constitutional judicial review sits analytically at the beneficiary end (d~0.05) because it exists to enforce the constraint, not to bear its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exhibit mandatrophy. The founding problem remains live: France continues to confront the tensions between presidential decisiveness and democratic accountability that motivated the dual-executive design. The constraint's function has not outlived its justification. What HAS changed is the nature of cohabitation pressure: the 2000 constitutional amendment synchronized the presidential and legislative election cycles, reducing the frequency and duration of cohabitation but not eliminating it (2022 elections produced a fractured Assembly where no single party controlled a legislative majority, reviving cohabitation dynamics). This is drift within the constraint's persistent function, not mandatrophy. The rising extraction values in the mid-interval reflect institutional learning about how to interpret ambiguous constitutional boundaries, not a fundamental loss of the constraint's coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserved_domain_boundary_ambiguity,
    'Where is the boundary between the president''s reserved-domain powers (foreign policy, defense, emergency decrees) and legislative co-sovereignty? Can a president exercise emergency decree authority without subsequent legislative authorization?',
    'Constitutional Court jurisprudence (Conseil Constitutionnel) interpreting Articles 15, 16, and 89; legislative testing through actual cohabitation crises where presidents attempt boundary expansion; comparative analysis of court rulings across cohabitation periods.',
    'If reserved domains are truly exclusive, the constraint''s scope on executive power is narrower than claimed; if they require legislative ratification post-implementation, the constraint is tighter. The reading''s classification depends on this boundary being genuinely contestable, which produces different classifications if the boundary shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_domain_boundary_ambiguity, conceptual, 'Constitutional boundary between presidential reserved domain and legislative authorization.').

omega_variable(
    hyper_presidential_reading_vs_parliamentary_reading_foreclosure,
    'Can both the hyper-presidential reading (president as sovereign) and the parliamentary-constraint reading (executive requires legislative authorization) coexist as live interpretations within a single constitutional framework, or does one logically foreclose the other?',
    'Constitutional jurisprudence history: review whether the Conseil Constitutionnel has ever validated both interpretations simultaneously, or whether it has settled on one. Legal-philosophical analysis of whether ''sovereignty'' (hyper-presidential axiom) and ''legislative authorization requirement'' (parliamentary axiom) are logically contradictory or merely contested.',
    'If they foreclose each other, the reading_relations entry from parliamentary_constraint_reading to hyper_presidential_reading should be ''forecloses'' rather than ''coexists_with''. If they coexist through institutional compromise (different domains, different crisis moments), the relation remains ''coexists_with''. This is an empirical-plus-conceptual question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hyper_presidential_reading_vs_parliamentary_reading_foreclosure, conceptual, 'Whether the parliamentary-constraint and hyper-presidential readings logically foreclose each other or coexist as live interpretations.').

omega_variable(
    cohabitation_extractiveness_spike,
    'During cohabitation periods, does the extraction measured in the constraint rise because the president is genuinely trapped (high d, high χ) or because the measuring optics shift to make the constraint more visible while its actual function remains coordination?',
    'Measure extraction across matched periods: same-party control vs. cohabitation. If extractiveness rises during cohabitation, is it because the president''s agency is curtailed (measurement-optics shift) or because the legislative majority is actively extracting rents? Compare to instances where cohabitation occurred but institutional norms prevented extractive behavior.',
    'If the rise is optics (the constraint becomes more visible but not more extractive), the classification remains rope across both same-party and cohabitation regimes. If the rise reflects genuine rent-seeking by a cohabiting legislature, classification flips toward tangled_rope during cohabitation. This omega documents whether the constraint''s identity is stable or state-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_extractiveness_spike, empirical, 'Whether extraction-metric rise during cohabitation reflects genuine changes in constraint structure or measurement-optics shift.').

omega_variable(
    democratic_authorization_vs_institutional_capture,
    'Is the legislative authorization requirement a democratic coordination mechanism (as this reading claims) or a structural vulnerability through which legislative majorities can capture the presidency and extract rents via cohabitation?',
    'Examine whether cohabiting legislatures used their authority to systematically redirect executive agency toward legislative-majority interests (capturing agencies, reversing executive policy, using confidence votes coercively). Compare to periods where authorization was used to sustain rather than overturn executive policy. Analyze whether legislative majorities had extractive intent or merely policy divergence.',
    'If the legislature systematically extracted rents during cohabitation (redirecting executive resources, imposing legislative-preferred policies beyond what policy divergence explains), classification shifts toward snare or tangled_rope. If legislative use of authorization powers was consistent with democratic contestation and policy divergence (no rent-seeking), the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_authorization_vs_institutional_capture, empirical, 'Whether legislative authorization requirement operates as democratic coordination or institutional-capture mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(fift_tr_t35, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 35, 0.21).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(fift_tr_t65, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 65, 0.24).
narrative_ontology:measurement(fift_tr_t70, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(fift_be_t35, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 35, 0.33).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(fift_be_t65, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 65, 0.31).
narrative_ontology:measurement(fift_be_t70, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 70, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(fift_su_t35, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 35, 0.24).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement(fift_su_t65, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 65, 0.19).
narrative_ontology:measurement(fift_su_t70, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 70, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, french_constitutional_amendment_cycle).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, government_no_confidence_vote_rule).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'fifth_republic_constitution'. Three separate constraint stories instantiate from this single constitutional text: (1) parliamentary_constraint_reading (this story) reads the core meaning as executive subordination to legislative authorization; (2) hyper_presidential_reading reads the same text as granting presidential sovereignty; (3) cohabitation_equilibrium_reading reads the text as dual-power negotiation. The epsilon values differ substantially across readings (0.31 parliamentary, ~0.15 hyper-presidential, ~0.45 cohabitation-equilibrium) because the readings assess the same constitutional arrangement against different counterfactuals: parliamentary reading measures extraction relative to pure coordination (legislative majorities enforcing authorization requirements); hyper-presidential reading measures extraction relative to unrestricted presidential sovereignty; cohabitation reading measures extraction as the cost of two-branch negotiation. The three readings are linked via network.affects_constraints because each reading's classification depends on which of the other readings' premises you accept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__parliamentary_constraint_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
