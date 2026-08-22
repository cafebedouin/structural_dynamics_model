% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy_hybrid_pragmatic_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   In the late 19th century, a major religious institution faced federal
 *   legal pressure to abandon a core doctrinal practice (polygamous
 *   marriage). Institutional leadership issued the Manifesto, framed as new
 *   prophecy commanding the practice's abandonment. The hybrid pragmatic
 *   reading interprets this as strategic institutional adaptation: prophetic
 *   authority deployed to preserve both federal legal standing and
 *   theological coherence by introducing scope ambiguity ('eternal doctrine'
 *   vs. 'local practice'). This reading is ONE of three contending readings
 *   of the same kernel commitment—the claim about marriage legitimacy
 *   grounded in prophetic authority. The other readings are: (1)
 *   endogenous_reinterpretation_reading (God commanded the reversal to
 *   preserve the Church), which asserts pure revelation without acknowledging
 *   exogenous pressure; (2) exogenous_override_reading (federal coercion
 *   forced capitulation), which denies prophetic authenticity and treats the
 *   Manifesto as institutional pragmatism without theological authority. This
 *   reading sits between: institutional leadership benefits from the scope
 *   ambiguity, which preserves legitimacy and flexibility simultaneously;
 *   rank-and-file members and doctrinal purists bear the cost of interpretive
 *   uncertainty.
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda-setter seat (benefits from scope ambiguity; controls prophetic authentication)
 *   - rank_and_file_members: payer seat (identity-locked; bear interpretive uncertainty)
 *   - doctrinal_purists: payer seat (recognize the ambiguity as contradiction; most resistant but most suppressed)
 *   - federal_authority: excluded seat (materially responsible; officially absent from theological frame)
 *   - theological_interpreters: beneficiary seat (benefit from interpretive work; more mobile than members)
 *   - alternative_prophetic_claimants: excluded seat (structurally barred; claim original doctrine still valid)
 *   - historical_witness_class: observer seat (can document causality; no legitimacy authority)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.56).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious/institutional/political theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '3fbc2a24-99d8-45b8-a74c-e92c971efc65').
narrative_ontology:cs_kernel_codification('3fbc2a24-99d8-45b8-a74c-e92c971efc65', fixed_text).
narrative_ontology:cs_authority_grounding('3fbc2a24-99d8-45b8-a74c-e92c971efc65', lineage).
narrative_ontology:cs_interpretation_layer_present('3fbc2a24-99d8-45b8-a74c-e92c971efc65').
narrative_ontology:cs_reading_relation('3fbc2a24-99d8-45b8-a74c-e92c971efc65', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fbc2a24-99d8-45b8-a74c-e92c971efc65', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('3fbc2a24-99d8-45b8-a74c-e92c971efc65', foundational, institutional_pragmatism_compatible_with_prophetic_authority).
narrative_ontology:cs_axiom_status(institutional_pragmatism_compatible_with_prophetic_authority, holdable).
narrative_ontology:cs_axiom_grounding('3fbc2a24-99d8-45b8-a74c-e92c971efc65', institutional_pragmatism_compatible_with_prophetic_authority, conventional).
narrative_ontology:cs_axiom('3fbc2a24-99d8-45b8-a74c-e92c971efc65', foundational, scope_ambiguity_preserves_doctrinal_coherence).
narrative_ontology:cs_axiom_status(scope_ambiguity_preserves_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('3fbc2a24-99d8-45b8-a74c-e92c971efc65', scope_ambiguity_preserves_doctrinal_coherence, deontological).
narrative_ontology:cs_reference_frame('3fbc2a24-99d8-45b8-a74c-e92c971efc65', prophetic_revelation_legitimacy_fixed_doctrine).
narrative_ontology:cs_drift_state('3fbc2a24-99d8-45b8-a74c-e92c971efc65', post_federal_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3fbc2a24-99d8-45b8-a74c-e92c971efc65', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_purists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and authorizes the Manifesto as prophetic guidance responding to exogenous federal pressure. Maintains doctrinal orthodoxy in theory while creating interpretive flexibility in practice. Benefits from preserving institutional continuity, federal legal standing, and theological authority simultaneously—avoiding both dissolution and explicit doctrinal reversal. Bears the burden of managing constituency expectation across opposing interpretive camps.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Must navigate a legitimacy ambiguity: the Manifesto is presented as prophetic revelation yet visibly responds to federal coercion. Identity-locked by theological membership and kinship networks; exit costs include spiritual rupture and social isolation. Bear the cognitive and relational cost of holding contradictory frameworks simultaneously (divine command AND institutional pragmatism). No formal voice in the reading authority.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, identity_locked, national).

% Maintain that the original doctrine was revealed truth; the Manifesto represents institutional capitulation dressed as prophecy. They recognize the scope ambiguity—'eternal doctrine' asserted alongside 'local practice change'—as a distinction without difference. Constrained by kinship and institutional ties but possess theological expertise that gives them partial platform for dissent. Their resistance is greatest but most suppressed.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_purists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_purists, observer).

% Applied legal pressure (criminal penalties, property seizure, institutional harassment) that forced the marriage policy reversal. Officially absent from the theological reading but causally responsible for the timing and scope of the Manifesto. Would argue for institutional capitulation reading; their voice is structurally external to the religious authority but materially determines its bounds.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authority, excluded,
    institutional, generational, analytical, national).

% Academic and professional theologians tasked with elaborating the Manifesto's meaning. Benefit from the scope ambiguity—it creates interpretive work and institutional demand for their expertise. Can author competing readings of what 'eternal doctrine preserved in local practice change' means, generating scholarly apparatus and institutional prestige. More mobile than rank-and-file members; can shift careers if interpretive burden becomes unsustainable.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_interpreters, beneficiary,
    organized, biographical, mobile, national).

% Sect members and dissenting communities who claim the original doctrine is still divinely mandated and the Manifesto is false prophecy. Structurally barred from the institutional reading authority by the very mechanism that validates the Manifesto (the leadership monopoly on prophetic authentication). Would argue for doctrinal continuity; their exclusion from legitimacy production is the enforcement mechanism itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, alternative_prophetic_claimants, excluded,
    moderate, biographical, trapped, national).

% Historians, legal analysts, and outside observers who can document the timing between federal pressure and prophetic declaration. They provide external calibration on whether the Manifesto is best read as revelation, coercion, or institutional adaptation. No formal role in legitimacy production but their testimony constrains the plausibility of any single reading.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, historical_witness_class, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the religious community under a single authority structure that can change practice doctrine while preserving theological identity. Solves the coordination problem of managing an institution under exogenous legal pressure without fracturing into competing prophetic claims or explicit doctrinal reversal that would lose members to alternative groups.
% TRANSFER_FUNCTION: Transfers interpretive authority and institutional legitimacy from a transparent doctrinal boundary ('this doctrine is eternally true') to a scope-ambiguous boundary ('this doctrine is eternally true but applies only to this context'). The leadership captures the authority to determine what counts as 'local practice' versus 'eternal doctrine.' Rank-and-file members and doctrinal purists transfer legitimacy to the institution while bearing the cognitive cost of the ambiguity.
% ABSENT_VOICES: Federal authorities (who created the coercive pressure) are structurally excluded from the theological reading. Alternative prophetic claimants are barred from the legitimacy-producing apparatus by the same mechanism the Manifesto relies on (centralized leadership authentication). Historians and legal observers document the constraint's causal structure but have no authority seat.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its hybrid pragmatic reading disappeared, the institution would either fracture into competing prophetic authorities (sect formation), revert to the original doctrine (legal jeopardy with federal authorities), or adopt explicit institutional pragmatism (abandoning the prophetic authority frame entirely). The community would reorganize around a different legitimacy mechanism.
% FOUNDING_PROBLEM: Federal legal pressure criminalized a core doctrinal practice. The institution needed to reverse the practice without losing theological authority, member commitment, or doctrinal coherence. A transparent capitulation would undermine prophetic legitimacy; explicit doctrinal revision would require acknowledging fallibility; silence would invite federal escalation.
% FOUNDING_PROBLEM_CORROBORATION: Federal legal documents and institutional property seizure records attest to the exogenous pressure. Institutional records and contemporaneous member correspondence (from dissenting members, academic theologians, and historians outside the benefiting leadership) attest that the problem persisted and drove the Manifesto's timing. The temporal alignment between federal escalation and prophetic declaration is corroborated by external observers. The institutional leadership does not corroborate the 'exogenous pressure' reading—they attest divine revelation only.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.56, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.56) because the institutional leadership captures legitimacy authority while appearing to transmit divine revelation, and the scope ambiguity creates ongoing interpretive work that benefits theologians and administrators relative to rank-and-file members. Suppression is substantial (0.62) because the mechanism depends on maintaining the scope distinction against criticism—doctrinal purists who recognize it as false distinction must be silenced or expelled. Theater ratio rises over time (0.22→0.48) as the Manifesto's prophetic framing becomes increasingly performative and the institutional apparatus of interpretation elaborates around the original ambiguity—more energy devoted to defending the reading than to prophetic function itself. Accessibility_collapse is high (0.65) because once you understand the reading's dependence on scope ambiguity, the alternatives (pure revelation, pure coercion, explicit pragmatism) become salient, but the identity lock keeps members from adopting them. Resistance is high (0.71) because doctrinal purists mount substantive theological critique and federal observers note the causal timing. The shared time grid tracks the same eight measurement points for all three metrics, so the engine can integrate them without misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership (agenda-setter) and rank-and-file members (payers) should compute very differently. From the leadership seat, the arrangement solves a genuine institutional coordination problem under exogenous pressure—a tangled rope with real coordination function and real extraction (the authority capture). From the payer seats, the arrangement is experienced as ambiguity-imposition: members are told to hold two contradictory frameworks (eternal doctrine + local practice reversal) and the cost of resolution (exit via doctrinal schism or identity rupture) is prohibitive. The engine computes directionality per seat from the authored power + exit + beneficiary/victim declarations; the leadership's mobile, organized, powerful position derives toward low d (beneficiary end) while the rank-and-file identity-locked, powerless position derives toward high d (target end). This seat-level divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership: powerful + constrained exit (organizational stake) + beneficiary role → low d, near full beneficiary end (d ≈ 0.2). They control the authentication mechanism and benefit from the scope ambiguity's flexibility. Rank-and-file members: powerless + identity-locked exit + payer role → high d, near target end (d ≈ 0.8). They absorb the interpretive cost and cannot exit without rupture. Doctrinal purists: moderate power (theological expertise gives them platform) + constrained exit (kinship, organizational ties) + payer role + observer access → mid-high d (d ≈ 0.65). They can articulate critique but are suppressed by the authority structure. The suppression mechanism targets them specifically: their dissent is most dangerous because they speak the institution's own theological language. Federal authority is analytically crucial but structurally excluded; they derive no d within the religious commitment system (they sit outside it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal legal pressure on a core doctrinal practice) is LIVE, not dead. The constraint's persistence depends on avoiding both federal escalation AND institutional fracture. The Manifesto's scope ambiguity is the solution: it allows the institution to reverse practice while preserving doctrinal claim. This prevents mandatrophy in the sense that the founding problem persists and the constraint addresses it. However, the rising theater_ratio (0.22→0.48) is diagnostic of degradation in the prophetic function—the arrangement becomes increasingly performative rather than revelatory. The mandatrophy marker is the growing gap between the Manifesto's prophetic framing and its causal history (exogenous pressure timing correlation increases over time as historical records accumulate). The constraint remains tangled_rope rather than devolving to piton because institutional leadership continues to actively enforce the scope distinction and member compliance persists; but the theater trajectory suggests eventual piton-hood if the prophetic claim erodes further and enforcement becomes purely institutional maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_agency_ambiguity,
    'Is the Manifesto best understood as genuine divine revelation communicated through institutional channels, or as institutional pragmatism framed in prophetic language, or as federal coercion laundered through prophetic authorization?',
    'Historical-epistemological: (1) contemporaneous institutional records (letters, council minutes, leadership memoirs) that document private reasoning vs. public framing; (2) testimony from dissenting members and alternative prophetic claimants about their grounds for rejecting the reading; (3) comparative analysis of similar institutional adaptations in other religious and secular contexts to identify common patterns of framing under pressure. No single data point resolves this—different evidence types support different readings.',
    'If endogenous (divine revelation): the constraint is pure rope (genuine coordination function, no extraction). If exogenous (federal coercion): the constraint is pure snare (coercive extraction, coordination frame is cover story). If hybrid pragmatic: the constraint is tangled rope (real coordination function + real extraction via authority capture and legitimacy ambiguity). Classification hinges entirely on which reading is adopted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_agency_ambiguity, conceptual, 'Whether the Manifesto instantiates revelation, coercion, or institutional pragmatism—or whether the distinction collapses under scrutiny.').

omega_variable(
    scope_ambiguity_mechanism,
    'Is the distinction between ''eternal doctrine'' and ''local practice change'' a genuine theological category the Manifesto introduces, or is it a distinction without difference—a rhetorical strategy to claim doctrinal continuity while enabling doctrinal reversal?',
    'Theological and institutional analysis: (1) whether the leadership explicitly articulated criteria for what counts as ''local practice'' vs. ''eternal doctrine'' in authoritative texts, or whether the boundary remains ambiguous; (2) subsequent cases where the institution claimed something else was ''local practice'' to show whether the scope distinction is applied consistently or used ad hoc; (3) dissenting theologians'' explications of why they reject the distinction—do they identify a logical flaw in the category, or protest its application in this case specifically?',
    'If the scope distinction is genuine and consistent: the Manifesto preserves doctrinal coherence while changing practice—a real coordination solution. If the scope distinction is ad hoc or rhetorical: the constraint is extractive (leadership claims authority to redefine doctrine as ''local practice'' when convenient)—extraction via reframing. The constraint''s type hinges on whether the scope mechanism is coherent or strategically ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_mechanism, empirical, 'Whether the ''eternal doctrine vs. local practice'' distinction is a workable theological category or rhetorical camouflage.').

omega_variable(
    identity_lock_suppression_causality,
    'Is the rank-and-file member''s suppression (their silence and compliance despite recognizing the Manifesto''s pragmatic origin) primarily structural (federal pressure makes the institution existentially fragile; dissent risks collapse) or primarily internalized (members have fused their identity with the institution and cannot imagine exit despite its possibility)?',
    'Comparative analysis of post-suppression trajectories: (1) members who left after doctrinal crises in other periods—how quickly did their suppression lift once external pressure changed or alternative communities became available? (2) cohort analysis: did suppression ease for younger members with weaker identity lock, or remain uniform across generations? (3) ethnographic or interview evidence from members describing what prevented their dissent—external barriers or psychological identity fusion?',
    'If suppression is primarily structural: the constraint''s persistence depends on federal pressure remaining credible; if pressure eases, suppression eases. If suppression is primarily internalized: members carry the suppression with them even if they exit; the constraint has extracted internalized self-policing. For piton detection: strong internalized suppression + weakening external pressure = the constraint continues via theater and identity maintenance even after the institutional justification erodes (key piton signal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_causality, empirical, 'Whether rank-and-file suppression is imposed by structural coercion or internalized identity fusion.').

omega_variable(
    sibling_reading_foreclosure,
    'Do any two of the three readings logically foreclose each other—i.e., can a single institutional actor coherently hold more than one reading simultaneously, or are they mutually exclusive commitments?',
    'Logical-structural: examine whether endogenous_reinterpretation (God commanded the reversal) and exogenous_override (federal coercion forced it) could both be true for the same actor. They could if the actor believes both ''God moved federal pressure to force the institution to recognize a doctrinal error''—which is coherent. They cannot both be true if one actor must hold exactly one. The hybrid pragmatic reading coexists with both: it asserts that institutional actors *treated* the situation as pragmatic adaptation while *claiming* it as revelation. This is the core committer-frame question: can a single reading framework hold multiple causal stories, or must it commit to one?',
    'If readings are mutually exclusive (foreclosure): the kernel will eventually crystallize into one reading if evidence accumulates. If readings coexist (no foreclosure): the constraint will persist in multi-reading equilibrium indefinitely. The institutional answer determines whether this constraint is unstable or stable long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical compatibility or exclusivity of the three readings of the marriage-commitment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(marr_tr_t25, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(marr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(marr_be_t25, observed).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(marr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(marr_su_t25, observed).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(marr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'marriage_commitment_legitimacy,' grounded in the Manifesto text and the institutional authority claims surrounding it. All three readings share the same empirical facts (federal pressure, Manifesto issuance, practice reversal) but disagree on causal attribution and theological meaning. The hybrid_pragmatic_reading treats the Manifesto as strategic institutional adaptation using prophetic framing. The endogenous_reinterpretation_reading treats it as genuine divine revelation. The exogenous_override_reading treats it as federal coercion without theological authority. Each reading instantiates a different ε (extraction), different beneficiary/victim structure, and different claimed type. The kernel family decomposes a single natural-language concept (the Manifesto and its meaning) into three structurally distinct constraints, each internally ε-invariant. Links: the hybrid_pragmatic reading influences both siblings by providing a framework that acknowledges legitimate elements in each (prophetic authority is real for endogenous readers, federal pressure is real for exogenous readers) while centering institutional pragmatism as the primary motor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
