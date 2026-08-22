% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary (Balanced Contestation Reading)
 *   domain: constitutional_law/judicial_review
 *
 * SUMMARY:
 *   This constraint instantiates the BALANCED CONTESTATION READING of the
 *   interpretive boundary between Israel's Supreme Court and the Knesset.
 *   Under this reading, both institutions hold legitimate but bounded
 *   authority: the court interprets Basic Laws and reviews legislation for
 *   consistency; the legislature retains ultimate sovereign power to amend
 *   Basic Laws via supermajority and retains implicit authority to alter the
 *   boundary itself. The reading is distinguished by its emphasis on
 *   institutional dialogue rather than supremacy of either pole. Courts
 *   operate within a jurisdictional domain that includes reviewing
 *   legislation but not final veto; the legislature operates under norms of
 *   respecting judicial independence and accepting supermajority constraints
 *   while retaining the power to override. Neither institution is fully
 *   dominant; the constraint's operation depends on continuous negotiation
 *   across policy domains. The claim (tangled_rope: coordination function +
 *   asymmetric extraction) reflects the duality: genuine coordination between
 *   institutional actors on how rights are protected and sovereignty is
 *   exercised; asymmetric extraction because the court bears the cost of
 *   legitimacy without final authority, and the legislature bears the cost of
 *   justification without unilateral speed.
 *
 * KEY AGENTS:
 *   - Supreme Court: institutional agenda-setter and beneficiary; claims interpretive authority within bounded domain; operates under judicial independence norms
 *   - Knesset: institutional agenda-setter and secondary payer; retains ultimate sovereign authority but constrained by supermajority amendment norms and judicial contestation
 *   - Executive Branch: institutional payer; caught between legislative mandate and judicial constraint; implements in the space negotiated between the two authority claims
 *   - Rights claimants: powerless beneficiaries; access constitutional protection through court but trapped by whichever institution dominates in each policy cycle
 *   - Opposition caucus: moderate payer; uses judicial review as constraint on majority legislation; leverage depends on court's willingness to intervene
 *   - Majoritarian coalition: powerful agenda-setter; controls legislative-executive apparatus but constrained by judicial review and supermajority requirements; must negotiate with court and opposition
 *   - International bodies and scholarship: analytical observers; exert reputational pressure on both institutions but excluded from domestic negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.41).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary (Balanced Contestation Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/judicial_review").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '4c629b71-afd0-48ac-9e83-da6fa701ec8e').
narrative_ontology:cs_kernel_codification('4c629b71-afd0-48ac-9e83-da6fa701ec8e', formalized).
narrative_ontology:cs_authority_grounding('4c629b71-afd0-48ac-9e83-da6fa701ec8e', extraction).
narrative_ontology:cs_interpretation_layer_present('4c629b71-afd0-48ac-9e83-da6fa701ec8e').
narrative_ontology:cs_reading_relation('4c629b71-afd0-48ac-9e83-da6fa701ec8e', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c629b71-afd0-48ac-9e83-da6fa701ec8e', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('4c629b71-afd0-48ac-9e83-da6fa701ec8e', foundational, dual_institutional_legitimacy).
narrative_ontology:cs_axiom_status(dual_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4c629b71-afd0-48ac-9e83-da6fa701ec8e', dual_institutional_legitimacy, conventional).
narrative_ontology:cs_axiom('4c629b71-afd0-48ac-9e83-da6fa701ec8e', foundational, bounded_authority_through_dialogue).
narrative_ontology:cs_axiom_status(bounded_authority_through_dialogue, holdable).
narrative_ontology:cs_axiom_grounding('4c629b71-afd0-48ac-9e83-da6fa701ec8e', bounded_authority_through_dialogue, instrumental).
narrative_ontology:cs_reference_frame('4c629b71-afd0-48ac-9e83-da6fa701ec8e', coordinate_authority_framework).
narrative_ontology:cs_drift_state('4c629b71-afd0-48ac-9e83-da6fa701ec8e', contemporary_supremacy_pressures, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c629b71-afd0-48ac-9e83-da6fa701ec8e', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_branch_sovereignty_claims).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_finality_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, opposition_legislature_caucus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws and reviews legislation for consistency with them. Operates within a bounded interpretive domain: claims authority over constitutional meaning but acknowledges the legislature's power to amend or override through supermajority process. The constraint grants it institutional standing to contest legislative action while subordinating final sovereignty. Gains legitimacy and institutional prestige from this role; bears institutional risk when the legislature overrides or curtails its authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, beneficiary).

% Retains ultimate sovereign authority to legislate and amend Basic Laws but operates under a norm that does so via supermajority, not simple majority, and acknowledges judicial review as a legitimate check within the legislative cycle. Pays the cost of negotiating with the court, defending legislation in public justification, and sometimes accepting remand or negotiated amendment rather than immediate enactment. Gains legitimacy from democratic election but constrained by judicial contestation and international norm pressure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset, payer).

% Operates under dual accountability: legislative oversight and, increasingly, judicial review of executive action. Bears costs of defending security, administrative, and policy decisions in court. Caught between legislative mandates and judicial constraints on implementation authority. Must navigate the institutional dialogue between court and legislature, sometimes becoming a proxy arena for constitutional contest.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Access constitutional rights protection through judicial review without needing supermajority legislative consent. The balanced contestation reading grants them standing to petition the court and expect substantive review, while the legislature retains authority to respond via amendment process. Their exit is trapped: rights protection depends on whichever institution dominates in each domain cycle.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, rights_claimants, beneficiary,
    powerless, biographical, trapped, national).

% Monitor judicial independence and legislative compliance with international human rights norms. Their assessments shape reputational pressure on both institutions. They would argue for strengthened judicial authority and limitations on legislative override, but are structurally outside the domestic constitutional negotiation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_bodies, excluded,
    powerful, generational, analytical, global).

% Uses judicial review as a check on government legislation, leveraging court decisions to delay or reshape executive-majority initiatives. Pays a cost when the court defers to legislative judgment; benefits when the court intervenes. Their leverage depends on the reading of the boundary: more court authority favors their contestation strategy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, opposition_legislature_caucus, payer,
    moderate, biographical, mobile, national).

% Controls the legislature and executive through electoral mandate but constrained by judicial review and supermajority amendment requirements. Must justify legislation to the court, engage in public constitutional argument, and sometimes negotiate amendment with the opposition to achieve supermajority. Gains speed and authority initially; bears cost of judicial contestation and negotiation friction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, majoritarian_coalition, agenda_setter,
    powerful, biographical, mobile, national).

% Interpret and advocate different readings of the interpretive boundary. Influence court decisions through amicus briefs and academic authority. Their analyses shape how the constraint is understood and contested by other institutions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dual-authority system that combines democratic legislative sovereignty with rights protection: both institutions claim authority within bounded domains, creating a structured dialogue that prevents either from exercising unilateral control while allowing constitutional development through court-legislature negotiation.
% TRANSFER_FUNCTION: Transfers interpretive power from the legislature's unilateral hands to a joint legislature-court negotiation process: legislation must survive judicial review, but the legislature retains amendment authority, creating a cycle where neither institution's decisions are final without the other's acquiescence or supermajority resistance.
% ABSENT_VOICES: The executive branch below the policy level (administrators, security officials, enforcement personnel) are indirectly constrained but not directly seated. Minorities and permanent opposition caucuses are nominally protected but depend on court good faith. International human rights bodies are excluded from domestic deliberation but exert reputational pressure from outside.
% DISAPPEARANCE_RATIONALE: If this interpretive boundary dissolved overnight—if the court lost authority to review legislation or the legislature lost authority to override the court—the constitutional system would reorganize: either into pure judicial supremacy (court finality) or pure parliamentary sovereignty (legislature supremacy), with cascading changes in how rights are protected, how legislation is justified, and where political contestation occurs.
% FOUNDING_PROBLEM: Israel's lack of a written constitution created ambiguity about the source and limits of authority: should sovereignty rest in the elected Knesset alone, or does a foundational commitment to rights (initially articulated in Basic Laws) create a higher-order constraint on legislative power? The balanced reading emerged as a middle ground to acknowledge both democratic authority and rights protection without resolving the underlying tension.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars outside the parties (e.g., Aharon Barak in his retirement writings, international comparative constitutionalism analysts) attest that the founding problem remains live: the Basic Laws still lack explicit supremacy language, creating ongoing institutional contestation. Legislative debates on nationality laws and judicial independence show both institutions still claim authority to define the boundary. Court decisions regularly defend the court's authority while acknowledging legislative supremacy in certain domains, corroborating that the problem is unresolved, not answered.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measured at 0.52 (interval end) reflects moderate asymmetry: both institutions claim authority but within different domains and subject to different constraints. The court claims interpretive authority but lacks final power; the legislature claims final authority but operates under supermajority and judicial deference norms. This is not pure coordination (where both gain without one paying) nor pure extraction (where gains concentrate). Suppression at 0.41 reflects institutional enforcement mechanisms (court dockets, legislative procedure, constitutional amendment rules) rather than coercive force—the suppression machinery maintains the boundary, not external domination. Theater at 0.28 is modest: there is genuine institutional dialogue (court-legislature negotiation, public justification), but an increasing share of energy defends the boundary itself rather than solving the underlying coordination problem (how to protect rights while respecting democratic authority). The measurement series shows rising extractiveness through the first 25 years (institutional costs mounting as the court expands review, legislature develops override strategies) then plateauing—the boundary has stabilized at a higher-extraction point than the founding reading envisioned. This plateau pattern is characteristic of tangled ropes that have calcified into semi-permanent institutional positions.
 *
 * PERSPECTIVAL GAP:
 *   This reading (balanced contestation) presents the relationship between court and legislature as institutionally symmetric but functionally asymmetric: both claim authority, neither dominates finally, but in practice the court operates as a check on the legislature rather than a co-equal partner. A seat occupied by legislative sovereignty maximalists (the parliamentary_sovereignty_reading) would see the same constraint as pure extraction by an unelected, unaccountable judicial body claiming authority it does not legitimately possess. A seat occupied by judicial authority maximalists (the judicial_supremacy_reading) would see the legislature as a force that undermines constitutional protection and rule of law. The balanced reading sits between these poles, accepting that both institutions have legitimate claims and that their interaction shapes constitutional development. This perspectival gap is not a failure of the framework but its strength: the same structural arrangement produces different types depending on which seat computes the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court (institutional power, constrained exit, agenda-setter role) derives directionality near 0.3–0.4: it is a beneficiary of the constraint's existence (gains institutional authority and prestige) but pays through continuous contestation and lack of finality. The Knesset (institutional power, constrained exit, agenda-setter role) derives directionality near 0.5–0.6: it retains ultimate authority (beneficiary dimension) but must negotiate and justify (payer dimension), with the payer dimension slightly dominant because the supermajority requirement is a genuine friction cost. Rights claimants (powerless, trapped exit, beneficiary role) derive directionality near 0.8: they benefit from having access to the court but cannot exit and depend on the court's capacity and willingness to intervene—the constraint gives them protection but they cannot ensure it. The opposition caucus (moderate power, mobile exit, payer role) derives directionality near 0.6: they use the court as a tool and can exit to electoral competition, but while in the legislature they must negotiate with the majority. The executive (institutional power, constrained exit, payer role) derives directionality near 0.7: it is subordinate to both institutions and must implement in the negotiated space. These divergent d values are the mechanism by which the same constraint produces different effective extraction rates across seats—a hallmark of tangled_rope structures where some seats coordinate and some are extracted from through the same institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining the source and scope of constitutional authority) is explicitly LIVE, not superseded: the Knesset continues to claim unfettered amendment authority, the court continues to claim interpretive authority, and international pressure pushes toward stronger judicial independence norms. The constraint's mandate—to coordinate authority between two institutions—is not atrophied but increasingly performs under strain: theater_ratio rises from 0.12 to 0.28 because both institutions spend growing effort defending the boundary itself rather than performing the function (constitutional development, rights protection) the boundary supposedly serves. This is the signature of mandatrophy in process: the constraint persists, enforcement machinery is maintained, but the primary function has withered and is being replaced by a performative function (justifying the existence of the boundary). The measured extractiveness plateau (0.52 at intervals 25–40) indicates the constraint has settled into a semi-permanent extraction state rather than being resolved. Mandatrophy_resolved is not yet appropriate (the constraint still functions; the founding problem is still contested), but the trajectory suggests mandatrophy developing if the theater_ratio continues rising or if one institution successfully overrides the other's authority claims entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_vs_supremacy,
    'Is the interpretive boundary this reading describes a stable institutional equilibrium, or does it inevitably resolve toward either judicial supremacy or parliamentary sovereignty?',
    'Historical observation of how the boundary shifts under stress: when the legislature overrides the court massively (e.g., nationality law revisions), does the court reassert authority or accept subordination? When the court strikes down core legislation, does the legislature accept or attempt override? Long-term institutional behavior under constitutional pressure reveals whether the balanced reading is self-sustaining or unstable.',
    'If the boundary is unstable and drifts toward supremacy of either pole, the classification should shift to judicial_supremacy_reading or parliamentary_sovereignty_reading. If it remains contentious and negotiated, the balanced_contestation reading is confirmed as the accurate structural model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_vs_supremacy, empirical, 'Whether the balanced boundary is institutionally stable or inherently unstable.').

omega_variable(
    supermajority_norm_versus_constitutional_text,
    'Is the supermajority requirement for constitutional amendment a binding constitutional rule or a conventional norm that could be overridden by a future determined legislature?',
    'Test case: if a legislature with overwhelming supermajority support (75%+) attempts to amend the Basic Law via simple majority, would the court enforce the supermajority requirement or defer to the legislature''s claim of sovereign power? The court''s response determines whether the supermajority norm is constitutional or merely prudential.',
    'If the court enforces supermajority against a supermajority-supported amendment, the balanced reading is confirmed (the court has enforceable constitutional authority). If the court defers, the reading collapses toward parliamentary_sovereignty_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supermajority_norm_versus_constitutional_text, empirical, 'Whether the supermajority amendment requirement is binding or merely conventional.').

omega_variable(
    institutional_rivalry_as_extraction_mechanism,
    'Does the institutional dialogue between court and legislature constitute a genuine coordination mechanism that benefits both (allowing each to constrain excess and develop constitutional meaning jointly), or is it increasingly a rivalry that extracts legitimacy from both institutions while protecting neither side''s core interests?',
    'Content analysis of institutional positions: are the court and legislature converging on shared constitutional principles, or are they increasingly staking claims to divergent bases of authority? Measurement of public trust in each institution: if both are declining, the dialogue may be delegitimizing rather than coordinating.',
    'If the dialogue is coordinating, the constraint remains tangled_rope (coordination function + asymmetric extraction). If the dialogue is increasingly a rivalry for authority with no coordination benefit, the constraint drifts toward snare (pure institutional extraction masked as constitutional dialogue).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_rivalry_as_extraction_mechanism, empirical, 'Whether the court-legislature interaction is coordinative or exploitative of both institutions'' legitimacy.').

omega_variable(
    committer_reading_underdetermination,
    'Does the balanced_contestation reading represent a genuine third position between judicial supremacy and parliamentary sovereignty, or is it a temporary compromise that masks an underlying commitment to one pole that would be revealed under stress?',
    'Examine the reading''s own internal coherence: can both institutions actually hold their claimed authority within a single legal framework, or do the claims logically conflict at crucial boundaries? In domains where the court and legislature have directly collided (nationality law, constitutional rights), which institution''s authority claim does the balanced reading ultimately privilege?',
    'If the balanced reading contains an implicit privileging of one institution''s authority (e.g., it actually privileges court authority under the guise of balance), it is a misdeclared reading and should be reclassified under its actual commitment. If the reading genuinely maintains balance by accepting permanent institutional contestation, it is correctly classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_underdetermination, conceptual, 'Whether the balanced reading is genuinely triadic or covertly resolves to supremacy of one pole.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(basi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(basi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 5, 0.31).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(basi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__balanced_contestation_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_override_amendment_power).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_review_scope_and_justiciability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the basic_law_interpretive_boundary kernel. The three readings (judicial_supremacy, parliamentary_sovereignty, balanced_contestation) are not alternative measurements of the same constraint; they are structurally distinct constraints instantiated by the same contested kernel. Each has a different ε, different beneficiary/victim structures, and different claims about where constitutional authority resides. The balanced_contestation reading is distinguished by its emphasis on institutional dialogue and mutual constraint rather than supremacy of either pole. All three readings affect the same downstream constraints (override power, scope of review) but in different ways—a supremacy reading would forecast different override patterns than a balanced reading would.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
