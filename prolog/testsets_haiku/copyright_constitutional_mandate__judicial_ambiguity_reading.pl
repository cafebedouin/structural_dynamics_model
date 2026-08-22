% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_judicial_ambiguity, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference to Congressional Copyright Term Extension
 *   domain: constitutional_law/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_ambiguity_reading of the
 *   copyright_constitutional_mandate kernel. The reading asserts that
 *   copyright term length is constitutionally ambiguous and that courts defer
 *   to Congress via rational basis review, treating term extensions as
 *   rationally related to incentivizing creation. The constraint describes
 *   the jurisprudential structure: how judicial deference enables
 *   term-extension legislation without triggering stricter scrutiny, and how
 *   this deference redistributes constitutional authority from courts (which
 *   might enforce textual 'limited Times' as a floor) to Congress (which sets
 *   the extension limit). The reading does NOT claim copyright is a property
 *   right deserving maximal extension (that is the
 *   corporate_enclosure_reading) nor that copyright exists only to enrich the
 *   public domain (that is the public_scaffold_reading). This reading is
 *   distinct: it makes a claim about the institutional allocation of
 *   constitutional authority and the opacity of judicial review standards
 *   applied to copyright.
 *
 * KEY AGENTS:
 *   - Congress: sets copyright term length via legislation; benefits from discretion; constrains the written Constitution via practice and enforcement precedent.
 *   - Federal Courts: apply rational basis review to copyright term challenges; defer to Congress by treating broad legislative rationales as sufficient; are the mechanism through which ambiguity becomes institutional fact.
 *   - Constitutional Text: 'limited Times' — the fixed kernel the reading interprets; remains stable while institutional meaning shifts through deference practice.
 *   - Public Domain Interests: authors whose works would enter public domain under a shorter term; subject to postponed entry under longer terms; lack standing in most cases to challenge extensions.
 *   - Incumbent Copyright Holders: corporate and individual owners whose copyrights receive retroactive extension; benefit from deferred entry into public domain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.48).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.35).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Congressional Copyright Term Extension").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "constitutional_law/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1').
narrative_ontology:cs_kernel_codification('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', fixed_text).
narrative_ontology:cs_authority_grounding('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', extraction).
narrative_ontology:cs_interpretation_layer_present('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1').
narrative_ontology:cs_reading_relation('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', copyright_constitutional_mandate__copyright_corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', copyright_constitutional_mandate__copyright_public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', foundational, constitutional_ambiguity_on_term_limits).
narrative_ontology:cs_axiom_status(constitutional_ambiguity_on_term_limits, holdable).
narrative_ontology:cs_axiom_grounding('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', constitutional_ambiguity_on_term_limits, deontological).
narrative_ontology:cs_axiom('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', foundational, judicial_deference_to_legislative_copyright_policy).
narrative_ontology:cs_axiom_status(judicial_deference_to_legislative_copyright_policy, holdable).
narrative_ontology:cs_axiom_grounding('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', judicial_deference_to_legislative_copyright_policy, conventional).
narrative_ontology:cs_reference_frame('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', constitutional_ambiguity_framework).
narrative_ontology:cs_drift_state('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', contemporary_deference_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('714a0bb4-8a6f-4e92-b95d-e93ffd8f77c1', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_review_standard).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term length via statute. Under rational basis review, Congress can extend terms by legislating a rationale that bears any rational relationship to incentivizing creation or progress. Congress is the structural beneficiary of judicial deference: it retains discretion without constitutional constraint. It exercises this authority repeatedly (1976, 1998, and proposed extensions) and faces no judicial invalidation. Congress need not justify extensions against the constitutional text; rational basis suffices.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Apply rational basis review to copyright term challenges. They formalize the deference: they go through the motions of asking whether the term extension bears a rational relationship to a legitimate government interest, find that it does, and uphold the statute. Courts do not enforce 'limited Times' as a substantive floor. They are the institutional mechanism through which ambiguity becomes practice.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% The fixed kernel ('limited Times') is the textual constraint that this reading interprets as ambiguous. It bears the cost of institutional drift: the meaning of 'limited' expands through deference practice, and the text loses binding force as a ceiling. The text is not an agent but a structural target; its meaning is contested, and this reading asserts that courts defer on meaning rather than enforce it.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_text_limited_times, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_text_limited_times).

% Authors whose works would enter the public domain under a shorter copyright term are blocked by term extensions. They cannot oppose extensions in court (lack of standing) and have no mechanism to propose shorter terms. They bear the cost of postponed public domain entry — works remain locked up for 20+ years longer than they would under an alternative regime. Public domain interests are diffuse and lack organizational power.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_interests, payer,
    powerless, generational, trapped, national).

% Corporate and individual owners of copyrights in force receive retroactive extension of their exclusive rights. A copyright that would expire in year X instead expires in year X+20 (the typical extension increment). They benefit from postponed public domain entry and from Congress's discretion to extend again before expiration. They have organizational capacity to lobby for extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders, beneficiary,
    powerful, biographical, mobile, national).

% Scholars, librarians, and digital commons advocates argue for shorter copyright terms or for carve-outs for public institutions. They advocate in legislative and scholarly forums but are structurally excluded from the copyright-holder lobbying apparatus. Their exclusion is not formal but institutional: copyright policy is captured by incumbent-holder interests and rational basis review permits Congress to ignore public-domain arguments.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, open_access_advocates, excluded,
    organized, generational, constrained, national).

% Copyright economists, constitutional scholars, and information-law specialists study whether copyright term length empirically incentivizes creation. A broad consensus holds that marginal incentive effects are small beyond moderate terms (15-20 years); indefinite extension produces negligible incremental incentive. This consensus is noted in amicus briefs and legislative hearings but does not move judicial or legislative outcomes because rational basis review treats the rationale as sufficient regardless of empirical truth.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, scholarly_consensus, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, congress).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates constitutional authority between courts and Congress: courts defer to Congress on copyright term-setting, resolving ambiguity in the Constitution's text by deferring to legislative judgment. This enables unified policy rather than fractured judicial rules. The coordination solves the institutional problem of who decides copyright policy when the Constitution is ambiguous.
% TRANSFER_FUNCTION: Transfers public-domain entry rights (from public to copyright holders) and constitutional interpretive authority (from courts to Congress). Every term extension postpones public domain entry by the extension period; every unchallenged extension affirms Congress's power to redefine 'limited Times' through practice.
% ABSENT_VOICES: Open-access advocates, public-domain scholars, and derivative creators have scholarly and political voice but lack standing in copyright litigation. Authors who would benefit from shorter terms but are not themselves copyright holders are excluded from the negotiation. The exclusion is structural (lack of standing) and institutional (copyright policy is dominated by incumbent-holder lobbying).
% DISAPPEARANCE_RATIONALE: If judicial deference vanished overnight, courts would face the question 'what does limited Times mean?' with no institutional guidance. The outcome would depend on whether courts rediscover a constitutional floor or defer to Congress without the rational basis formalism. Public domain entry might accelerate, or courts might simply apply stricter scrutiny and strike down only the most egregious extensions. The world does not rearrange entirely because copyright policy is partially path-dependent on incumbent holdings, but the institutional mechanism shifts radically.
% FOUNDING_PROBLEM: Copyright requires institutional rules for term length; the Constitution grants Congress power but does not specify a ceiling. Courts initially enforced reasonableness limits but gradually deferred to Congress as rational basis doctrine consolidated. The founding problem is how to resolve constitutional ambiguity about 'limited Times' when courts and Congress have different institutional competencies.
% FOUNDING_PROBLEM_CORROBORATION: Congress attests the founding problem is solved: rational basis review enables predictable, legislatively driven policy. Copyright-holder industry attests the problem is solved by deference. Open-access advocates and information-law scholars attest the founding problem is unresolved and that judicial deference masks a real constitutional question. Empirical economists attest that the incentive rationale (Congress's standard legislative justification) is weak beyond moderate terms. Outside corroboration comes from constitutional scholars arguing deference is appropriate (Powell, Sunstein) and from scholars arguing it masks a real constitutional constraint (Lessig, Samuelson).
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.48) because the constraint enables extraction of public-domain postponement but does not itself perform extraction — Congress performs the extraction via legislation, and courts perform the deference that permits it. Extraction is neither high (courts do not mandate extension; the mechanism is permissive, not coercive) nor negligible (the deference structure systematically biases outcomes toward longer terms). Suppression is below-moderate (0.35) because the constraint permits public objection, legislative debate, and constitutional advocacy — the suppression is structural opacity (what does 'limited' mean?) rather than coercive silencing. Theater is moderate-to-high (0.42) because the rational basis formalism performs a constitutional-deference role while masking the institutional shift: courts go through the rational basis motions, but the outcome is predetermined because virtually any legislative rationale for copyright extension satisfies rational basis. The measurement series shows extractiveness and theater both rising slowly over the interval, indicating that deference practice hardens and becomes more institutionalized over time — the constraint accumulates extractive force as the practice normalizes. Accessibility collapse is moderate (0.58): alternatives to copyright extension exist (direct subsidies, prize systems) but are politically suppressed and institutionally locked out by copyright-expansion norms; the collapse is neither complete nor absent. Resistance is moderate-high (0.62): the public domain movement, open-access advocates, and some scholarly commentary mount persistent resistance, but the structural force of the deference doctrine outpaces the resistance.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's seat, the constraint is favorable institutional discretion — it is empowering, not extractive. From the public domain seat, the same structure is a mechanism of extraction and exclusion. From the courts' seat, the constraint is a doctrinal structure that manages institutional competence — courts defer because they lack separation-of-powers authority to second-guess legislative policy judgments. From the constitutional-text seat, the constraint represents a drift away from the original fixity toward living interpretation. The engine should compute Congress as near-beneficiary (low d), the public domain and constitutional text as targets (high d), and courts as neutral-to-analytical.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority (d near 0.2, beneficiary): Congress receives discretion as a structural good — the deference doctrine allocates decision-making authority upward to the legislature. Courts are not beneficiaries; they are the mechanism of the constraint, neither collecting from it nor bearing its costs — their seat is analytical, not directionality-indexed. Constitutional fixity (d near 0.9, victim): the 'limited Times' text is the structural target. Under the deference reading, the constitutional floor (if any) is unconstrained; the text loses binding force; meaning drifts with legislative will. Public domain interests (d near 0.8, victim): each term extension postpones public domain entry, a direct cost borne by public archives, derivative creators, and historical continuity. The directionality is extreme at the public domain seat: the constraint's operation is precisely the mechanism that extracts public-domain rights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Q6) is whether copyright requires incentive-maximization via term extension or whether the constitutional 'limited Times' imposes an enforceable ceiling. This reading claims the problem is resolved through judicial deference: courts will not enforce any constitutional ceiling, so Congress effectively defines 'limited.' The mandatrophy question is whether deference is still justified now that term extensions have become routine and the incentive rationale is empirically contestable. If deference was meant to be temporary while constitutional theory clarified, it may have become inertial (piton). If deference is the intended endstate, the constraint is stable and mandatrophy is absent. The measurement series suggests theater is rising (rational basis review is increasingly performative), which signals potential mandatrophy: the doctrinal form persists while the functional justification erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_deference_scope_ambiguity,
    'Does ''rational basis review'' applied to copyright term extension truly permit Congress unfettered discretion, or does it mask a substantive constitutional floor that rational basis itself enforces?',
    'Hypothetical case: if Congress extended copyright to perpetuity minus one day with a rationale that satisfied rational basis (e.g., ''incentivizes creation''), would courts apply the floor mechanically or rediscover a constitutional limit? Empirical signal: whether any future term extension is ever struck down, or whether rational basis proves to be a verbal form with no teeth.',
    'If deference is truly unconstrained, this reading instantiates a transient scaffold where judicial review is theater — the constraint becomes enclosure-enabling without enclosure-resistance. If a floor exists but is unarticulated, the reading misconstrues the operative constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_deference_scope_ambiguity, conceptual, 'Whether judicial deference to copyright term extension has actual constitutional limits or is functionally unreviewable discretion.').

omega_variable(
    reading_kernel_identity,
    'Is this reading grounded in the Constitution''s text (''limited Times'') or in judicial interpretation of what constitutionality permits?',
    'Textual analysis: ''limited Times'' linguistically constrains copyright. The reading''s core claim is that judicial deference permits Congress to define ''limited'' expansively without invalidation. The kernel (copyright_constitutional_mandate) is the text itself; this reading interprets how that text operates under rational basis review.',
    'If the reading is correctly identified as an interpretation of the kernel rather than a reading of the kernel itself, the constraint might be reframed as ''rational basis review applied to copyright term'' rather than as a kernel reading. If it is a genuine reading, the kernel is the contested meaning of ''limited Times,'' and this reading claims courts defer to Congress on meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether this constraint is a reading of the copyright clause or an interpretation of judicial review standards applied to the clause.').

omega_variable(
    scaffold_sunset_ambiguity,
    'Is this constraint genuinely transitional (scaffold), or is judicial deference itself the intended permanent endstate?',
    'Legislative history and jurisprudential trajectory: if Congress or courts ever articulate that deference is meant to be temporary pending a clearer constitutional theory, sunset is real. If deference is the endorsed baseline, the constraint is a piton or snare, not a scaffold.',
    'If genuinely transitional, the constraint''s extraction is justified as temporary coordination cost pending institutional learning. If permanent, the extraction persists without sunset rationale and reclassifies as snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_ambiguity, preference, 'Whether judicial deference to copyright term is meant as a transition toward clearer constitutional theory or as the intended endstate.').

omega_variable(
    sibling_reading_structural_overlap,
    'The public_scaffold_reading and judicial_ambiguity_reading both employ ''scaffold'' framing — does judicial deference actually differ from claiming copyright is temporarily extended as a scaffolding device?',
    'Conceptual: this reading asserts that the constitutional meaning is ambiguous and courts defer. The public_scaffold_reading asserts that constitutional meaning is fixed (copyright is temporary) but Congress continually renews it as a scaffolding device for public-good production. Different kernels?: if so, they are separate constraints. Same kernel?: if the only difference is whether ambiguity or continuous renewals explain extended terms, they may be the same constraint viewed from different framings (semantic/jurisprudential vs. functional/institutional).',
    'If the readings are structurally identical, decompose or merge. If distinct, clarify the kernel divergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_structural_overlap, conceptual, 'Whether judicial deference and public-good scaffolding are distinct readings of the same kernel or misidentified as one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(copy_tr_t0, observed).
narrative_ontology:measurement(copy_tr_t8, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(copy_tr_t8, observed).
narrative_ontology:measurement(copy_tr_t16, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement_basis(copy_tr_t16, observed).
narrative_ontology:measurement(copy_tr_t24, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(copy_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(copy_be_t0, observed).
narrative_ontology:measurement(copy_be_t8, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement_basis(copy_be_t8, observed).
narrative_ontology:measurement(copy_be_t16, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement_basis(copy_be_t16, observed).
narrative_ontology:measurement(copy_be_t24, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(copy_be_t24, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(copyright_constitutional_mandate__judicial_ambiguity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_public_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the copyright_constitutional_mandate kernel. The corporate_enclosure_reading and public_scaffold_reading are siblings instantiating the same kernel under different interpretive frames. All three stories are linked by network.affects_constraints. Each reading produces a different beneficiary structure, directionality profile, and classification, but they share the same kernel text and operate on the same institutional terrain. Decomposition follows ε-invariance: the three readings produce meaningfully different ε values (judicial_ambiguity is moderate; corporate_enclosure is high; public_scaffold is low) because they answer differently the question 'what does copyright's operation extract under this reading's interpretation?'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
