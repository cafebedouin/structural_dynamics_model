% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Basic Laws: Court Invalidation Binding on Knesset
 *   domain: constitutional_law/judicial_review
 *
 * SUMMARY:
 *   Israel's Basic Laws, adopted in 1992 with no prior written constitution,
 *   were interpreted by the Supreme Court starting in the mid-1990s as
 *   establishing a constitutional super-layer that binds ordinary
 *   legislation. Under this judicial supremacy reading, the Court holds
 *   authority to invalidate Knesset legislation it deems contradictory to the
 *   interpreted Basic Laws, and that invalidation is binding—the Knesset
 *   cannot override it through ordinary legislative means. This reading
 *   transfers veto authority from electoral majorities to the Court. The
 *   constraint is CLAIMED as tangled_rope: genuine coordination function
 *   (constitutional protection, minority-rights safeguard) plus asymmetric
 *   extraction (Knesset sovereignty subordinated, majoritarian legislation
 *   vulnerable to nullification). The authored metrics reflect substantially
 *   extractive, actively enforced operation requiring ongoing suppressive
 *   force to maintain the Court's interpretation monopoly and bind
 *   legislative action. This is ONE READING of a contested kernel
 *   (basic_law_interpretive_boundary); sibling readings instantiate
 *   parliamentary sovereignty and balanced contestation framings—they are
 *   separate constraint stories with different ε values and beneficiary
 *   structures.
 *
 * KEY AGENTS:
 *   - Supreme Court: institutional agenda-setter with power to invalidate legislation; benefits from interpretive authority and institutional autonomy
 *   - Knesset: organized legislative body subject to judicial veto; bears cost of invalidation and constraint on sovereignty
 *   - Rights-claimants: moderate-power beneficiaries who gain veto pathway through litigation
 *   - Majoritarian coalitions: organized electoral winners who face nullification of mandated legislation
 *   - Competing interpreters (foreign courts, academia): excluded from binding authority; Court monopoly maintained through suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy over Basic Laws: Court Invalidation Binding on Knesset").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/judicial_review").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '709159c3-cd68-4d46-8976-eb48f7c2ca0a').
narrative_ontology:cs_kernel_codification('709159c3-cd68-4d46-8976-eb48f7c2ca0a', formalized).
narrative_ontology:cs_authority_grounding('709159c3-cd68-4d46-8976-eb48f7c2ca0a', extraction).
narrative_ontology:cs_interpretation_layer_present('709159c3-cd68-4d46-8976-eb48f7c2ca0a').
narrative_ontology:cs_reading_relation('709159c3-cd68-4d46-8976-eb48f7c2ca0a', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('709159c3-cd68-4d46-8976-eb48f7c2ca0a', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('709159c3-cd68-4d46-8976-eb48f7c2ca0a', foundational, judicial_interpretive_supremacy_over_basic_laws).
narrative_ontology:cs_axiom_status(judicial_interpretive_supremacy_over_basic_laws, holdable).
narrative_ontology:cs_axiom_grounding('709159c3-cd68-4d46-8976-eb48f7c2ca0a', judicial_interpretive_supremacy_over_basic_laws, deontological).
narrative_ontology:cs_axiom('709159c3-cd68-4d46-8976-eb48f7c2ca0a', foundational, invalidation_authority_binding_on_legislature).
narrative_ontology:cs_axiom_status(invalidation_authority_binding_on_legislature, holdable).
narrative_ontology:cs_axiom_grounding('709159c3-cd68-4d46-8976-eb48f7c2ca0a', invalidation_authority_binding_on_legislature, deontological).
narrative_ontology:cs_reference_frame('709159c3-cd68-4d46-8976-eb48f7c2ca0a', judicial_interpretive_supremacy_framework).
narrative_ontology:cs_drift_state('709159c3-cd68-4d46-8976-eb48f7c2ca0a', contemporary_legislative_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('709159c3-cd68-4d46-8976-eb48f7c2ca0a', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants_via_litigation).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_sovereignty).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, majoritarian_legislation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, administrative_bodies).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, public_constituency).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, majoritarian_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, administrative_bodies).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, public_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a super-constitutional floor; reviews all legislation for compatibility with the interpreted Basic Laws; invalidates legislation the Court deems contradictory. The Court frames this as protecting the constitutional order from majoritarian erosion. The Court administers the invalidation machinery and sets the scope of what counts as 'Basic Law territory' through interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Holds formal legislative authority but operates under constraint that any law the Supreme Court deems to contradict an interpreted Basic Law can be nullified. The Knesset cannot override a judicial invalidation through ordinary legislative means without triggering another Court challenge. Its sovereignty over the Basic Law interpretation is subordinated to the Court's interpretive authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_sovereignty, payer,
    organized, biographical, constrained, national).

% Gain a veto mechanism against legislation they claim violates Basic Law protections: they can litigate, the Court can invalidate, and the law falls without legislative override power. This gives individual and group claimants a pathway to block majoritarian legislation without needing electoral power. They do not administrate the system but reap its protective benefits.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants_via_litigation, beneficiary,
    moderate, biographical, mobile, national).

% Enact legislation reflecting electoral mandates but face the risk that the Supreme Court will declare it incompatible with an interpreted Basic Law and nullify it. The veto power shifts from the electoral majority to the nine-member Court. They bear the cost of legislative invalidation and cannot recover through ordinary legislative means.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, majoritarian_coalitions, payer,
    organized, biographical, constrained, national).

% Must implement only legislation that survives Court review; they are bound by invalidation verdicts and cannot execute laws the Court has nullified. They also benefit insofar as the Court's invalidations may constrain interference with their independence or operations. Caught between parliamentary mandates and judicial constraints.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, administrative_bodies, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, administrative_bodies, beneficiary).

% International courts, foreign constitutional precedent, and academic constitutional theory produce competing interpretations of what rights or limits the Basic Laws should protect. These interpretations are excluded from binding weight; only the Supreme Court's interpretation carries enforcement authority. Their exclusion is maintained by the Court's monopoly on invalidation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, competing_interpretation_authorities, excluded,
    powerful, generational, trapped, national).

% Receives protection of rights the Court interprets as Basic Law rights (beneficiary side), but also faces invalidation of legislation they elected representatives to enact (payer side). The constraint transfers veto power from electoral majorities to the Court, which some view as rights protection and others as counter-majoritarian usurpation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, public_constituency, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, public_constituency, payer).

% Produce analysis of the constitutionality and legitimacy of the judicial review structure. They interpret the readings of the constraint and contest whether judicial supremacy or parliamentary sovereignty is the correct framing. Their analysis informs public and judicial discourse but carries no binding force.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, academic_and_policy_commentators, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, authoritative institutional seat (the Supreme Court) that interprets the Basic Laws and applies them as binding constraints on ordinary legislation. Solves the coordination problem of how a plural, politically divided society can maintain constitutional fidelity and protect minority rights against majoritarian legislation.
% TRANSFER_FUNCTION: Transfers veto authority over legislation from the electoral majority (the Knesset and the voting public) to a non-elected, insulated institutional body (the Supreme Court). Legislation invalidated by the Court cannot be executed, regardless of parliamentary votes. Rights-claimants gain standing to trigger this veto through litigation.
% ABSENT_VOICES: Parliamentary sovereignty advocates, foreign constitutional interpreters, and ordinary citizens who view the Court's veto as illegitimate interference with democratic will are structurally excluded from binding participation in Basic Law interpretation. They can litigate and publish criticism, but the Court holds exclusive binding authority.
% DISAPPEARANCE_RATIONALE: If judicial supremacy over Basic Laws disappeared and the Knesset reasserted exclusive sovereign authority to interpret and amend the Basic Laws, legislation now vulnerable to invalidation would take effect immediately, the veto pathway through litigation would close, and constitutional protections would become contingent on parliamentary majorities rather than judicial interpretation. Rights protections would shift from judicially enforceable to legislatively contingent.
% FOUNDING_PROBLEM: A uni-cameral, sovereign parliament with no written constitution prior to the 1990s had no institutional check against majoritarian erosion of fundamental rights and could legislate changes to the character of the state without constitutional constraint. The Basic Laws (formally adopted in 1992 with judicial enforcement authority added through Court decisions) were framed as establishing a constitutional floor.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and rights-advocacy organizations attest the founding problem is live: constitutional protection against majoritarian legislation remains necessary. The Knesset, elected coalitions, and parliamentary sovereignty advocates attest the founding problem has been superseded by democratic norm consolidation and that the current constraint reflects the Court's institutional power-grab rather than a structural necessity. International comparisons from democracies without judicial review (Canada pre-1982, Israel pre-1992, UK throughout) and democracies with weaker review (Germany with constitutional limits on amendment) provide external perspectives; neither conclusively resolves the contest.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.42 (moderate) at interval start because the judicial review structure is contested and not uniformly enforced against all legislation—only legislation the Court deems to cross the Basic Law boundary is nullified. By interval midpoint (t=15), extractiveness rises to 0.61 as the Court expands the scope of what counts as 'Basic Law territory' through aggressive interpretation, capturing more legislation in the invalidation net. It plateaus at 0.68 by t=20-35, indicating the system has reached equilibrium: the Court's interpretation scope is stable, the Knesset has adapted legislative strategy (drafting to anticipate Court review), and the veto mechanism is routinely deployed but not accelerating. Suppression requirement rises from 0.55 to 0.72 and plateaus, reflecting the enforcement infrastructure needed to maintain the Court's monopoly on invalidation authority: the Court must suppress competing interpretations (parliamentary, international, academic), suppress legislative attempts to circumvent review, and suppress political pressure to limit judicial authority. Theater ratio rises from 0.28 to 0.42: the Court performs constitutional guardianship and rights protection (genuine function, ~58%), but an increasing share of suppression activity is devoted to maintaining institutional power (legislative exclusion, interpretation monopoly, suppression of override mechanisms—~42%). The measurement grid is aligned: every metric authored at every time point so temporal analysis has coherent data.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat, the constraint is a necessary constitutional protection against majoritarian erosion of fundamental rights and judicial independence—the reading frames it as a rope coordinating constitutional fidelity. From the Knesset's seat, the same structure is a counter-majoritarian veto that subordinates elected authority to unelected judges—the reading frames it as a snare extracting legislative sovereignty. Rights-claimants see coordination (their rights are protected), while majoritarian coalitions see extraction (their electoral mandate is overridden). The engine should compute these divergences from the structural data: Court and Knesset occupy different institutional power positions with fundamentally opposed interests in interpretation authority. The authored claimed_type (tangled_rope) reflects the analyst's judgment that both functions (coordination + extraction) are structurally present; the computed types per seat may diverge substantially.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly to structural positions: the Supreme Court and rights-claimants benefit from the invalidation mechanism; the Knesset and majoritarian legislation bear the costs. The Court's institutional power atom (institutional + arbitrage exit = beneficiary end of d-scale); the Knesset's organized power atom with constrained exit (organized + constrained exit = target end); rights-claimants' moderate power with mobile exit (moderate + mobile = beneficiary end but not as extreme as the Court); majoritarian coalitions' organized power with constrained exit (organized + constrained = target end, similar to Knesset). The directionality differences are stark: the Court gains veto authority, the Knesset loses it. Suppression is high because maintaining this asymmetry requires active enforcement: the Court must suppress competing interpretations, the Knesset must suppress attempts to strip judicial review authority or override invalidations, and both must suppress the other side's institutional claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing majoritarian erosion of constitutional rights) remains contested: the Court and rights-advocates say it is live; parliamentary sovereignty advocates say the founding problem is dead and the current constraint reflects institutional power-grab rather than constitutional necessity. The disappearance verdict (world_rearranges) indicates the constraint is structurally consequential: if judicial supremacy vanished, the legislative landscape would shift immediately as nullified laws took effect, veto pathways closed, and constitutional protection became contingent on parliamentary majorities. However, the mandatrophy question is whether the constraint's persistence depends on active enforcement of the Court's interpretation monopoly (which would make it a piton or snare if the founding problem is dead) or whether it solves a genuine structural coordination problem (which would keep it a rope or tangled_rope). The measurement trajectory is informative: extractiveness and suppression plateau at high levels and do not continue rising, suggesting the system has reached a stable equilibrium rather than accelerating power concentration. If the founding problem were dead, we would expect either decay (the constraint fades as the need for it evaporates) or pure theater (the constraint persists performatively with little actual enforcement). The measured plateau at high suppression suggests genuine enforcement is required to maintain the constraint—consistent with tangled_rope (both coordination and extraction are active), not piton (where enforcement would be theatrical). The theater ratio plateaus at 0.42, indicating a stable split between functional rights protection and performative institutional maintenance. This is consistent with a constraint solving a real problem (protecting rights) while extracting real costs (veto authority), rather than a purely theatrical arrangement. Mandatrophy is not resolved, but the data pattern (plateau rather than decay or acceleration) is consistent with tangled_rope rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (majoritarian erosion of fundamental rights) still a live structural risk in Israeli democracy, or has it been superseded by democratic norm consolidation and parliamentary self-restraint?',
    'Time-series analysis of legislative threats to fundamental rights and institutional independence; survey data on Knesset members'' commitment to constitutional norms; comparative analysis of democracies with and without judicial review—what patterns of rights erosion emerge when judicial veto is absent?',
    'If the founding problem is dead, the constraint''s persistence depends on Court institutional maintenance and suppression of alternatives, making it a piton or snare rather than tangled_rope. If the founding problem is live, the constraint''s extractiveness reflects the real cost of constitutional protection, consistent with tangled_rope. This distinction determines whether mandatrophy has resolved or remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint solves a structural coordination problem or persists as institutional power maintenance.').

omega_variable(
    competing_readings_institutional_lock,
    'Is the judicial supremacy reading self-reinforcing through institutional dynamics (Court interprets to entrench its authority, Knesset''s challenge capacity is weakened by successive nullifications) or is it contestable from alternative institutional positions?',
    'Track successive Court decisions invalidating Knesset attempts to limit judicial authority (e.g., attempts to raise the threshold for invalidation or establish legislative override mechanisms); analyze whether the Court''s interpretation of ''Basic Law territory'' expands in response to legislative challenge.',
    'High self-reinforcement would suggest the reading is institutionally entrenched and alternative readings are effectively foreclosed at the operative level, even if not logically foreclosed. Low self-reinforcement would suggest the readings remain genuinely contestable and could shift with electoral/political change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_readings_institutional_lock, empirical, 'Whether the judicial supremacy reading is locked in by institutional feedback or remains contingent.').

omega_variable(
    basic_law_interpretation_scope_instability,
    'Does the Supreme Court''s interpretation of what constitutes a ''Basic Law violation'' expand over time to capture more legislation in the invalidation net, or does it stabilize at a fixed scope?',
    'Catalogue all judicial review cases and track the scope of what the Court deems to fall under Basic Law protection; measure invalidation rate over time; compare scope statements across Court decisions across years.',
    'If scope expands, extractiveness will continue to rise and the constraint''s impact on Knesset sovereignty will deepen—consistent with a Boltzmann drift signature. If scope stabilizes, the constraint has reached equilibrium—consistent with the measurement plateau observed in this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_law_interpretation_scope_instability, empirical, 'Whether Basic Law interpretation scope is expanding or has stabilized.').

omega_variable(
    reading_foreclosure_empirical_grounding,
    'Which sibling reading (parliamentary sovereignty or balanced contestation) is the judicial supremacy reading in logical/institutional tension with?',
    'Test the logical structure: (1) Can a single institutional framework hold both judicial supremacy AND parliamentary sovereignty (can both be true simultaneously under any coherent rule set)? Answer: no—they assign ultimate authority to mutually exclusive seats. (2) Can a single framework hold judicial supremacy AND balanced contestation (both true simultaneously)? Answer: only if the Court''s authority is bounded and shared, which contradicts supremacy. Forecast: judicial_supremacy forecloses parliamentary_sovereignty (they are logical opposites); judicial_supremacy coexists_with balanced_contestation only if supremacy is reinterpreted as ''institutional expertise within bounded scope'' rather than ultimate authority—i.e., the reading itself must degrade for coexistence to hold.',
    'If judicial_supremacy forecloses parliamentary_sovereignty, the two readings cannot coexist in a single framework and competition between them is a structural fight for authority, not a mere interpretive disagreement. This would shift the engine''s computation of foreclosure from emergent to structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical_grounding, conceptual, 'Logical structure of the contest between judicial supremacy and parliamentary sovereignty readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t35, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(basi_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t35, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(basi_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t35, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(basi_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_process_constraint).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claim_justiciability_boundary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel basic_law_interpretive_boundary. The sibling readings (parliamentary_sovereignty_reading and balanced_contestation_reading) are separate constraint stories with different ε values, beneficiary structures, and foundational axioms. All three stories link via network.affects_constraints to indicate they are competing instantiations of the same kernel. The constraint family decomposes a single legal/political kernel (how are Basic Laws interpreted and enforced?) into three structurally distinct claims with different extraction profiles. This decomposition follows the ε-invariance principle: if a single constraint story would require different ε values depending on which reading is adopted, the story actually represents multiple constraints—one per reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
