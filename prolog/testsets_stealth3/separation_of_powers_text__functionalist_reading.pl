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
 *   human_readable: Functionalist Separation-of-Powers Settlement: Intelligible-Principle Delegation and Judicial Deference
 *   domain: constitutional law/political theory/administrative law
 *
 * SUMMARY:
 *   A standing constitutional settlement treats the separation of powers as a
 *   flexible framework: Congress may transfer lawmaking judgment to agencies
 *   so long as statutes supply an intelligible principle, agencies exercise
 *   combined rulewriting, enforcement, and adjudicative functions
 *   legitimately, and courts accept expert statutory readings rather than
 *   relitigating meaning. This file instantiates the functionalist_reading of
 *   the separation_of_powers_text kernel as a clean, epsilon-invariant
 *   constraint; the formalist_reading and unitary_executive_reading siblings
 *   are separate constraint files linked through the network block, not
 *   folded into this one. Epsilon's referent is the standing
 *   delegation-plus-deference arrangement itself, assessed by this reading's
 *   own lights — not the strict-boundary regime this reading rejects. The
 *   claim/metric gap is deliberate: the reading CLAIMS a
 *   coordination-weighted hybrid while the authored metrics record moderate
 *   extraction with a recent partial reversal at the deference edge; the
 *   engine measures that divergence.
 *
 * KEY AGENTS:
 *   - federal_administrative_agencies: primary beneficiary (institutional/identity_locked) — hold delegated lawmaking power and receive interpretive deference
 *   - sitting_presidents: beneficiary with agenda-setting reach (powerful/arbitrage) — direct the apparatus delegation empowers
 *   - incumbent_members_of_congress: beneficiary (organized/immediate) — shed detailed lawmaking and diffuse blame
 *   - congress_as_institution: primary target (institutional/trapped) — cedes legislative detail and oversight grip
 *   - federal_judiciary: target and enforcement administrator (institutional/trapped) — polices a framework that withdraws interpretive authority from its own dockets
 *   - electorate_principals: diffuse target (powerless/trapped) — the accountability chain lengthens beyond their tracing
 *   - regulated_industries: mixed payer/beneficiary (organized/constrained) — bear compliance costs; large players reshape the rules they bear
 *   - formalist_jurists_and_scholars: excluded voice (moderate/constrained) — would bar delegation outright; confined to dissents and journals
 *   - administrative_law_academy: analytical observer (analytical/analytical) — maps the gap between stated tests and operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.48).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.38).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Separation-of-Powers Settlement: Intelligible-Principle Delegation and Judicial Deference").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional law/political theory/administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '1a8c9c8c-aa3c-4544-bc79-bb0662668a16').
narrative_ontology:cs_kernel_codification('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', fixed_text).
narrative_ontology:cs_authority_grounding('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', lineage).
narrative_ontology:cs_interpretation_layer_present('1a8c9c8c-aa3c-4544-bc79-bb0662668a16').
narrative_ontology:cs_reading_relation('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', separation_of_powers_text__formalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', foundational, intelligible_principle_delegation_constitutional).
narrative_ontology:cs_axiom_status(intelligible_principle_delegation_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', intelligible_principle_delegation_constitutional, instrumental).
narrative_ontology:cs_axiom('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', foundational, overlapping_authority_legitimate_under_checks).
narrative_ontology:cs_axiom_status(overlapping_authority_legitimate_under_checks, holdable).
narrative_ontology:cs_axiom_grounding('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', overlapping_authority_legitimate_under_checks, deontological).
narrative_ontology:cs_axiom('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', secondary, agency_statutory_interpretation_binding_on_courts).
narrative_ontology:cs_axiom_status(agency_statutory_interpretation_binding_on_courts, overridden).
narrative_ontology:cs_axiom_grounding('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', agency_statutory_interpretation_binding_on_courts, conventional).
narrative_ontology:cs_reference_frame('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', flexible_equilibrium_with_checked_overlap).
narrative_ontology:cs_drift_state('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', contemporary_post_chevron_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a8c9c8c-aa3c-4544-bc79-bb0662668a16', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, sitting_presidents).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, incumbent_members_of_congress).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, congress_as_institution).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, electorate_principals).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulated_industries).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, judicial_deference_to_agency_expertise).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, administrative_necessity_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the detailed rules that carry general statutes into effect. Their authority exists only because Congress delegates and courts accept expert readings; their missions, budgets, and professional identities are built on that delegated mandate. They cannot relocate their function elsewhere — dismantling the arrangement would dismantle them.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_administrative_agencies, beneficiary,
    institutional, generational, identity_locked, national).

% Appoint agency leadership, issue executive orders, and direct enforcement priorities across the machinery that delegation created. Each administration inherits and reshapes the arrangement; the office's leverage over the apparatus persists across occupants.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, sitting_presidents, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, sitting_presidents, agenda_setter).

% Vote on general statutes, claim credit for popular programs, and avoid ownership of contested technical choices that agencies later make. Individually they could press for detailed legislation but face electoral incentives to keep discretion distant and blame diffuse.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, incumbent_members_of_congress, beneficiary,
    organized, immediate, constrained, national).

% Cedes ever-finer lawmaking judgment to agencies while retaining formal oversight tools that lag behind the volume of rulemaking. Its committees cannot reconstruct, statute by statute, the detail it has transferred; the institution's grip on policy content thins even as its members individually profit from the distance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress_as_institution, payer,
    institutional, generational, trapped, national).

% Authors and polices the doctrinal framework — the intelligible-principle test and the deference standards — while that same framework routes statutory interpretation away from its dockets. Judges dissent against deference, teach alternatives, and periodically narrow the doctrine, but the institution cannot resign from administering it.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, federal_judiciary, agenda_setter).

% Elect every lawmaker and the president but cannot trace which body made a given binding rule, vote on administrators, or readily attribute outcomes across the delegation chain. Their recourse runs through the very institutions whose clarity the arrangement dilutes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, electorate_principals, payer,
    powerless, generational, trapped, national).

% Comply with agency rules, litigate their bounds, and lobby the agencies that write them. Large repeat players gain privileged access and sometimes preferred treatment; smaller entrants bear the compliance load with less voice. Exiting the jurisdiction means exiting the market.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_industries, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, regulated_industries, beneficiary).

% Argue that the constitutional text bars transferring legislative judgment and that accepting expert readings abdicates judicial duty. They publish, dissent, and mentor, and periodically see their arguments adopted at the margins, but they hold no seat in the governing doctrinal coalition.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_jurists_and_scholars, excluded,
    moderate, generational, constrained, national).

% Studies the arrangement from outside participation: measures the gap between announced tests and operational outcomes, tracks the doctrine's migration across eras, and supplies the vocabulary both defenders and critics use.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a capacity-and-expertise problem: a bicameral legislature of generalists cannot specify technical content for a continental economy, so general statutes are implemented by specialized bodies, and deference rules tell courts when to accept expert readings instead of relitigating meaning case by case.
% TRANSFER_FUNCTION: Moves lawmaking discretion from Congress to agencies, statutory interpretation from courts to agencies, and implementation labor from the legislature to the executive branch; correspondingly moves accountability visibility away from voters, who face a longer, dimmer causal chain to the officials making binding rules.
% ABSENT_VOICES: Formalist jurists and scholars, unitary-execution theorists, and citizens pressing for traceable accountability sit outside the governing coalition; their objections surface as dissents, petitions, and commentary rather than as votes inside the framework that allocates authority.
% DISAPPEARANCE_RATIONALE: Overnight replacement of the flexible framework with strict boundary enforcement would void the legal basis of the administrative state: agency rulemaking authority rests on delegated statutes upheld under this reading. Drug approvals, environmental limits, benefits administration, and financial supervision would lose their issuing authority until Congress redrafted thousands of statutes in enforceable detail — a reconstruction measured in decades, if achievable at all.
% FOUNDING_PROBLEM: Reconcile republican accountability with governable scale: the founders separated powers to prevent concentrated tyranny, and the twentieth century added the problem that a national industrial economy needed uniform, technical regulation a part-time, geographically rooted legislature could not supply. The functionalist settlement answers both by letting powers overlap under mutual checks.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: formalist critics concede the capacity problem while disputing the remedy; regulated industries fund litigation against agencies yet consistently demand stable, uniform rules only agencies can issue; Congresses of both parties keep choosing delegation even when drafting in detail is available; comparative constitutional systems with differently structured legislatures converge on similar delegation volumes.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48 for the standing delegation-plus-deference arrangement as this reading assesses it: substantial transfer of lawmaking and interpretive authority, discounted by the reading's own judgment that much of the transfer is the price of governable scale. Suppression is 0.38 — enforcement runs through doctrine rather than coercion, and its requirement has eased since the deference peak. Theater_ratio is 0.42: the intelligible-principle test has struck down no delegation statute since 1935 and largely performs limitation, while the major-questions line has begun doing real limiting work again. Accessibility_collapse sits at 0.45 because the formalist alternative never disappears — it persists in dissents, scholarship, and periodic majority opinions — and resistance is 0.58 because opposition is organized, published, and currently ascendant. The measurement series share one eight-point grid (t=0..98, mapping 1928–2026): extractiveness and suppression rise to a deference-era peak near t=56–70 and partially reverse afterward, while theater climbs as the nondelegation test goes dormant and eases slightly once the Court re-engages limits.
 *
 * PERSPECTIVAL GAP:
 *   The same body computes oppositely from its two seats: incumbent members experience the arrangement as blame-shedding coordination while congress_as_institution experiences thinning control over policy content. The judiciary occupies a stranger position still — it administers the deference framework whose operation withdraws interpretive authority from its own dockets, so its seat mixes administrator and payer. Agencies, from inside, experience the arrangement not as a constraint at all but as their constituting condition; what they receive is invisible as extraction from that seat. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place federal_administrative_agencies, sitting_presidents, and incumbent_members_of_congress near the subsidized end; victim declarations place congress_as_institution, federal_judiciary, electorate_principals, and regulated_industries toward the target end. Exit modulation sharpens the spread: agencies are identity_locked (their missions constitute them, pinning them at the beneficiary pole), presidents hold arbitrage-grade flexibility, while trapped institutional targets and the powerless diffuse electorate sit nearer the full-target end. No directionality_overrides are authored: the derivation chain already separates the two same-body seats (incumbent members as beneficiaries versus congress_as_institution as payer) because they carry distinct role and exit declarations, and the judiciary's dual position is expressed through secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconcile accountable government with governable scale — remains live, so the arrangement has not outlived its mandate and mandatrophy is not resolved. The tangled_rope claim is what keeps both halves visible: a pure-coordination reading would erase the accountability and institutional-authority costs the payer seats bear, while a pure-extraction reading would erase the capacity function that every corroborating source outside the beneficiary set attests. The recent partial reversal (deference narrowed, delegation intact) is drift at the arrangement's edge, not obsolescence of its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the separation_of_powers_text kernel (reading: functionalist_reading). How would instantiating the formalist_reading or unitary_executive_reading siblings change the structural picture?',
    'Track which reading commands a durable Supreme Court majority across appointment cycles; the sibling files instantiate the alternatives with their own epsilon, beneficiaries, and victims.',
    'Under the formalist sibling, epsilon rises sharply and the victim set expands to every agency-dependent program; under the unitary sibling, the victim set shifts to independent-agency insulation and the beneficiary set contracts toward the presidency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest among three readings; this file instantiates only the functionalist reading.').

omega_variable(
    nondelegation_revival_trajectory,
    'Will the Court revive enforceable nondelegation limits, converting the dormant intelligible-principle test from performance into constraint?',
    'Watch majority composition and the concurring opinions signaling willingness to articulate a new test: a five-vote bloc would convert the theater_ratio series into falling extractive headroom.',
    'Revival would raise epsilon for the delegation half of the arrangement, shrink the beneficiary set, and push the computed type toward the formalist sibling''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nondelegation_revival_trajectory, empirical, 'Probability that the dormant nondelegation doctrine revives within the current appointment cycle.').

omega_variable(
    agency_capture_receipt,
    'Does the extraction that nominally accrues to federal_administrative_agencies pass through to organized private interests that shape rulemaking, so that the receipt seat is a conduit rather than a terminus?',
    'Compare rule outcomes against comment-period participation concentration and revolving-door placement data across agencies and sectors.',
    'If capture is systematic, gain_flow shifts from the agency seat toward organized industry seats and the arrangement reads as extraction routed through public authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capture_receipt, empirical, 'Whether delegated authority is exercised for agency-institutional benefit or captured by organized private players.').

omega_variable(
    accountability_diffusion_cost,
    'Is the accountability loss borne by electorate_principals a real welfare cost of the arrangement, or is it offset by presidential electoral control over the administrative apparatus?',
    'Compare responsiveness of agency outputs to electoral turnover versus to organized-commenter pressure; measure voter attribution accuracy for agency-made rules.',
    'If offset, the diffuse-target directionality is overstated and epsilon falls toward the coordination floor; if not, the accountability transfer is uncompensated extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_diffusion_cost, conceptual, 'Whether democratic accountability survives the delegation chain well enough to price the diffusion cost.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the constitutional text itself, or the accumulated doctrinal canon that does the operative work while the text stays under-determined?',
    'Test whether interpretive change ever turns on the text''s clauses or always on canonical case lines: if the text is never the operative margin, the canon is the functioning kernel and cs_structure should be re-declared accordingly.',
    'Re-framing onto the canon would move kernel_codification from fixed_text toward distributed and change which drift events count as codification_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing choice between text-as-kernel and canon-as-kernel changes the commitment-system classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 98).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sopt_functionalist_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t0, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t14, separation_of_powers_text__functionalist_reading, theater_ratio, 14, 0.26).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t14, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t28, separation_of_powers_text__functionalist_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t28, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t42, separation_of_powers_text__functionalist_reading, theater_ratio, 42, 0.38).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t42, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t56, separation_of_powers_text__functionalist_reading, theater_ratio, 56, 0.45).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t56, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t70, separation_of_powers_text__functionalist_reading, theater_ratio, 70, 0.47).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t70, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t84, separation_of_powers_text__functionalist_reading, theater_ratio, 84, 0.45).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t84, observed).
narrative_ontology:measurement(sopt_functionalist_tr_t98, separation_of_powers_text__functionalist_reading, theater_ratio, 98, 0.42).
narrative_ontology:measurement_basis(sopt_functionalist_tr_t98, observed).

% Extraction over time
narrative_ontology:measurement(sopt_functionalist_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(sopt_functionalist_be_t0, observed).
narrative_ontology:measurement(sopt_functionalist_be_t14, separation_of_powers_text__functionalist_reading, base_extractiveness, 14, 0.34).
narrative_ontology:measurement_basis(sopt_functionalist_be_t14, observed).
narrative_ontology:measurement(sopt_functionalist_be_t28, separation_of_powers_text__functionalist_reading, base_extractiveness, 28, 0.42).
narrative_ontology:measurement_basis(sopt_functionalist_be_t28, observed).
narrative_ontology:measurement(sopt_functionalist_be_t42, separation_of_powers_text__functionalist_reading, base_extractiveness, 42, 0.47).
narrative_ontology:measurement_basis(sopt_functionalist_be_t42, observed).
narrative_ontology:measurement(sopt_functionalist_be_t56, separation_of_powers_text__functionalist_reading, base_extractiveness, 56, 0.55).
narrative_ontology:measurement_basis(sopt_functionalist_be_t56, observed).
narrative_ontology:measurement(sopt_functionalist_be_t70, separation_of_powers_text__functionalist_reading, base_extractiveness, 70, 0.57).
narrative_ontology:measurement_basis(sopt_functionalist_be_t70, observed).
narrative_ontology:measurement(sopt_functionalist_be_t84, separation_of_powers_text__functionalist_reading, base_extractiveness, 84, 0.53).
narrative_ontology:measurement_basis(sopt_functionalist_be_t84, observed).
narrative_ontology:measurement(sopt_functionalist_be_t98, separation_of_powers_text__functionalist_reading, base_extractiveness, 98, 0.48).
narrative_ontology:measurement_basis(sopt_functionalist_be_t98, observed).

% Suppression requirement over time
narrative_ontology:measurement(sopt_functionalist_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(sopt_functionalist_su_t0, observed).
narrative_ontology:measurement(sopt_functionalist_su_t14, separation_of_powers_text__functionalist_reading, suppression_requirement, 14, 0.3).
narrative_ontology:measurement_basis(sopt_functionalist_su_t14, observed).
narrative_ontology:measurement(sopt_functionalist_su_t28, separation_of_powers_text__functionalist_reading, suppression_requirement, 28, 0.36).
narrative_ontology:measurement_basis(sopt_functionalist_su_t28, observed).
narrative_ontology:measurement(sopt_functionalist_su_t42, separation_of_powers_text__functionalist_reading, suppression_requirement, 42, 0.42).
narrative_ontology:measurement_basis(sopt_functionalist_su_t42, observed).
narrative_ontology:measurement(sopt_functionalist_su_t56, separation_of_powers_text__functionalist_reading, suppression_requirement, 56, 0.5).
narrative_ontology:measurement_basis(sopt_functionalist_su_t56, observed).
narrative_ontology:measurement(sopt_functionalist_su_t70, separation_of_powers_text__functionalist_reading, suppression_requirement, 70, 0.48).
narrative_ontology:measurement_basis(sopt_functionalist_su_t70, observed).
narrative_ontology:measurement(sopt_functionalist_su_t84, separation_of_powers_text__functionalist_reading, suppression_requirement, 84, 0.43).
narrative_ontology:measurement_basis(sopt_functionalist_su_t84, observed).
narrative_ontology:measurement(sopt_functionalist_su_t98, separation_of_powers_text__functionalist_reading, suppression_requirement, 98, 0.38).
narrative_ontology:measurement_basis(sopt_functionalist_su_t98, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'separation of powers' — covers three structurally distinct constraints: the formalist reading (strict impermeable boundaries, no delegation), this functionalist reading (flexible overlap, intelligible-principle delegation, deference), and the unitary-executive reading (all executive power vested in the president, independent agencies illegitimate). Their epsilon values diverge widely because each reading prices the same text differently; they share a kernel but not a constraint. This file links both siblings via affects_constraints. Pressure between readings runs through appointment politics and doctrinal precedent: the formalist sibling is logically incompatible with this reading's delegation premise within any single framework, while the unitary sibling competes as a live position that a hybrid framework (broad delegation under tight presidential supervision) could partly accommodate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
