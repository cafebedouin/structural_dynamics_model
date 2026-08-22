% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading of the Article II Vesting Clause
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the separation_of_powers_text
 *   kernel: the unitary_executive_reading, on which Article II vests ALL
 *   executive power in a single President, making for-cause removal
 *   protections and 'independent' agency structures constitutionally void.
 *   The constraint is treated as a clean, epsilon-invariant claim: the
 *   standing arrangement under contest is presidential supremacy over every
 *   executive officer, assessed as this reading's own operation.
 *   Constraint-family note (epsilon-invariance decomposition): the colloquial
 *   label 'separation of powers' covers three structurally distinct claims —
 *   the formalist_reading (impermeable boundaries; delegation itself the
 *   target), the functionalist_reading (intelligible-principle flexibility;
 *   insulated agencies legitimate), and this reading. Each is a separate file
 *   with its own epsilon, victim set, and classification; the functionalist
 *   reading authors LOW extraction for the same administrative arrangements
 *   this reading authors as high, because they are different constraints, not
 *   one constraint viewed twice. Assumptions recorded: interval 1980-2025
 *   tracks the modern movement from academic position (Meese-era OLC) to
 *   enforced doctrine (Seila Law, Collins, pending Humphrey's Executor
 *   challenges and governor-removal litigation); all measurement points are
 *   observed events; sampling parameters assumed temperature=1.0.
 *
 * KEY AGENTS:
 *   - incumbent_presidents: agenda-setting beneficiary (institutional/arbitrage) — commands the arrangement and collects its gains
 *   - presidential_appointees: secondary beneficiaries (powerful/mobile) — inherit command levers each administration
 *   - originalist_legal_academy: idea-supply beneficiary (moderate/mobile) — collects influence as the argument advances
 *   - independent_agency_commissioners: primary targets (organized/trapped) — statutory tenure under active invalidation
 *   - federal_reserve_leadership: partially shielded target (institutional/constrained, global scope) — market-enforced credibility
 *   - career_civil_servants: identity-fused targets (moderate/identity_locked) — professional standing converted to at-will exposure
 *   - article_i_congress: structural loser (institutional/constrained) — design authority narrowing under litigation
 *   - article_iii_courts: dual-positioned (institutional/trapped) — lose adjudicable ground while harvesting jurisdiction
 *   - nlrb_covered_workers and ftc_protected_consumers: excluded bearers (powerless/trapped) — absent from the debate that governs them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.74).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading of the Article II Vesting Clause").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'b5bea3f0-cc56-46dd-911c-6ba0e9a5588b').
narrative_ontology:cs_kernel_codification('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', fixed_text).
narrative_ontology:cs_authority_grounding('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', lineage).
narrative_ontology:cs_interpretation_layer_present('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b').
narrative_ontology:cs_reading_relation('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', foundational, plenary_indivisible_article_two_vesting).
narrative_ontology:cs_axiom_status(plenary_indivisible_article_two_vesting, holdable).
narrative_ontology:cs_axiom_grounding('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', plenary_indivisible_article_two_vesting, conventional).
narrative_ontology:cs_axiom('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', foundational, at_will_presidential_removal_requirement).
narrative_ontology:cs_axiom_status(at_will_presidential_removal_requirement, holdable).
narrative_ontology:cs_axiom_grounding('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', at_will_presidential_removal_requirement, conventional).
narrative_ontology:cs_reference_frame('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', undivided_executive_vesting_1787).
narrative_ontology:cs_drift_state('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', contemporary_removal_litigation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b5bea3f0-cc56-46dd-911c-6ba0e9a5588b', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, incumbent_presidents).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidential_appointees).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, originalist_legal_academy).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agency_commissioners).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_reserve_leadership).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, career_civil_servants).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, article_i_congress).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, article_iii_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, article_iii_courts).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, article_two_vesting_plenitude).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, myers_at_will_removal).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, seila_collins_removal_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts authority to remove at will any officer exercising executive functions, directs the Justice Department's Office of Legal Counsel to defend that authority in court, nominates judges expected to endorse it, and tests it against sitting commissioners. Gains direct command of agency policy, personnel, and the patronage that follows; answers to voters for the whole executive branch as a result.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, incumbent_presidents, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, incumbent_presidents, beneficiary).

% Staff the White House and departmental leadership on short rotations tied to each administration. Each cohort inherits levers over regulatory agendas that previously required negotiating with statutorily protected commissions; they move on when the administration ends, carrying the connections with them.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, presidential_appointees, beneficiary,
    powerful, immediate, mobile, national).

% Scholars and affiliated lawyers who supply the textual arguments, staff the judiciary and the Office of Legal Counsel, and collect citations, clerkships, appointments, and influence as the argument gains institutional traction. Their attachment to the argument is professional, not structural; ordinary academic mobility remains open.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, originalist_legal_academy, beneficiary,
    moderate, biographical, mobile, national).

% Hold staggered multi-year terms with removal limited to enumerated causes — the structure Congress built to insulate adjudication and rulemaking from day-to-day presidential direction. Successive court decisions and pending cases treat that insulation as constitutionally defective. Their offices exist only inside the structure being challenged, so there is nowhere to relocate the tenure they hold.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agency_commissioners, payer,
    organized, biographical, trapped, national).

% Governors and chairs whose long terms and removal limits were designed to shield monetary policy from electoral cycles. Financial markets treat their independence as load-bearing for the dollar system, giving them a defense no other agency head possesses — yet recent removal attempts show the legal shield thinning. Their policy effects reach far beyond United States borders.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_reserve_leadership, payer,
    institutional, generational, constrained, global).

% Career staff who understood their role as impartial expertise serving whichever administration holds office. At-will exposure converts professional standing into personal risk; reclassification initiatives have already stripped job protections from thousands. Many describe their attachment as running to the mission rather than to any employer, which makes leaving feel like abandoning the work itself.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, career_civil_servants, payer,
    moderate, biographical, identity_locked, national).

% Wrote the statutes creating the commissions and their tenure protections, and retains formal authority to redesign them. Every new for-cause structure now invites litigation it may lose; the practical option set has narrowed toward building agencies inside the executive department hierarchy. It cannot exit the constitutional order it operates within.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, article_i_congress, payer,
    institutional, generational, constrained, national).

% Decide the removal cases that determine how much insulation survives, and have moved case by case from upholding for-cause limits to striking them down. Each ruling narrows the range of structures they can uphold in future rounds, while the disputes themselves concentrate doctrinal authority and public attention on the bench.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, article_iii_courts, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, article_iii_courts, beneficiary).

% Rely on board independence to enforce organizing and bargaining rights against employers who can wait out an unfriendly administration. They have no seat in the removal-power debate that decides how much independence survives.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, nlrb_covered_workers, excluded,
    powerless, biographical, trapped, national).

% Count on commission stability to keep antitrust and fraud enforcement predictable across administrations. Like covered workers, they bear the consequences of politicized enforcement but appear nowhere in the litigation or scholarship that governs the outcome.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, ftc_protected_consumers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, incumbent_presidents).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the accountability-and-command problem of large-scale administration: a single identifiable principal for all executive action, so voters can attribute outcomes to someone, and so the government can act with unified direction instead of inter-agency stalemate.
% TRANSFER_FUNCTION: Moves control over administrative instruments — removal, direction, agenda-setting, patronage — from statutorily insulated commissions and career services to the incumbent President and his appointees; secondarily moves litigation volume and doctrinal authority toward the bench that adjudicates the boundary.
% ABSENT_VOICES: Covered workers and protected consumers bear the costs of politicized enforcement but hold no seat in the removal debate; career civil servants appear as objects of the litigation, rarely as witnesses; state-level regulators dependent on federal agency independence are likewise unrepresented. Elite unanimity partly reflects that these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the principle vanished overnight, for-cause tenure statutes revive as uncontroversial, commissions resume insulated operation, Congress regains freedom to design structures outside the executive hierarchy, and the removal-litigation stream dries up — the administrative state reorganizes around congressional design rather than presidential command.
% FOUNDING_PROBLEM: Two problems, layered: the 1787 founders chose a single executive to solve the Articles-era diffusion of responsibility and to guarantee energy and accountability in execution; the modern movement revived the reading to solve a newer problem — an administrative state whose senior officers answered to statutes and professional norms rather than to any elected official.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: mid-century pluralist and progressive critics of the 'fourth branch' (from Lowi's critique of interest-group liberalism back to Wilson-era accountability arguments) attested the unaccountability problem decades before the modern movement began; administrative-state defenders concede the democratic-control deficit while disputing the remedy. No one outside the benefiting parties attests that the problem is solved — only that it is real.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.74: the arrangement transfers command of the entire administrative apparatus to whoever holds the presidency, decoupled from any service rendered to the transferred parties. Suppression 0.72 is structural legal coercion — invalidation of tenure statutes, removal litigation, reclassification programs — not participant preference; suppression is authored unscaled, and the engine scales only extractiveness. Theater_ratio 0.32: the accountability rationale is genuinely argued by non-beneficiaries and has pre-modern-movement corroboration, but a growing share of enforcement activity defends unilateral control rather than measurable accountability gains. Accessibility_collapse 0.65: within the reading's frame, insulated structures are categorically void, yet rival frames remain live in doctrine and scholarship, so alternatives collapse only partially. Resistance 0.62: agency litigation, congressional counter-design, scholarly opposition, and intra-court disagreement are sustained and occasionally winning. All three tracked series share one seven-point grid (1980-2025). The trajectories rise monotonically with partisan-phase pauses — stalls under divided government, jumps under unified government aligned with the doctrine's sponsors — a ratchet with intermittent acceleration rather than a full cycle; the pauses are tactical, not reversals, and the enforcement build-up (OLC expansion, then litigation wins, then direct removal attempts) is precisely the dynamic the suppression_requirement series traces.
 *
 * PERSPECTIVAL GAP:
 *   From the Oval Office the arrangement reads as restoration of the constitutional design and of democratic accountability; from a commissioner's chair it reads as expropriation of a statutory tenure; from the career service, as conversion of professional standing into at-will exposure; from the bench, as both a harvest of doctrinal centrality and a narrowing of what it may uphold next term. The payer and beneficiary seats therefore compute different types from identical structural data — the engine derives this divergence; the authored claim does not adjudicate it. Same-level lateral differentiation matters too: commissioners and Fed leadership hold nominally parallel seats, but the Fed's market-enforced shielding gives it an exit-adjacent defense the litigation-dependent commissions lack, so equal nominal rank produces unequal effective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency sits nearest the beneficiary pole (d approximately 0.05): the arrangement subsidizes it with personnel control and patronage, and its exit is arbitrage-grade because it defines the terms of engagement. Appointees (d approximately 0.15) and the academy (d approximately 0.35, indirect via influence markets) trail behind. Targets cluster high: commissioners (d approximately 0.9) and civil servants (d approximately 0.85) bear the transfer with trapped or identity-locked exit; Congress (d approximately 0.7) loses design authority it cannot recover; the Fed (d approximately 0.6) is pulled down from the target pole by market-enforced shielding; courts (d approximately 0.6) sit ambivalently, structural losses offset by jurisdictional harvest. No directionality_overrides were authored: the override key is the power atom alone, and this story contains five distinct institutional seats whose directionalities diverge — an override keyed to 'institutional' would flatten exactly the differentiation the story exists to measure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restoring democratic control over an administratively powerful state — remains live and is corroborated from outside the beneficiary set, so this is not a mandate outliving its function; mandatrophy_resolved stays unset. The tangled-rope classification guards both mislabels: reading the arrangement as pure extraction erases the accountability coordination that non-beneficiary scholars and pre-movement history corroborate; reading it as pure coordination erases the one-directional transfer from insulated structures to the incumbent. The receipt surface settles the residue: gains accrue to a named seat (the incumbent presidency), and fixing is prohibitive for every actor positioned to attempt it — Congress's designs lose in court, and reversal now runs through the very appointment pipeline the arrangement controls.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the separation_of_powers_text kernel; what would the sibling readings change structurally?',
    'Comparative classification of the sibling stories (formalist_reading, functionalist_reading): the functionalist reading removes independent agencies from the victim set entirely and drops measured extraction toward coordination-cost levels; the formalist reading keeps the agencies but redirects the victim set toward delegating Congress.',
    'Classification is reading-relative: the same administrative arrangements compute as tangled_rope under this reading, closer to rope or scaffold under the functionalist reading, and as a different extraction topology under the formalist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-relativity of the constraint within the vesting-clause kernel.').

omega_variable(
    textual_compulsion_vs_construction,
    'Does the Article II vesting clause compel plenary, indivisible executive power as a matter of text and founding understanding, or is the plenitude reading a constructed interpretation selected for its usefulness?',
    'Systematic review of founding-era evidence: the 1789 Executive Departments removal debate (Madison''s fundamental-principle speech versus Smith''s amendment), Pacificus-Helvidius, First Congress practice, early state constitutions, weighed against the counter-evidence Taft''s Myers opinion set aside.',
    'Textual compulsion would give the constraint mountain-like fixity within the American system — with declared beneficiaries, hence false-summit evaluation; construction would leave it an ordinary political product subject to ordinary revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_compulsion_vs_construction, empirical, 'Whether the reading is textually compelled or politically constructed.').

omega_variable(
    partisan_asymmetry_of_invocation,
    'Does the doctrine operate symmetrically — every president controlling every agency — or asymmetrically, invoked and enforced mainly when it consolidates one political coalition''s power?',
    'Code every modern administration''s OLC positions, signing statements, and removal actions for direction and consistency; compare stated principle to practiced tolerance of the rival coalition''s equivalent uses.',
    'Demonstrated asymmetry would indicate the accountability rationale is cover for coalition extraction, pushing the computed type toward snare; demonstrated symmetry would support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_asymmetry_of_invocation, empirical, 'Symmetry of the doctrine''s application across administrations.').

omega_variable(
    accountability_efficacy,
    'Does unified presidential control actually produce the democratic accountability the coordination argument requires — do voters reliably perceive and punish administrative failures as the incumbent''s?',
    'Political-science evidence on retrospective voting for administrative performance, blame attribution under unified versus divided control, and experimental attribution studies.',
    'If attribution fails, the coordination function is largely theatrical and the arrangement''s residual justification collapses toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_efficacy, empirical, 'Whether the accountability payoff is real or rhetorical.').

omega_variable(
    federal_reserve_exceptionalism_durability,
    'Will monetary independence survive the removal-power trajectory — does market-enforced credibility constitute a durable shield the other agencies lack?',
    'Track the pending governor-removal litigation and its successors; observe market pricing of Fed-independence risk across escalation episodes.',
    'A durable exemption removes the largest, most globally scoped victim from the set and lowers measured extraction; its fall would extend the transfer to the last insulated redoubt and raise extraction further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_reserve_exceptionalism_durability, empirical, 'Durability of the Fed''s structural shielding.').

omega_variable(
    authority_grounding_framing,
    'Is the authority that adjudicates this kernel grounded in lineage (continuity with founding text and tradition) or in extraction (institutions whose authority grows from preventing kernel revision)?',
    'Examine whether the interpreting institutions revise against counterevidence or only entrench: compare the Court''s treatment of contrary founding-era evidence and of its own precedents (Morrison) with the treatment of congenial precedent (Myers).',
    'An extraction-grounding framing would reclassify the commitment-system pattern and strengthen the snare-leaning reading of the enforcement trajectory; the lineage framing adopted here treats the interpretive apparatus as transmitting rather than monetizing the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination in the authority_grounding declaration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unitary_exec_tr_t1980, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(unitary_exec_tr_t1980, observed).
narrative_ontology:measurement(unitary_exec_tr_t1988, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement_basis(unitary_exec_tr_t1988, observed).
narrative_ontology:measurement(unitary_exec_tr_t1996, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1996, 0.22).
narrative_ontology:measurement_basis(unitary_exec_tr_t1996, observed).
narrative_ontology:measurement(unitary_exec_tr_t2004, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2004, 0.25).
narrative_ontology:measurement_basis(unitary_exec_tr_t2004, observed).
narrative_ontology:measurement(unitary_exec_tr_t2012, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement_basis(unitary_exec_tr_t2012, observed).
narrative_ontology:measurement(unitary_exec_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement_basis(unitary_exec_tr_t2020, observed).
narrative_ontology:measurement(unitary_exec_tr_t2025, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(unitary_exec_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(unitary_exec_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(unitary_exec_be_t1980, observed).
narrative_ontology:measurement(unitary_exec_be_t1988, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1988, 0.42).
narrative_ontology:measurement_basis(unitary_exec_be_t1988, observed).
narrative_ontology:measurement(unitary_exec_be_t1996, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1996, 0.45).
narrative_ontology:measurement_basis(unitary_exec_be_t1996, observed).
narrative_ontology:measurement(unitary_exec_be_t2004, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement_basis(unitary_exec_be_t2004, observed).
narrative_ontology:measurement(unitary_exec_be_t2012, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2012, 0.6).
narrative_ontology:measurement_basis(unitary_exec_be_t2012, observed).
narrative_ontology:measurement(unitary_exec_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(unitary_exec_be_t2020, observed).
narrative_ontology:measurement(unitary_exec_be_t2025, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(unitary_exec_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(unitary_exec_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement_basis(unitary_exec_su_t1980, observed).
narrative_ontology:measurement(unitary_exec_su_t1988, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1988, 0.38).
narrative_ontology:measurement_basis(unitary_exec_su_t1988, observed).
narrative_ontology:measurement(unitary_exec_su_t1996, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1996, 0.42).
narrative_ontology:measurement_basis(unitary_exec_su_t1996, observed).
narrative_ontology:measurement(unitary_exec_su_t2004, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement_basis(unitary_exec_su_t2004, observed).
narrative_ontology:measurement(unitary_exec_su_t2012, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2012, 0.58).
narrative_ontology:measurement_basis(unitary_exec_su_t2012, observed).
narrative_ontology:measurement(unitary_exec_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.66).
narrative_ontology:measurement_basis(unitary_exec_su_t2020, observed).
narrative_ontology:measurement(unitary_exec_su_t2025, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(unitary_exec_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'separation of powers' decomposes into three structurally distinct readings of one kernel (separation_of_powers_text). This file is the unitary_executive_reading; formalist_reading and functionalist_reading are separate files with their own epsilon, beneficiary/victim sets, and classifications. The coupling runs through litigation: whichever reading the courts adopt rewrites the others' victim sets — this reading's adoption converts the functionalist reading's beneficiary class (insulated agencies) into this reading's victim set. Members are linked via affects_constraints; epsilon is never averaged across members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
