% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading — Final, Unchallengeable Constitutional Arbiter
 *   domain: political/legal/institutional_design
 *
 * SUMMARY:
 *   Under the judicial supremacy reading, the constitutional text vests
 *   courts with final, unchallengeable authority over all constitutional
 *   questions: courts invalidate legislative and executive acts, no branch
 *   may correct a constitutional judgment, and no remedy attaches to a losing
 *   branch. The arrangement solves a real coordination problem — some
 *   institution must resolve disputes over fundamental law or the
 *   constitution cannot govern — while extracting along the same structure:
 *   the bench collects interpretive monopoly rents (prestige, caseload
 *   centrality, control of the constitutional vocabulary), and the elected
 *   branches bear a counter-majoritarian veto on their output without appeal
 *   or override. This file is ONE READING of the contested kernel
 *   constitutional_authority_boundary; the coordinate-construction and
 *   parliamentary-primacy readings are separate constraint files with
 *   different epsilon values, linked via network.affects_constraints. The
 *   claimed type (tangled_rope) and the authored metrics are independent
 *   facts: the claim states the authoring seat's structural judgment; the
 *   metrics describe observed operation.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda setter and primary beneficiary (institutional / identity_locked) — administers the boundary, resolves all challenges including challenges to itself, collects interpretive monopoly rents
 *   - elected_legislature: primary target (powerful / constrained) — bears invalidation without remedy on its enactments
 *   - executive_branch: secondary target (powerful / constrained) — executive acts subject to judicial veto; lever limited to slow appointment politics
 *   - minority_rights_holders: incidental beneficiary (powerless / trapped) — protected by a veto it cannot itself exercise
 *   - citizen_majorities: dual-positioned payer-beneficiary (organized / constrained) — its enactments are vetoed; its stability and rights protections are subsidized
 *   - legal_professional_class: secondary beneficiary (organized / mobile) — collects careers and markets from judicial centrality
 *   - legislative_override_advocates: excluded (organized / constrained) — foreclosed from the conversation by settled finality doctrine
 *   - comparative_constitutional_scholars: analytical observer (analytical / analytical) — sees the full structure and its live alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading — Final, Unchallengeable Constitutional Arbiter").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "political/legal/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '33b0ddd9-42a2-4de5-a950-995ffd389c10').
narrative_ontology:cs_kernel_codification('33b0ddd9-42a2-4de5-a950-995ffd389c10', fixed_text).
narrative_ontology:cs_authority_grounding('33b0ddd9-42a2-4de5-a950-995ffd389c10', lineage).
narrative_ontology:cs_interpretation_layer_present('33b0ddd9-42a2-4de5-a950-995ffd389c10').
narrative_ontology:cs_reading_relation('33b0ddd9-42a2-4de5-a950-995ffd389c10', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('33b0ddd9-42a2-4de5-a950-995ffd389c10', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('33b0ddd9-42a2-4de5-a950-995ffd389c10', foundational, judicial_finality_unchallengeable).
narrative_ontology:cs_axiom_status(judicial_finality_unchallengeable, holdable).
narrative_ontology:cs_axiom_grounding('33b0ddd9-42a2-4de5-a950-995ffd389c10', judicial_finality_unchallengeable, conventional).
narrative_ontology:cs_axiom('33b0ddd9-42a2-4de5-a950-995ffd389c10', foundational, counter_majoritarian_guardianship_necessary).
narrative_ontology:cs_axiom_status(counter_majoritarian_guardianship_necessary, holdable).
narrative_ontology:cs_axiom_grounding('33b0ddd9-42a2-4de5-a950-995ffd389c10', counter_majoritarian_guardianship_necessary, instrumental).
narrative_ontology:cs_reference_frame('33b0ddd9-42a2-4de5-a950-995ffd389c10', textually_designated_judicial_finality).
narrative_ontology:cs_drift_state('33b0ddd9-42a2-4de5-a950-995ffd389c10', contemporary_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33b0ddd9-42a2-4de5-a950-995ffd389c10', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, minority_rights_holders).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, legal_professional_class).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, citizen_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, citizen_majorities).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_review_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, counter_majoritarian_guardianship_theory).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides what the constitution means whenever another branch's act is challenged, strikes down statutes and executive actions it finds unconstitutional, and hears every challenge to its own authority — which it alone resolves. Prestige, caseload centrality, and control of the constitutional vocabulary flow to it continuously. No external body reviews its constitutional judgments; renouncing finality would dissolve the institution's reason for being, so the function and the institution have fused.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Enacts statutes that courts may invalidate with no appeal, no override, and no compensating mechanism. Must draft every bill in anticipation of judicial reinterpretation and cannot correct a constitutional ruling through ordinary legislation under this reading. Formal amendment is the only corrective path and is rarely attainable; members' careers run on electoral cycles shorter than the litigation horizon.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).

% Implements policy through orders, proclamations, and agencies whose actions courts may enjoin or strike down. Its principal lever over the arrangement is appointing judges, which operates slowly, unpredictably, and long after the policies that motivated the appointments have lapsed.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Lacks the votes to protect itself legislatively and relies on courts striking down discriminatory enactments; judicial venues are frequently the only effective channel available. It bears almost none of the arrangement's burdens and receives much of its protection, and it cannot exit the jurisdiction on affordable terms.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, minority_rights_holders, beneficiary,
    powerless, generational, trapped, national).

% Enacts policy preferences through elections that courts may veto after the fact; simultaneously receives constitutional stability and rights protections it never voted for. Emigration is costly, and its voice runs through the same electoral channels whose products courts can nullify.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, citizen_majorities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, citizen_majorities, beneficiary).

% Attorneys, law schools, clerkships, and bar institutions cluster around the courts' decisiveness: careers, curricula, and markets for constitutional expertise presuppose that judicial answers are the ones that count. Members can move to other specialties or sectors if the center of gravity ever shifts.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legal_professional_class, beneficiary,
    organized, biographical, mobile, national).

% Propose departmentalist interpretation, override clauses, or jurisdiction-limiting amendments. Once finality is settled doctrine, these proposals are received as attacks on constitutional order rather than as competing readings, and their proponents hold no standing forum inside the arrangement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislative_override_advocates, excluded,
    organized, biographical, constrained, national).

% Document that peer democracies solve the same problem — who decides when branches disagree about fundamental law — through parliamentary primacy, specialized councils, or unwritten conventions, and publish their findings outside the arrangement's enforcement loop. They neither collect from nor bear the arrangement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves disputes between branches over constitutional meaning by vesting final interpretive authority in a single institution, producing uniform authoritative answers across jurisdictions and preventing inter-branch deadlock over what the constitution permits.
% TRANSFER_FUNCTION: Moves interpretive authority and veto power over enacted law from the elected branches to the judiciary; concretely, moves policy outcomes — statutes struck, executive actions enjoined — from legislative and executive determination to judicial determination, with no compensating remedy to the losing branch.
% ABSENT_VOICES: Legislative override advocates and coordinate-construction partisans are outside the conversation: once finality is settled doctrine, their proposals are heard as attacks on constitutional order rather than as competing readings, and they hold no standing forum. Majorities whose enactments are invalidated have no venue after the ruling; their objection registers only as political backlash that the judiciary does not answer to.
% DISAPPEARANCE_RATIONALE: If judicial finality vanished overnight, inter-branch constitutional disputes would lose their designated resolver: either recurring deadlock and legitimacy crises, or rapid reorganization around coordinate construction or legislative override — the sibling readings of this kernel. Statutory and regulatory practice built in anticipation of judicial review would need rebuilding, and rights-protection expectations currently routed through courts would migrate to political channels.
% FOUNDING_PROBLEM: Recurring inter-branch conflict over constitutional meaning threatened governance from the founding era onward, and later, rights-hostile legislatures enacted laws no other institution could reliably stop; consolidating final interpretive authority in the courts answered both.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the judiciary document the consolidation of judicial review as a response to identifiable governance crises rather than a self-serving invention; comparative constitutional scholars corroborate that the underlying problem — allocating final interpretive authority — is real by showing peer democracies solving it through different arrangements; legislative hearing records and political-science studies attest the burden from the paying side. No corroboration exists for the stronger claim that the text itself mandates finality — that premise is actively disputed within the academy.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.68 because the veto is counter-majoritarian, unappealable, and unremedied: the losing branch receives no compensating mechanism, and the arrangement's scope expanded over two centuries from a handful of federal statutes to the whole of legislative and administrative output. Suppression is authored at 0.62 as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by the engine's directionality and scope modifiers. The suppression lives in enforcement structure rather than physical coercion: courts defend their own finality, treat override proposals as illegitimate per se, and the profession socializes its members into treating finality as axiomatic (a small internalized component atop the structural barrier). Theater is 0.25: opinion-writing ceremony and deference ritual are real but secondary; the function performed is genuine. Accessibility_collapse is 0.55 — within this jurisdiction the alternatives (binding departmentalism, legislative override) are largely closed, but comparative systems demonstrate live alternatives, so collapse is partial. Resistance is 0.60: court-packing confrontations, jurisdiction-stripping bills, amendment campaigns, and a sustained scholarly attack on the textual-designation premise. The claimed_type (tangled_rope) is stated independently of these metrics: the arrangement coordinates (single authoritative resolver of inter-branch disputes) and extracts (monopoly rents to the bench, unremedied veto borne by the elected branches) through the same structure, held in place by active enforcement. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the machinery hardened from a timid early-republic assertion to modern absolute-finality doctrine, contempt powers, and jurisdiction defense. All three temporal series run on one shared grid (seven points across t=0..220) so no metric borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the bench, the arrangement is the constitution working as designed: guardianship, uniformity, rule of law — a coordination service it provides and from which it draws standing. From the legislature and executive seats, the identical structure is an unaccountable veto over their enactments with no remedy, enforced by the very institution that profits from it. Minority rights holders experience a third structure again: a protector they cannot vote out and do not pay for. Coalition potential among the paying branches exists — court-curbing coalitions have repeatedly formed — but collective-action problems among legislators and the amendment threshold blunt it. The engine derives these divergences from the declared roles, exits, and horizons; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: the constitutional_judiciary declares itself beneficiary and agenda setter with identity-locked exit, placing it near the beneficiary pole (low d, chi damped or inverted into subsidy). elected_legislature and executive_branch are declared victims with constrained exits, placing them near the target pole (high d, chi amplified). minority_rights_holders are beneficiaries with trapped exit — trapped beneficiaries still sit at low d; their entrapment concerns the protection they receive, not costs borne. citizen_majorities carry a payer role with a secondary beneficiary role, so their derived d sits mid-to-high rather than maximal. legal_professional_class is a mobile beneficiary — nearest the subsidy pole. National spatial scope modestly amplifies effective extraction for target seats because verification of judicial reasoning is institutionally difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-branch conflict over constitutional meaning, later joined by rights-hostile legislatures — is still live, so the arrangement has not outlived its mandate and mandatrophy is not resolved. The tangled_rope classification prevents mislabeling in both directions: reading the arrangement as pure coordination (rope) erases the unremedied veto and the interpretive monopoly rents; reading it as pure extraction (snare) erases the real service — uniform authoritative answers and rights protection that no other institution in this arrangement provides. If the founding problem died — if disputes became politically self-resolving and rights protection migrated to durable statutory guarantees — the arrangement would drift toward inertial maintenance, and the theater_ratio series would be the place to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the constitutional text actually designate courts as final arbiters of constitutional questions, or is judicial finality a constructed accretion that the text underdetermines — making this reading one contestable instantiation of the constitutional_authority_boundary kernel rather than a discovered fact?',
    'Genealogical and textual analysis of founding-era drafting records, ratification debates, and early practice, cross-checked against the textual arguments of the sibling readings; the reading that explains the full record without special pleading prevails provisionally.',
    'If the text underdetermines finality, this reading''s epsilon measures constructed rent rather than textual mandate, strengthening the extraction-side classification and weakening the legitimacy premise that currently damps resistance; if the text does designate finality, the arrangement approaches a mandated structure and epsilon drops toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: this constraint is the judicial_supremacy_reading of kernel constitutional_authority_boundary; siblings coordinate_construction_reading and parliamentary_primacy_reading instantiate different constraints.').

omega_variable(
    interpretive_monopoly_rent_magnitude,
    'How large is the judiciary''s interpretive-monopoly rent relative to the genuine coordination cost of maintaining a final arbiter?',
    'Compare judicial compensation, prestige flows, caseload growth, and institutional power expansion against counterfactual arrangements (specialized constitutional councils, coordinate-resolution conventions) performing comparable dispute-resolution functions in peer democracies.',
    'A wide rent-over-cost gap supports tangled-rope-to-snare drift; a narrow gap supports treating most measured extraction as coordination cost and reclassifying toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_rent_magnitude, empirical, 'Size of the judiciary''s monopoly rent net of coordination cost.').

omega_variable(
    veto_distribution_valence,
    'Is the counter-majoritarian veto''s net incidence protective (striking rights-violating enactments) or extractive (striking distributive and regulatory enactments that threaten concentrated interests)?',
    'Systematic coding of invalidated legislative and executive acts by affected population and valence across the interval.',
    'Predominantly protective incidence lowers effective extraction toward the coordination floor; predominantly interest-serving incidence raises it and strengthens the snare-flavored reading of the veto.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(veto_distribution_valence, empirical, 'Distributional valence of judicial invalidations.').

omega_variable(
    override_foreclosure_vs_suppression,
    'Are legislative override mechanisms logically foreclosed by this reading, or merely politically suppressed — could a jurisdiction run judicial review alongside a functioning override clause?',
    'Comparative evidence from jurisdictions operating override-style mechanisms alongside judicial review; if such arrangements are stable, foreclosure is contingent politics rather than logical structure.',
    'If override is compatible, measured suppression is overstated and the reading coexists with override mechanisms rather than eliminating them; if incompatible, the foreclosure is structural and suppression is correctly measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_foreclosure_vs_suppression, conceptual, 'Whether the reading''s foreclosure of override is logical or political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement_basis(cons_tr_t80, observed).
narrative_ontology:measurement(cons_tr_t120, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 120, 0.19).
narrative_ontology:measurement_basis(cons_tr_t120, observed).
narrative_ontology:measurement(cons_tr_t160, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 160, 0.21).
narrative_ontology:measurement_basis(cons_tr_t160, observed).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 200, 0.23).
narrative_ontology:measurement_basis(cons_tr_t200, observed).
narrative_ontology:measurement(cons_tr_t220, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 220, 0.25).
narrative_ontology:measurement_basis(cons_tr_t220, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement_basis(cons_be_t80, observed).
narrative_ontology:measurement(cons_be_t120, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement_basis(cons_be_t120, observed).
narrative_ontology:measurement(cons_be_t160, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 160, 0.64).
narrative_ontology:measurement_basis(cons_be_t160, observed).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 200, 0.67).
narrative_ontology:measurement_basis(cons_be_t200, observed).
narrative_ontology:measurement(cons_be_t220, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 220, 0.68).
narrative_ontology:measurement_basis(cons_be_t220, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement_basis(cons_su_t80, observed).
narrative_ontology:measurement(cons_su_t120, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement_basis(cons_su_t120, observed).
narrative_ontology:measurement(cons_su_t160, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 160, 0.55).
narrative_ontology:measurement_basis(cons_su_t160, observed).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 200, 0.59).
narrative_ontology:measurement_basis(cons_su_t200, observed).
narrative_ontology:measurement(cons_su_t220, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 220, 0.62).
narrative_ontology:measurement_basis(cons_su_t220, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional authority' decomposes into three structurally distinct readings of one kernel (constitutional_authority_boundary): judicial supremacy (this file — courts final, epsilon ~0.68, judiciary in the beneficiary set), coordinate construction (no final arbiter, distributed interpretation), and parliamentary primacy (legislature final). Each reading has its own epsilon, beneficiaries, and victims; they are separate constraints, not one constraint viewed from angles. This reading influences the siblings' operating environment: where judicial finality consolidates, coordinate-construction practices survive only as informal departmental interpretation, and parliamentary-primacy claims require explicit override machinery that the supremacy reading classifies as illegitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
