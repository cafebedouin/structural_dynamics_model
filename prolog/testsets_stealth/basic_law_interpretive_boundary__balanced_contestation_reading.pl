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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Balanced Contestation Boundary of Israeli Basic Law Interpretation
 *   domain: legal/constitutional-political
 *
 * SUMMARY:
 *   Since the 1995 United Mizrahi Bank decision, Israel's Supreme Court has
 *   reviewed Knesset legislation against the Basic Laws, but no text fixes
 *   the boundary of that power: the Basic Laws contain no entrenched override
 *   mechanism and no entrenchment of judicial review itself, while the
 *   Knesset retains simple-majority power to amend them. Practice has filled
 *   the silence with a negotiated regime — the Court strikes, coalitions
 *   re-enact or attach notwithstanding clauses, override proposals surface
 *   and recede, the Attorney General brokers compliance, and international
 *   obligations enter chiefly through judicial citation. This file authors
 *   that negotiated arrangement as ONE reading of the contested kernel
 *   basic_law_interpretive_boundary; the judicial-supremacy and
 *   parliamentary-sovereignty readings are separate constraint files with
 *   their own structures and are not averaged here. The burden referent is
 *   the standing negotiated arrangement itself, assessed by this reading's
 *   own lights — not the court-dominated or coalition-dominated alternatives
 *   this reading argues against. KEY AGENTS (by structural relationship): -
 *   supreme_court_justices: agenda-setting beneficiary
 *   (institutional/identity_locked) — administers the boundary, gains
 *   interpretive territory, fused with the guardian role -
 *   knesset_governing_coalitions: agenda-setting payer
 *   (institutional/immediate horizon) — holds formal amendment power, loses
 *   statutes, guards the override option - rights_dependent_minorities:
 *   primary payer (powerless/trapped) — protection contingent on each
 *   negotiation round - civil_society_rights_organizations: payer with
 *   incidental benefit (organized/constrained) — supplies petitions, absorbs
 *   costs - constitutional_law_professionals: beneficiary (moderate/mobile) —
 *   monetizes contestation - attorney_general_office: dual-positioned broker
 *   (institutional/constrained) - israeli_electorate: near-symmetric
 *   beneficiary-payer (organized/identity_locked) -
 *   international_treaty_bodies: excluded external critic (institutional/no
 *   domestic standing) - comparative_constitutional_scholars: analytical
 *   observer
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda-setting beneficiary (institutional/identity_locked) — administers the boundary through review practice, gains interpretive territory, institutionally fused with the guardian role
 *   - knesset_governing_coalitions: agenda-setting payer (institutional/immediate horizon) — holds formal constituent power, loses statutes to invalidation, guards the unused override option
 *   - rights_dependent_minorities: primary payer (powerless/trapped) — protection arrives case by case and can be undone by the next legislative round
 *   - civil_society_rights_organizations: payer with incidental benefit (organized/constrained) — supplies the petitions, absorbs the litigation costs
 *   - constitutional_law_professionals: beneficiary (moderate/mobile) — monetizes the contest through doctrine, briefs, and personnel
 *   - attorney_general_office: dual-positioned broker (institutional/constrained) — gatekeeps between bench demands and coalition priorities
 *   - israeli_electorate: near-symmetric beneficiary-payer (organized/identity_locked) — receives protection and responsiveness, pays volatility
 *   - international_treaty_bodies: excluded external critic (institutional/constrained) — observes and urges without domestic standing
 *   - comparative_constitutional_scholars: analytical observer (moderate/analytical) — tracks the arrangement against peer systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.47).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.44).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Balanced Contestation Boundary of Israeli Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "legal/constitutional-political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '18ae17d7-dacf-47f2-82bc-e37a924ab04a').
narrative_ontology:cs_kernel_codification('18ae17d7-dacf-47f2-82bc-e37a924ab04a', fixed_text).
narrative_ontology:cs_authority_grounding('18ae17d7-dacf-47f2-82bc-e37a924ab04a', practice).
narrative_ontology:cs_interpretation_layer_present('18ae17d7-dacf-47f2-82bc-e37a924ab04a').
narrative_ontology:cs_reading_relation('18ae17d7-dacf-47f2-82bc-e37a924ab04a', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('18ae17d7-dacf-47f2-82bc-e37a924ab04a', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('18ae17d7-dacf-47f2-82bc-e37a924ab04a', foundational, mutual_bounded_authority_principle).
narrative_ontology:cs_axiom_status(mutual_bounded_authority_principle, holdable).
narrative_ontology:cs_axiom_grounding('18ae17d7-dacf-47f2-82bc-e37a924ab04a', mutual_bounded_authority_principle, conventional).
narrative_ontology:cs_axiom('18ae17d7-dacf-47f2-82bc-e37a924ab04a', foundational, international_norms_bound_sovereignty).
narrative_ontology:cs_axiom_status(international_norms_bound_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('18ae17d7-dacf-47f2-82bc-e37a924ab04a', international_norms_bound_sovereignty, deontological).
narrative_ontology:cs_reference_frame('18ae17d7-dacf-47f2-82bc-e37a924ab04a', negotiated_dual_authority_equilibrium).
narrative_ontology:cs_drift_state('18ae17d7-dacf-47f2-82bc-e37a924ab04a', post_2023_overhaul_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('18ae17d7-dacf-47f2-82bc-e37a924ab04a', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalitions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_law_professionals).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, rights_dependent_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_rights_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_rights_organizations).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, attorney_general_office).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_electorate).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the review hierarchy: decides which Basic Law provisions anchor scrutiny of statutes, develops the operative doctrines (proportionality; reasonableness until its 2023 statutory narrowing), and strikes or upholds legislation. Gains interpretive territory whenever the boundary stays open, and spends institutional capital defending review against override proposals. Leaving the review role would dissolve the Court's self-conception as guardian of the Basic Laws — the role and the institution have fused. Career paths run through the Attorney General's office and academia into the bench.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_justices, beneficiary).

% Enacts Basic Laws and ordinary statutes with a simple majority and holds the formal power to amend either. Responds to judicial invalidation by re-enacting amended versions, attaching notwithstanding clauses, proposing override mechanisms, or amending Basic Laws directly (as with the 2023 change to the reasonableness ground). Loses finished legislation to invalidation and spends coalition time on constitutional fights; the unused option to override is itself valuable electorally, so it is guarded even when never exercised.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalitions, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_governing_coalitions, payer).

% Arab-Palestinian citizens, asylum seekers, women in religious-status matters, and conscription objectors rely on judicial review for protections the political process does not reliably supply. Protection arrives case by case and can be undone by the next legislative round; interim periods leave people in limbo, as with the conscription arrangements struck in 2012 and 2017 and repeatedly extended in between. Citizenship and family ties bind them to the jurisdiction; exit is not a realistic option.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, rights_dependent_minorities, payer,
    powerless, generational, trapped, national).

% Public-interest organizations supply the petitions that keep review busy: they identify targets, litigate for years, absorb the costs, and lose often enough to fundraise around. Their staffing, funding, and public standing depend on an open front of contestable statutes; a closed boundary would shrink their role as surely as a captured one would.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_rights_organizations, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_rights_organizations, beneficiary).

% Academics, litigators, and former clerks supply the doctrine, briefs, and personnel the boundary consumes. Contestation sustains demand: every proposed override, every novel doctrine, every comparative analogy generates publications, consultations, and fee income. Exit is realistic — chairs abroad, private practice, advisory work in other jurisdictions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_law_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Issues binding legal opinions to the government, decides whether to defend challenged statutes, and negotiates compliance timetables with the Court. The unsettled boundary enlarges the office: every dispute routes through its gatekeeping. It serves at political appointment yet answers professionally to the bench — a dual position that makes it both broker and occasional refuser.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, attorney_general_office, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, attorney_general_office, agenda_setter).

% Receives both sides of the bargain: judicial protection of dissenting positions and legislative responsiveness to majorities. Pays in volatility — mass protest mobilization, reservist-refusal episodes, politicization of judicial appointments — and in the sense that basic rights feel negotiable. National membership is constitutive; leaving means emigration, which most experience as loss of self rather than relocation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_electorate, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_electorate, payer).

% UN treaty bodies and peer reviewers issue concluding observations urging alignment of domestic law with ratified obligations; foreign apex-court judgments are cited by petitioners and occasionally by the Court itself. They hold no domestic enforcement standing; their leverage is reputational and enters chiefly through judicial citation. They would press for firmer judicial protection if they had a seat at the table.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_bodies, excluded,
    institutional, generational, constrained, global).

% Track the arrangement against peers — Westminster sovereignty, American judicial supremacy, Canada's notwithstanding-clause dialogue — and advise committees on design options. They collect citations and consulting engagements but bear none of the arrangement's costs; their assessments travel into both camps' arguments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, comparative_constitutional_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_law_professionals).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides constitutional labor between an elected legislature and an unelected court so that basic-law disputes are processed through iterated negotiation — invalidation, response, accommodation — instead of being settled once by whichever branch moves first. It also keeps a functioning constitutional order alive in a polity with no formal constitution, and gives international obligations a domestic port of entry.
% TRANSFER_FUNCTION: Moves final-decision rights over contested statutes back and forth between court and coalition depending on domain and political moment; moves litigation costs and waiting-period burdens onto petitioners and the populations whose status hangs on each round; moves reputational and electoral costs onto whichever branch overreaches last.
% ABSENT_VOICES: Palestinian citizens of Israel and residents of the occupied territories are governed under the same Basic Law framework but hold little negotiating presence — their parties are rarely in governing coalitions, and the territorial population lacks the franchise altogether. International treaty bodies comment from outside without standing. Both would object that the balance is struck among the institutions while those subject to it were never seated.
% DISAPPEARANCE_RATIONALE: Overnight removal forces immediate resolution of the underlying contest: either the Court claims general supremacy — freezing coalition legislation pending review — or the Knesset claims override — collapsing rights protection to majority preference. Every branch's operating procedure presupposes the negotiated middle; treaty partners' legality assessments, the professional complex built on the boundary, and the protest infrastructure organized around defending it would all reorganize within months.
% FOUNDING_PROBLEM: A polity founded on parliamentary majority rule with no formal constitution needed a way to bound that majority without a written constitution to bound it with. The 1950 Harari compromise deferred constitution-making chapter by chapter; the 1992 Basic Laws and the 1995 United Mizrahi Bank decision converted the deferral into a working review arrangement whose limits no text fixes.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Israel Democracy Institute's constitutional studies, decades of Knesset Constitution, Law and Justice Committee proceedings under competing governments, comparative scholarship treating the arrangement as an unfinished design, and the fact that proponents of both rival readings concede the bounding problem is real while disputing only its solution. UN treaty-body observations independently attest that the underlying problem — securing rights against shifting majorities — remains open.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are independent authored facts. The claim — tangled_rope — rests on structure: the arrangement solves a real coordination problem (dividing constitutional labor in a polity with no formal constitution) AND moves real costs through the same structure onto identifiable parties (minorities whose protection is contingent, organizations that fund the litigation, legislators whose statutes die), AND it is held up by active enforcement (review practice, opinion gatekeeping, override deterrence) rather than participant unanimity. The metrics describe operation, not aspiration. Burden 0.47 at interval end: real transfers run through the boundary, but roughly half its activity is genuine constitutional production. Suppression 0.44: persistence leans on enforcement risk and reputational exposure rather than heavy coercion, but the 2023 episode showed the machinery hardening fast when challenged — defiance threats, appointment-packing bills, personal intimidation of judges — before partially standing down. Theater 0.38: override-clause cycles, symbolic Basic Laws, and commission choreography consume a large minority of activity without moving the boundary. Accessibility collapse 0.40: the rival arrangements remain visibly available — the 2023 overhaul was a live attempt to walk through one of them — so understanding the boundary does not close exits. Resistance 0.60: the arrangement is contested from both flanks simultaneously, which is precisely what keeps it a negotiation rather than a settlement. All three tracked series share one time grid (T=0–30, mapping 1995–2025 in five-year steps) with end-state values matching the base properties; the trajectories show crisis-driven cycling — contest intensity accumulates, spikes at reform attempts (2017 conscription-law rounds, 2019–2020 coalition warfare, the 2023 overhaul), then partially de-escalates — and the oscillation itself functions as a mobilization mechanism for both camps rather than as noise. Identity-lock note: the bench's exit is locked by institutional identity fusion — abandoning review would dissolve the guardian self-conception that constitutes the institution — so its beneficiary position is not freely revisable; the electorate's lock is national-identity fusion. Suppression here is predominantly structural (enforcement risk, international exposure) rather than internalized; the residual ambiguity is routed to the restraint_mechanism_ambiguity omega.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from similar nominal standing. From the bench, the arrangement is a dialogue it moderates and from which it gains terrain; from the coalition table, the same dialogue is an unelected veto that consumes statutes and agenda time; from minority organizations, it is shelter that must be re-purchased each session; from the professional complex, it is a market. Two institutional seats (bench, coalition) share a power atom yet face opposite incentive gradients — the engine derives this divergence from roles and exit structure, not from the power labels. The excluded international seats, and the disenfranchised population governed by the same framework, see a balance negotiated entirely among insiders.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (bench, professional complex, governing coalitions in their agenda-setting capacity) sit near the subsidized end; declared payers (rights-dependent minorities, rights organizations net of their incidental standing) sit near the targeted end; the electorate sits near symmetric, receiving both protection and responsiveness while paying volatility. Exit modulates within groups: the bench's identity lock pins it to its beneficiary position regardless of preference; the professional complex's mobility lets it arbitrage the arrangement's continuation; minorities' trap amplifies their effective burden. No directionality overrides are authored: the seats that most need differentiation (bench versus coalition) share a power atom, and the override mechanism keys on power atoms alone, so a blanket correction would smear across structurally opposite seats; per-seat derivation from roles, exits, and declarations carries the distinction instead. International treaty bodies hold no declaration and fall to the power-atom fallback — a known limitation recorded here rather than papered over with a fabricated override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding a parliamentary majority in a polity without a formal constitution — is live: every faction concedes the bounding need while disputing its solution, and the 2023 crisis was fought over exactly that question. With founding_problem_status=live and disappearance_verdict=world_rearranges, the mismatch consumer finds no dead-mandate flag, correctly: the arrangement has not outlived its function. The classification guards against both standard mislabels. Reading the arrangement as pure coordination ignores who pays — minorities re-purchase protection each round and the professional complex monetizes the churn. Reading it as pure predation ignores what would vanish with it — the only working channel for constitutional dispute in a constitution-less polity, and the domestic port of entry for international obligations. The tangled-rope claim keeps both halves on the table; the engine's per-seat computation tests whether the halves balance or tip.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates one reading (balanced_contestation) of the kernel basic_law_interpretive_boundary; what changes structurally if a sibling reading governs instead?',
    'Adoption events: a binding override clause or Basic Law amendment transferring final authority to the Knesset selects parliamentary_sovereignty_reading; constitutional entrenchment of review or a binding-finality doctrine selects judicial_supremacy_reading. Each sibling is a separate constraint file with its own beneficiary/victim structure.',
    'Under judicial supremacy the bench becomes sole capturer and minority protection stabilizes while burden concentrates on the legislature; under parliamentary sovereignty the coalition becomes capturer and minority protection becomes majoritarian discretion. This file''s negotiated structure and its moderate, domain-varying burden profile hold only while the contest itself governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; siblings are separate files, not averaged here.').

omega_variable(
    epsilon_domain_variance,
    'Does the single aggregate burden scalar mask wide variation across policy domains — security and occupation legislation versus socio-economic regulation versus religious-status law?',
    'Decompose into per-domain stories per the epsilon-invariance rule and compare invalidation rates, override responses, and petitioner populations by domain.',
    'The aggregate understates burden in security and religious-status domains, where review bites hardest and override pressure is strongest, and overstates it in technical-economic domains; decomposition could shift the overall type assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_domain_variance, empirical, 'Domain-heterogeneous burden hidden under one aggregate scalar.').

omega_variable(
    equilibrium_durability,
    'Is the negotiated boundary a durable equilibrium or a transition that resolves toward one of the sibling readings?',
    'Track post-2023 indicators: renewed override attempts, Basic Law amendment frequency, Court docket retrenchment, coalition behavior at the next constitutional flashpoint.',
    'Resolution toward either sibling converts this arrangement into a captured structure with a named capturer seat; durability evidence supports the current negotiated classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_durability, empirical, 'Whether the balance persists or collapses toward a sibling reading.').

omega_variable(
    restraint_mechanism_ambiguity,
    'Is the legislature''s customary restraint — not deploying its formal simple-majority amendment power wholesale — internalized norm-compliance or strategic response to enforcement and reputational risk?',
    'Observe coalition behavior when enforcement capacity dips: court-packing windows, supermajority moments, wartime unity governments.',
    'If restraint is purely strategic, the boundary decays quickly once enforcement capacity erodes and the durable-balance characterization weakens; if internalized, the arrangement survives enforcement decay and the normative constraint runs deeper than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restraint_mechanism_ambiguity, empirical, 'Internalized versus strategic sources of legislative self-limitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(basi_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 25, 0.49).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(basi_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 5, 0.29).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(basi_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'the Israeli Basic Law interpretive boundary': the label covers three structurally distinct arrangements — this negotiated reading, court-captured supremacy, coalition-captured sovereignty — with different beneficiary/victim structures and therefore different intrinsic burden profiles. This file links both siblings per the family rule; influence runs in both directions — each sibling's advocacy cites failures of the balance, and the balance's survival conditions are defined by the siblings' continued availability as live alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
