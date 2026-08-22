% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Dual-Sourced Sovereign Legitimacy: Constitutional Hybrid Reading
 *   domain: political philosophy/constitutional theory/legitimacy studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the sovereign_legitimacy kernel:
 *   the constitutional hybrid, in which legitimate authority is dual-sourced
 *   — ceremonial and symbolic authority inherited by a hereditary head of
 *   state, political authority delegated to elected offices — with
 *   constitutional law mediating the boundary between the poles. The ε
 *   referent is the standing dual-sourced arrangement itself, assessed by
 *   this reading's own lights: the hybrid sees itself as a compromise that
 *   reduces the extraction of both pure forms (absolute monarchy's total
 *   claim, pure republic's total disposal of tradition) while introducing
 *   permanent ambiguity costs. Per the ε-invariance principle, the sibling
 *   readings — monarchical_reading (unitary downward flow from inherited
 *   right) and republican_reading (unitary upward flow from popular consent)
 *   — are separate constraint stories with their own ε values, beneficiary
 *   structures, and victim sets; they are linked, not averaged, through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   hereditary_monarch: Primary beneficiary (institutional/identity_locked) —
 *   inherits office, income, immunity; performs ceremony, barred from policy
 *   - elected_officials: Secondary beneficiary (powerful/mobile) — holds
 *   delegated policy power, borrows continuity - constitutional_interpreter:
 *   Agenda setter with a beneficiary secondary position
 *   (institutional/constrained) — adjudicates the boundary, grows
 *   jurisdiction with each dispute - crown_funding_taxpayers: Target
 *   (moderate/constrained) — funds the ceremonial pole, absorbs delay costs -
 *   republican_reformers: Target (organized/constrained) — pure-form
 *   advocates foreclosed by amendment thresholds -
 *   absolutist_traditionalists: Target (moderate/constrained) — pure-form
 *   advocates foreclosed by the delegation settlement itself -
 *   indigenous_treaty_nations: Excluded voice (organized/trapped) — treaty
 *   counterparties never seated in the settlement -
 *   comparative_constitutionalists: Analytical observer — sees the full
 *   cross-jurisdiction structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.5).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Dual-Sourced Sovereign Legitimacy: Constitutional Hybrid Reading").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political philosophy/constitutional theory/legitimacy studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '575c6dfc-d7ed-4565-9845-725a4057f108').
narrative_ontology:cs_kernel_codification('575c6dfc-d7ed-4565-9845-725a4057f108', fixed_text).
narrative_ontology:cs_authority_grounding('575c6dfc-d7ed-4565-9845-725a4057f108', lineage).
narrative_ontology:cs_interpretation_layer_present('575c6dfc-d7ed-4565-9845-725a4057f108').
narrative_ontology:cs_reading_relation('575c6dfc-d7ed-4565-9845-725a4057f108', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('575c6dfc-d7ed-4565-9845-725a4057f108', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_axiom('575c6dfc-d7ed-4565-9845-725a4057f108', foundational, ceremonial_and_political_authority_severable).
narrative_ontology:cs_axiom_status(ceremonial_and_political_authority_severable, holdable).
narrative_ontology:cs_axiom_grounding('575c6dfc-d7ed-4565-9845-725a4057f108', ceremonial_and_political_authority_severable, instrumental).
narrative_ontology:cs_axiom('575c6dfc-d7ed-4565-9845-725a4057f108', foundational, constitutional_law_supreme_over_both_sources).
narrative_ontology:cs_axiom_status(constitutional_law_supreme_over_both_sources, holdable).
narrative_ontology:cs_axiom_grounding('575c6dfc-d7ed-4565-9845-725a4057f108', constitutional_law_supreme_over_both_sources, conventional).
narrative_ontology:cs_reference_frame('575c6dfc-d7ed-4565-9845-725a4057f108', negotiated_settlement_dual_authority).
narrative_ontology:cs_drift_state('575c6dfc-d7ed-4565-9845-725a4057f108', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('575c6dfc-d7ed-4565-9845-725a4057f108', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_interpreters).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, crown_funding_taxpayers).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_reformers).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_interpreter).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, separation_of_symbolic_and_executive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherits the head-of-state office for life together with its residences, income stream, and legal immunities. Performs ceremonial duties — opening legislatures, receiving ambassadors, assenting to laws — and is constitutionally barred from exercising policy power. Abdication exists on paper but would dissolve the institution that sustains the family's position and income; income and status continue regardless of performance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Contest elections, form governments, and exercise real policy power — taxation, legislation, war — while borrowing continuity and dignity from the inherited headship above them. Their authority expires with each electoral cycle; the ceremonial pole above them does not. Leaving office returns them to private life with reputation intact; the arrangement costs them little beyond deference rituals and respect for the boundary.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    powerful, biographical, mobile, national).

% Courts and convention authorities adjudicate where ceremonial authority ends and political authority begins — reviewing prerogative use, consent procedures, and reserve powers. Every boundary dispute expands the interpreter's own jurisdiction, since only they can say where the line sits. Removal protections insulate them from the politicians they referee; moving from the bench into ordinary politics is limited by convention.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_interpreter, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_interpreter, beneficiary).

% Fund the hereditary house through annual grants, security costs, and tax exemptions, and absorb the delay costs whenever a boundary dispute stalls government business. They can vote, protest, and petition for reform, but no ordinary election offers a menu item that removes the arrangement; exit means emigration.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, crown_funding_taxpayers, payer,
    moderate, biographical, constrained, national).

% Organize to replace the inherited headship with an elected or appointed one. They operate legally — publishing, campaigning, forcing referendums — but amendment thresholds and political convention make their goal reachable only through rare, high-cost constitutional moments. National referendum campaigns have been run and lost; the organizations persist between attempts.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_reformers, payer,
    organized, generational, constrained, national).

% Hold that the crowned head should again command real governing power rather than preside. The settlement forecloses their preferred arrangement more tightly than it does the republicans' — the delegation of political authority away from the crown is its load-bearing wall. They publish, lobby, and commemorate; their path runs through cultural memory rather than constitutional procedure.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_traditionalists, payer,
    moderate, generational, constrained, national).

% Nations whose treaties and land relationships were made with the Crown as a continuing sovereign person. The arrangement preserves the Crown as counterparty to those instruments while placing actual governing power elsewhere, leaving them negotiating across a divided counterparty. Their claims predate the settlement and were never seated in it; the relationship attaches to land, not residence, so exit is unavailable.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, indigenous_treaty_nations, excluded,
    organized, generational, trapped, regional).

% Scholars and analysts comparing how different polities source legitimate authority, tracking boundary crises, transition episodes, and republic referendums across jurisdictions. They bear none of the arrangement's costs and collect none of its benefits; their stake is explanatory.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, comparative_constitutionalists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, non-partisan head of state who embodies the polity's identity across electoral turnover, while channeling all discretionary governing power into offices that must be contested and can be changed. Succession at the ceremonial pole is solved once, by inheritance, without recurring legitimacy crises; the political pole renews by election.
% TRANSFER_FUNCTION: Moves annual public money, housing, security, and tax treatment from the general taxpayer to the hereditary house; moves deference, precedence, and international visibility to the monarch; moves borrowed continuity and dignity to elected officials; and moves interpretive jurisdiction over the boundary to courts and convention authorities.
% ABSENT_VOICES: Indigenous treaty nations whose sovereign counterparty was constituted by the Crown without their consent to the settlement's terms sit outside the conversation entirely. At each founding moment the pure-form advocates — republicans who wanted no crown, legitimists who wanted no parliament — accepted or lost the compromise under duress of circumstance; their descendants campaign from outside the amendment machinery. Where they are: social movements, litigation, and scholarship rather than the constitutional table.
% DISAPPEARANCE_RATIONALE: If dual-sourcing vanished overnight, every polity running it faces an unfilled head of state: either an emergency republic (new presidency, rewritten oaths, reassigned prerogatives, renegotiated Crown-treaty counterparties) or a scramble to restore governing monarchy. Reserve powers, assent procedures, and honors systems all lose their holder simultaneously; the rearrangement is constitutional, not cosmetic.
% FOUNDING_PROBLEM: Each hybrid was built to solve the same problem: how to move from hereditary or absolute rule to popular governance without civil war, exile, or state dissolution — buying elite and popular acquiescence by splitting the difference, preserving a shell for tradition while handing the actual powers to the people's delegates.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the founding settlements (the 1688 accommodation, the post-1945 imposed and negotiated constitutions, the 1970s Iberian transition) corroborates the founding problem and documents its solution. Republican movement analyses and the comparative-politics literature — sources outside the benefiting parties — attest that the transition problem is resolved in mature hybrids; palace communications and government statements attest a live continuity function. No attestation from inside the beneficiary set goes uncontradicted from outside.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.45 at interval end): the arrangement carries a real coordination dividend — non-partisan continuity, succession solved without recurring crises, cheap legitimacy transfer to elected governments — but it also runs a permanent fiscal transfer to the hereditary house and imposes ambiguity costs on everyone when the boundary blurs. Suppression (0.50) is structural rather than personal: nothing forbids advocating a republic or a restoration, but amendment thresholds and political convention foreclose both pure forms in practice; the enforcement machinery (courts policing prerogative, consent procedures, reserve powers) exists precisely to hold the boundary. Theater ratio (0.45) is honestly mixed: ceremony is partly the function (continuity, headship, diplomatic presence) and partly aura maintenance for inherited status — the share that is pure performance has grown as governing substance migrated wholly to the elected pole. Accessibility collapse (0.45): the pure-form alternatives remain visible and partially reachable (referendums have been held), but they collapse into impracticality once the settlement's entrenchment is understood. Resistance (0.50): organized republican campaigns, lost referendums, and recurring boundary scandals constitute real, sustained, non-systemic resistance. The temporal series run on one shared grid (t=0 approximates the post-1945 settlement wave; t=80 approximates the present; units are years). Base extractiveness traces a U: high ambiguity costs during postwar consolidation, a trough as conventions settled the boundary, then renewed drift upward in recent decades as consent procedures, funding escalations, and prerogative controversies re-blurred the line. Suppression_requirement declines through the normalization era then ticks back up with the recent boundary disputes — the enforcement picture is part of the story, so it is tracked rather than left to the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the hereditary_monarch's position the arrangement is a protected inheritance: maximal subsidy, minimal burden, no exit conceivable without self-dissolution — the mildest possible experienced regime. From the crown_funding_taxpayer and reformer seats the same structure operates as a standing transfer plus a foreclosure of preferred constitutional forms. The constitutional_interpreter experiences it as jurisdictional growth: every dispute is workload and importance. Elected officials experience nearly pure upside — borrowed dignity at the price of occasional deference. One structure, four phenomenologies; the engine computes this divergence from the structural data, and the divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: hereditary_monarch (beneficiary, identity_locked) sits near the full-beneficiary end — the arrangement subsidizes it directly and its exit is fused with the institution itself. Elected_officials (beneficiary, mobile) derive low d but slightly above the monarch's, since their gain is borrowed rather than owned and expires with each cycle. Constitutional_interpreters carry a dual position: agenda_setter administering the boundary and beneficiary collecting jurisdiction from its ambiguity — the more disputed the line, the more the interpreter's office matters, an incentive to preserve rather than resolve uncertainty. Victims derive high d: crown_funding_taxpayers (moderate, constrained) bear the fiscal transfer directly; republican_reformers and absolutist_traditionalists (organized/moderate, constrained) bear the foreclosure of their preferred forms. The excluded seat (indigenous_treaty_nations) is commentary-grade by design — their grievance is real but belongs structurally to the Crown-sovereignty-over-treaty-land family of constraints, not to this reading's ε, and is documented here without being fed into classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transitioning from hereditary to popular rule without rupture — is solved in every mature hybrid, yet the arrangement persists because removal is prohibitively expensive and because the ceremonial pole still performs live continuity work. The classification guards against two errors. Reading the hybrid as pure extraction ignores the genuine coordination dividend: polities that ran this settlement replaced heads of state across generations without a single legitimacy crisis at the summit, which pure-form polities purchase differently and sometimes painfully. Reading it as pure coordination ignores the permanent fiscal transfer, the foreclosure of both pure forms, and the interpreter's structural incentive to keep the boundary ambiguous. Theater is present and rising but is a symptom, not the test: the test is the cost asymmetry, and here the administrator could in principle redraw the boundary, but the cost of doing so exceeds what any single seat bears — while the function remains substantially live, so the arrangement is not mostly performance. Verdict: the mandate is contested, not resolved; the mismatch consumer should read founding_problem_status=contested alongside disappearance_verdict=world_rearranges and note that the persistence is explained by fixing_cost, not by a dead function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is dual-sourced legitimacy a coherent settlement of the sovereign-legitimacy kernel, or an unstable straddle that both pure readings correctly reject?',
    'Comparative tracking of hybrid polities'' boundary crises against pure-form polities'' legitimacy crises, combined with normative analysis of whether two ultimate sources can be held without contradiction in one constitutional framework.',
    'If the straddle is incoherent, this reading loses to a sibling and the boundary-mediating machinery becomes enforcement of a fiction with rising effective extraction; if coherent, the low-to-moderate extraction reflects a genuine stability dividend and the foreclosure relations stand as drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the constitutional hybrid reading survives the kernel contest against its monarchical and republican siblings.').

omega_variable(
    boundary_mediation_efficacy,
    'Does constitutional mediation actually resolve ceremonial/political boundary disputes, or merely defer them at accumulating cost?',
    'Longitudinal count and severity of boundary crises — ministerial dismissals, prorogation contests, consent-procedure controversies — and whether judicial and conventional rulings subsequently hold.',
    'Repeatedly unresolved disputes raise the ambiguity costs borne by every seat and undermine the coordination half of the arrangement; effective mediation supports the current low-to-moderate extraction assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_mediation_efficacy, empirical, 'Whether the constitutional mediator settles the boundary or parks it.').

omega_variable(
    ceremonial_function_genuineness,
    'Is the inherited ceremonial pole performing real coordination work — continuity, non-partisan headship, diplomatic function — or is it theatrical residue maintained by inertia and spectacle?',
    'Natural experiments from completed republic transitions (Ireland, Mauritius, Barbados): measure state-functioning deltas — continuity of representation, crisis response, international standing — after removal of the hereditary headship.',
    'If the ceremonial pole is purely theatrical, the coordination justification thins and the arrangement''s persistence leans entirely on fixing_cost and inertia, trending the classification toward extraction sustained by performance; if the function is real, the current assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_function_genuineness, conceptual, 'Whether the ceremonial source of legitimacy is functional or vestigial.').

omega_variable(
    net_crown_fiscal_cost,
    'What is the net public cost of the hereditary house once surrendered revenues, security spending, and tax treatment are counted against the headline grant?',
    'Independent audit reconciling crown estate revenue surrenders, sovereign grants, security and property costs, and exemption value.',
    'Materially changes the extraction borne by the taxpayer seat: a large net cost raises effective extraction above the headline-grant picture; a net contribution would support the beneficiary framing''s fiscal defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_crown_fiscal_cost, empirical, 'Net fiscal position of the hereditary house relative to the public.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t16, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(sove_tr_t32, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(sove_tr_t48, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 48, 0.39).
narrative_ontology:measurement(sove_tr_t64, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 64, 0.42).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sove_be_t16, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(sove_be_t32, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(sove_be_t48, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 48, 0.38).
narrative_ontology:measurement(sove_be_t64, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 64, 0.41).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 80, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sove_su_t16, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(sove_su_t32, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(sove_su_t48, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 48, 0.45).
narrative_ontology:measurement(sove_su_t64, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 64, 0.47).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 80, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate sovereign authority' decomposes into three structurally distinct readings of one kernel. monarchical_reading (upstream, historically prior) asserts unitary downward flow; republican_reading asserts unitary upward flow; this story, constitutional_hybrid_reading, asserts dual sourcing with constitutional mediation. The upstream monarchical claim is frequently cited as residual evidence for the ceremonial pole's legitimacy, and the republican claim supplies the delegating pole's warrant — both siblings structurally influence this reading's operating environment even where this reading forecloses their core premises within any single framework. ε differs sharply across the family: the hybrid's low-to-moderate ε reflects compromise; each pure reading carries its own distinct extraction profile and victim set, authored in its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
