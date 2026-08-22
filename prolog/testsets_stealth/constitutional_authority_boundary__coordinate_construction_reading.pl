% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction of Constitutional Authority (Distributed Interpretive Arrangement)
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   Under this arrangement, each constitutionally empowered branch of
 *   government reads the founding text for itself within its own sphere of
 *   action: the legislature enacts its understanding into statutes, the
 *   executive executes under its understanding, and the judiciary adjudicates
 *   under its understanding — and no branch's reading finally binds the
 *   others across all questions. Settlement emerges from ongoing negotiation:
 *   legislative revision, executive non-acquiescence, judicial decision, and
 *   electoral correction. The structure solves a real coordination problem
 *   (government remains operable amid persistent disagreement, and no single
 *   institution captures the power to define the limits of the others) while
 *   extracting real costs from those governed by it (unsettled meaning,
 *   deadlock, and rights claims with no guaranteed final forum). KEY AGENTS
 *   (by structural relationship): - legislative_majorities: primary
 *   beneficiary (institutional/constrained) — retains interpretive autonomy
 *   over the statutes it enacts - presidential_administrations: primary
 *   beneficiary (institutional/constrained) — executes under its own reading
 *   - federal_judges: beneficiary with an ambivalent position
 *   (institutional/generational horizon) — enjoys independence but cannot
 *   compel political-branch acquiescence - constitutional_litigants: primary
 *   target (powerless/trapped) — bear the cost of unsettled constitutional
 *   meaning - rights_claiming_minorities: primary target (powerless/trapped)
 *   — hold no guaranteed final forum - ordinary_citizens: diffuse target
 *   (organized/trapped) — bear instability and deadlock costs -
 *   state_governments: excluded actor (organized/regional) — bound by
 *   settlements they never joined - constitutional_theorists: analytical
 *   observer (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.44).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.55).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction of Constitutional Authority (Distributed Interpretive Arrangement)").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '119bfeb9-eb2f-4cce-aaa6-eea51871d4f3').
narrative_ontology:cs_kernel_codification('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', fixed_text).
narrative_ontology:cs_authority_grounding('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', distributed).
narrative_ontology:cs_reading_relation('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', foundational, no_final_interpretive_arbiter).
narrative_ontology:cs_axiom_status(no_final_interpretive_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', no_final_interpretive_arbiter, conventional).
narrative_ontology:cs_axiom('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', secondary, oath_binds_officers_to_constitution_not_colleagues).
narrative_ontology:cs_axiom_status(oath_binds_officers_to_constitution_not_colleagues, holdable).
narrative_ontology:cs_axiom_grounding('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', oath_binds_officers_to_constitution_not_colleagues, deontological).
narrative_ontology:cs_reference_frame('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', coordinate_departmental_equilibrium).
narrative_ontology:cs_drift_state('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('119bfeb9-eb2f-4cce-aaa6-eea51871d4f3', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, presidential_administrations).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, federal_judges).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, constitutional_litigants).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, rights_claiming_minorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, ordinary_citizens).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, oath_bound_independent_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts its own constitutional understanding into statutes and defends that understanding through legislation, confirmation politics, and jurisdiction-shaping. Its constitutional judgments stand unless another branch actively displaces them, and it collects that interpretive autonomy continuously. It cannot exit the arrangement short of amending the constitution it operates under.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).

% Executes the law under its own reading of the constitution, issuing signing statements, asserting privilege, and declining enforcement at the margins. Collects interpretive autonomy for the span of an administration. Accepting a binding external gloss on its powers would amount to exiting the arrangement, which no administration has done across the board.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, presidential_administrations, beneficiary,
    institutional, biographical, constrained, national).

% Adjudicates under independent constitutional judgment with life tenure insulating them from political displacement. They enjoy interpretive independence within their sphere but cannot compel the political branches' ongoing acquiescence beyond the particular case. Their professional identity is bound up with independent adjudication, making surrender of that role unthinkable even as final political authority eludes them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, federal_judges, beneficiary,
    institutional, generational, constrained, national).

% Bring disputes hoping for a definitive answer and receive rulings that other branches may decline to follow, re-litigate, or neutralize through subsequent action. They bear the cost of unsettled law: repeated litigation, strategic delay by better-resourced opponents, and outcomes that turn on which branch ultimately acts. They cannot opt out of the legal order whose meaning stays contested.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_litigants, payer,
    powerless, immediate, trapped, national).

% Depend on some branch choosing to vindicate their claims. When no branch does, no mechanism forces a final authoritative resolution in their favor. Venue pluralism occasionally helps them, since one branch may act where others refuse, but it leaves them without guaranteed recourse, and exit from the polity is not available to them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, rights_claiming_minorities, payer,
    powerless, generational, trapped, national).

% Bear the diffuse costs of inter-branch disagreement: policy instability, deadlock, and shifting compliance burdens as different branches act on different understandings. They hold episodic electoral leverage over two of the branches but no direct voice in inter-branch settlements, and they cannot exit the jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, ordinary_citizens, payer,
    organized, generational, trapped, national).

% Operate under federal constitutional interpretations they did not join and cannot finally contest. They assert their own readings through litigation and resistance, and would demand a seat in any settlement of constitutional meaning. Their exclusion from the three-branch conversation is structural rather than chosen.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, state_governments, excluded,
    organized, generational, constrained, regional).

% Analyze the arrangement's operation across its full history, documenting the divergence between professed norms and actual practice. They hold no stake in outcomes beyond scholarly standing and see the full structure that no participant seat sees whole.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of operating a limited government amid deep, persistent disagreement about constitutional meaning: each branch acts on its own best understanding within its sphere, and the mutual-undercutting risk is managed by keeping every branch's interpretive claim contestable rather than by crowning a settler. This prevents any single institution from capturing the power to define the limits of the others.
% TRANSFER_FUNCTION: Moves interpretive autonomy to incumbent officeholders of all three branches, each of which keeps effective final say within its sphere; moves the costs of unsettled meaning — legal uncertainty, deadlock, rights vulnerability — onto litigants, rights-claiming minorities, and the general public.
% ABSENT_VOICES: State governments and ordinary citizens are not seated at the inter-branch table where constitutional meaning is effectively negotiated; neither are those whose claims lose in every currently receptive venue. They would object that distributed authority distributes the evasion of responsibility, and that the governed never consented to bearing settlement costs the branches refuse to internalize.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, one branch would rapidly accumulate final interpretive authority — historically the judiciary is positioned to fill such a vacuum fastest — and the entire separation-of-powers equilibrium would reorganize around whatever settlement emerged. Statutory revision practices, executive enforcement discretion, and adjudicative independence would all be renegotiated within a few political generations.
% FOUNDING_PROBLEM: How to constitute a limited government without creating an unaccountable supreme interpreter: preventing any single branch from monopolizing constitutional meaning while keeping government operable despite sincere disagreement about the text.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional historians document the founders' explicit fear of interpretive concentration; state governments have repeatedly contested federal interpretations from their own seat; rights-advocacy organizations document the costs borne when no branch accepts responsibility for vindication. The branches themselves also invoke the founding problem, so the attestation is mixed-source rather than purely self-serving — but the strongest disinterested corroboration concerns the reality of the concentration risk, not the success of this arrangement in meeting it.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.44 (moderate): the arrangement decouples interpretive authority from any settlement obligation, imposing genuine uncertainty and rights-vulnerability costs, but no seat collects monopoly rent and the anti-concentration function is real. Suppression is authored at 0.55 as a raw structural property — the intensity of enforcement the arrangement requires (each branch must actively police its sphere against encroachment) — and is deliberately unscaled; directionality and scope scaling happen engine-side. Theater_ratio at 0.38 reflects a growing share of performative departmentalism (statements, resolutions, signing statements without operational consequence) alongside still-real practice. Accessibility_collapse is low (0.35) because the alternatives — concentrating final authority in one branch — remain live and have been partially realized in practice; resistance is substantial (0.55) because every assertion of judicial finality, legislative override, or executive non-acquiescence contests the arrangement's boundary maintenance. Claimed type and metrics were authored independently: I believe tangled_rope is structurally true (genuine coordination function plus asymmetric extraction through the same structure, held together by active enforcement), and the metrics describe what I believe is descriptively accurate; the engine computes per-seat classifications from the structural data. The measurement series run on one single shared time grid (7 points x 3 metrics, all observed) so no metric is sampled against another metric's end-state substitute. The trajectories oscillate rather than drift monotonically: extraction peaks around the mid-interval constitutional crisis (t=80), relaxes through the mid-century settlement era (t=160), and re-ratchets with the contemporary revival of branch assertion (t=200-236). The cycle is partly an extraction mechanism in itself — each crisis phase legitimizes sphere expansions that do not fully contract when calm returns, producing the mild secular rise in baseline extraction (0.38 to 0.44) visible beneath the oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural data. From the branch seats, the arrangement is a liberty-preserving mutual-respect regime each helped build and each polices; from the litigant, minority, and citizen seats, the same structure is a refusal to guarantee anyone an authoritative answer, with settlement costs externalized onto the governed. State governments experience a third position: bound by negotiations they were never admitted to. Coalition capacity differentiates the victim seats: ordinary_citizens hold organized electoral leverage and episodically punish inter-branch dysfunction, while rights_claiming_minorities hold little coalition power in the relevant timeframe and bear the sharpest version of the no-final-forum cost — the analysis treats their coalition-building potential as live but unbanked, and omega minority_venue_pluralism_valence tracks whether pluralism ever pays them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries (three branch seats) derive directionality near the subsidized end: the arrangement guarantees each of them interpretive autonomy they would otherwise have to win case-by-case. The declared victims (litigants, minorities, citizens) derive directionality near the full-target end: they pay the certainty costs the branches decline to internalize, with trapped exit pushing them further toward full-target. One residual is flagged rather than overridden: federal_judges derive as beneficiaries, but their true position sits nearer symmetric, since the same arrangement that protects their independence also denies them the finality their professional role seeks. No directionality override is authored because the override surface keys on power atom, and all three branch seats share the institutional atom — correcting the judges would mis-correct the legislature and executive, whose beneficiary positioning is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing interpretive monopoly while keeping government operable — is authored as contested, not dead, so no mandatrophy resolution is declared and none should fire. The tangled_rope classification does double preventive work: against the pure-rope mislabel, which would credit the anti-concentration function while ignoring that the same structure extracts settlement certainty from the governed; and against the snare mislabel, which would condemn the arrangement while ignoring that its coordination function is genuine and that every rival arrangement carries its own extraction profile. The rising theater_ratio series is the monitored failure path: if departmentalist practice decays into performance while the vocabulary persists, the arrangement crosses toward piton — a formerly functional coordination maintained inertially — and omega departmentalist_revival_authenticity is the instrument watching that crossing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the coordinate construction reading the correct instantiation of the constitutional_authority_boundary kernel, or would the judicial supremacy or parliamentary primacy readings better capture the text''s actual allocation of interpretive authority?',
    'Sustained doctrinal development, formal amendment, or an unambiguous inter-branch practice settling the finality question over successive political generations.',
    'If the judicial supremacy reading prevails, a monopoly beneficiary emerges (the judiciary captures interpretive rents), the victim set shifts to include the political branches'' autonomy, and gain_flow ceases to be diffuse; the classification migrates toward a captured structure with concentrated receipt. If parliamentary primacy prevails, the beneficiary collapses into the elected legislature and the judicial seat flips to target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'This constraint is one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    disagreement_location_interpretive_finality,
    'Where exactly do the readings disagree: does the text''s grant of jurisdiction over constitutional questions plus its supremacy clause entail final judicial settlement, or does the officers'' oath plus the separated allocation of powers entail that each branch judges for itself?',
    'Doctrinal analysis isolating which textual provisions each reading treats as load-bearing, tested against how each reading explains the provisions it must discount.',
    'Locating the disagreement in the finality question (rather than in, e.g., amendment rules or federalism) determines which structural elements of this story survive under a sibling reading and which are replaced wholesale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_interpretive_finality, conceptual, 'The specific structural element on which the sibling readings diverge is whether any provision assigns final interpretive authority.').

omega_variable(
    uncertainty_cost_magnitude,
    'How large are the real costs of unsettled constitutional meaning borne by litigants, minorities, and the public, relative to the liberty-protecting value of denying any branch a final interpretive monopoly?',
    'Comparative institutional analysis across jurisdictions with concentrated versus distributed final interpretive authority, controlling for regime type and rights outcomes.',
    'If uncertainty costs dominate, epsilon rises toward snare territory and the victim declarations strengthen; if the anti-concentration benefit dominates, epsilon falls toward rope and the arrangement approaches net-beneficiary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_cost_magnitude, empirical, 'Magnitude of the extraction side of the ledger under this reading.').

omega_variable(
    minority_venue_pluralism_valence,
    'Do rights-claiming minorities systematically bear venue pluralism as a cost, or can organized coalitions convert plural venues into an advantage by shopping for the receptive branch?',
    'Longitudinal study of minority rights campaigns: track whether multi-venue strategies yield net wins or net delays relative to counterfactual single-forum settlement.',
    'If coalitions systematically benefit, the victim declaration for rights_claiming_minorities weakens, the arrangement looks more rope-like, and the coalition-capacity concern recedes; if they systematically lose, extraction concentrates on the least powerful seat and the tangled_rope reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_venue_pluralism_valence, empirical, 'Whether the diffuse victim seat is net-harmed or net-helped by interpretive pluralism.').

omega_variable(
    departmentalist_revival_authenticity,
    'Does the contemporary revival of departmentalist assertion represent genuine restoration of coordinate practice, or theatrical maintenance of the vocabulary while acquiescence to judicial settlement continues in fact?',
    'Behavioral audit of branch conduct: measure how often professed departmentalist positions are acted on at real cost (enforcement declined, statutes defended against rulings) versus issued as statements without operational consequence.',
    'If the revival is largely theatrical, theater_ratio continues climbing past 0.5 and the arrangement drifts toward piton — a formerly functional coordination maintained as performance; if genuine, the arrangement remains a live tangled_rope with rising enforcement demands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(departmentalist_revival_authenticity, empirical, 'Authenticity of the contemporary practice revival against the piton failure mode.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 236).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(cons_tr_t80, observed).
narrative_ontology:measurement(cons_tr_t120, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 120, 0.33).
narrative_ontology:measurement_basis(cons_tr_t120, observed).
narrative_ontology:measurement(cons_tr_t160, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 160, 0.3).
narrative_ontology:measurement_basis(cons_tr_t160, observed).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement_basis(cons_tr_t200, observed).
narrative_ontology:measurement(cons_tr_t236, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 236, 0.38).
narrative_ontology:measurement_basis(cons_tr_t236, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement_basis(cons_be_t80, observed).
narrative_ontology:measurement(cons_be_t120, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 120, 0.46).
narrative_ontology:measurement_basis(cons_be_t120, observed).
narrative_ontology:measurement(cons_be_t160, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 160, 0.4).
narrative_ontology:measurement_basis(cons_be_t160, observed).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 200, 0.47).
narrative_ontology:measurement_basis(cons_be_t200, observed).
narrative_ontology:measurement(cons_be_t236, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 236, 0.44).
narrative_ontology:measurement_basis(cons_be_t236, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(cons_su_t80, observed).
narrative_ontology:measurement(cons_su_t120, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 120, 0.48).
narrative_ontology:measurement_basis(cons_su_t120, observed).
narrative_ontology:measurement(cons_su_t160, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 160, 0.36).
narrative_ontology:measurement_basis(cons_su_t160, observed).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement_basis(cons_su_t200, observed).
narrative_ontology:measurement(cons_su_t236, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 236, 0.55).
narrative_ontology:measurement_basis(cons_su_t236, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'who decides what the constitution means' covers three structurally distinct arrangements, not one constraint viewed from angles. This file authors the coordinate construction reading — distributed authority, no monopoly beneficiary, moderate epsilon driven by settlement-cost externalization. The judicial supremacy reading concentrates the beneficiary seat in the judiciary and converts the political branches into targets; the parliamentary primacy reading concentrates it in the elected legislature. Each sibling gets its own epsilon, its own victims, and its own classification in its own file; the family links here support contamination-propagation analysis across the kernel (a legitimacy shock to one reading propagates to the others through the shared text).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
