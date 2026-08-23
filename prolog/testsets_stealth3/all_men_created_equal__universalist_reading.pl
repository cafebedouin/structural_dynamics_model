% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of the Equality Clause: Self-Executing Expansion Warrant
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   The sentence 'all men are created equal' functions in American public
 *   life as a self-executing warrant: whoever is excluded may claim it,
 *   courts certify the claims, and each certification dissolves part of the
 *   incumbent arrangement that had done the excluding. This story
 *   instantiates the UNIVERSALIST READING of that kernel — the sentence as a
 *   genuine universal principle whose scope is set by its own content, not by
 *   its authors' intentions or their eighteenth-century taxonomy, and whose
 *   operation therefore mandates iterative expansion, each generation's
 *   exclusions becoming the next wave's docket. The arrangement modeled here
 *   is the expansion engine itself, with its real tolls: uncompensated
 *   dissolution of incumbent privilege, enforcement machinery built and
 *   dismantled across cycles, permanent contestation, and claim-making costs
 *   charged to the very classes seeking entry. KEY AGENTS (by structural
 *   relationship): - federal_judiciary: agenda setter
 *   (institutional/identity_locked) — certifies each widening, administers
 *   its obligations, accumulates interpretive authority with every
 *   confirmation; - inclusion_movement_organizations: primary beneficiary
 *   with payer overlay (organized/identity_locked) — wins admission, pays for
 *   it in organizing-years and lives; - newly_included_class_members:
 *   beneficiary (moderate/mobile) — receive standing opened by waves they did
 *   not purchase; - slaveholding_planter_class: payer (organized/trapped,
 *   regional) — lost uncompensated property and caste position in the first
 *   great wave; - jim_crow_institution_operators: payer
 *   (institutional/constrained, regional) — restructured under federal
 *   enforcement; - gender_exclusive_institution_leaders: payer
 *   (institutional/constrained) — lost membership prerogatives wave by wave;
 *   - originalist_interpretive_school: payer (institutional/identity_locked)
 *   — bears chronic interpretive defeat its method forbids it to concede; -
 *   wider_polity: beneficiary with payer overlay (organized/constrained) —
 *   receives the reusable membership standard, finances the machinery; -
 *   indigenous_nations: excluded (organized/trapped) — collective standing
 *   sits outside the engine's individualist perimeter; -
 *   noncitizen_residents: excluded (powerless/trapped) — bear obligations
 *   without membership claims; - political_theorists_of_membership:
 *   analytical observer. Per the epsilon-invariance discipline this is one of
 *   three sibling constraints carved from one colloquial label; the epsilon
 *   authored here refers to THIS arrangement as this reading assesses it —
 *   the expansion engine with its tolls — not to the originalist arrangement
 *   and not to an idealized completed-equality endpoint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.52).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.55).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of the Equality Clause: Self-Executing Expansion Warrant").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '8a07c7ef-a803-4cbf-a8ef-2118eb105afd').
narrative_ontology:cs_kernel_codification('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', fixed_text).
narrative_ontology:cs_authority_grounding('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', lineage).
narrative_ontology:cs_interpretation_layer_present('8a07c7ef-a803-4cbf-a8ef-2118eb105afd').
narrative_ontology:cs_reading_relation('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', foundational, equality_scope_set_by_universal_content).
narrative_ontology:cs_axiom_status(equality_scope_set_by_universal_content, holdable).
narrative_ontology:cs_axiom_grounding('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', equality_scope_set_by_universal_content, deontological).
narrative_ontology:cs_axiom('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', secondary, iterative_expansion_binding_on_polity).
narrative_ontology:cs_axiom_status(iterative_expansion_binding_on_polity, holdable).
narrative_ontology:cs_axiom_grounding('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', iterative_expansion_binding_on_polity, deontological).
narrative_ontology:cs_reference_frame('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', universal_self_executing_equality).
narrative_ontology:cs_drift_state('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', contemporary_colorblind_ascendancy, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8a07c7ef-a803-4cbf-a8ef-2118eb105afd', '2026-07-02T14:20:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, inclusion_movement_organizations).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, newly_included_class_members).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, slaveholding_planter_class).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, jim_crow_institution_operators).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, gender_exclusive_institution_leaders).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, originalist_interpretive_school).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, wider_polity).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, inclusion_movement_organizations).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, wider_polity).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, living_equality_warrant_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, brown_open_texture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which equality claims have matured into enforceable rights, certifies each widening of civic standing, and administers the obligations that follow. Its prestige is fused with its equality docket, so it cannot step outside the frame without repudiating its own most celebrated work. Each confirmed widening adds to the interpretive authority it holds.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, federal_judiciary, beneficiary).

% Abolitionist societies, suffrage associations, civil-rights legal funds, and marriage-equality litigators win admission for their constituencies and gain standing and purpose from each victory. They pay for every gain with decades of organizing, test cases, jailings, and killed members. They cannot abandon the claim without dissolving the reason they exist.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, inclusion_movement_organizations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, inclusion_movement_organizations, payer).

% Ordinary members of each newly admitted class — freedmen voters, women voters, children in integrated schools, same-sex spouses — receive standing, rights, and access that prior waves opened. They chose none of the struggle that purchased their admission and bear little of its ongoing cost beyond ordinary citizenship duties.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, newly_included_class_members, beneficiary,
    moderate, biographical, mobile, national).

% Held the largest private property interest in the prewar economy; emancipation and the Reconstruction amendments dissolved it without compensation. Descendant regional caste arrangements were then dismantled wave after wave. Wealth bound to land left them unable to leave the jurisdiction enforcing their losses; they financed prolonged counter-mobilization instead.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, slaveholding_planter_class, payer,
    organized, generational, trapped, regional).

% Southern governments, school boards, transit systems, and hospitals that administered racial caste. Court orders and federal enforcement compelled integration at scale: statutes rewritten, budgets redirected, daily operations restructured under supervision. They could delay and obstruct but could not lawfully keep the arrangement once enforcement arrived.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, jim_crow_institution_operators, payer,
    institutional, generational, constrained, regional).

% Trustees of male-only colleges and military institutes, employers and guilds with sex-exclusionary rules. Each successive equal-protection extension dissolved part of their membership prerogatives, and adaptation costs fell on them as each line fell. Concession, not exit, was the available move.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, gender_exclusive_institution_leaders, payer,
    institutional, biographical, constrained, national).

% Jurists and scholars committed to scope-by-founder-intent lose the decisive cases; their school's defeats are the expansion record itself. Their method forbids adopting the rival frame, so each widening deepens rather than resolves their position. They bear the arrangement's costs as chronic interpretive defeat while remaining fully inside the argument.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_interpretive_school, payer,
    institutional, generational, identity_locked, national).

% Receives a reusable membership standard that absorbs inclusion disputes without refounding crises, and a legitimacy formula that outlasts each generation's exclusions. Pays for the machinery that enforces the standard — courts, federal enforcement, at times war — and absorbs the permanent contestation an open-ended principle guarantees. Citizenship is not cheaply exitable, so the bill cannot be declined.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, wider_polity, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, wider_polity, payer).

% Sovereign peoples inside the territory the principle governs. The widening of individual equal citizenship has repeatedly come packaged with pressure on collective nationhood — allotment-era assimilation rhetoric spoke the language of equal individual title. They would object that the arrangement's universalism has a fixed individualist perimeter it never places on the table, but they are not a claimant class the arrangement recognizes.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, indigenous_nations, excluded,
    organized, generational, trapped, continental).

% Live, work, and pay taxes inside the arrangement's jurisdiction without membership claims the principle covers. The famous sentence has never reached them as a justiciable status. They bear the obligations of residence while the widening engine passes them by; deportation risk silences objection.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, noncitizen_residents, excluded,
    powerless, immediate, trapped, national).

% Study how a fixed eighteenth-century sentence became a trans-generational widening engine, what each wave did to losers, and who remains outside the circle's perimeter. Hold no material stake in any wave's outcome.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, political_theorists_of_membership, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, newly_included_class_members).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single trans-generational standard of civic membership: inclusion disputes are converted from secession-and-refounding problems into principled adjudication under a fixed sentence, letting each generation widen the circle without renegotiating the polity's legitimacy formula.
% TRANSFER_FUNCTION: With each wave: civic status, rights, and physical access move from incumbent exclusive arrangements to the newly admitted class, without compensation; enforcement and adjudication costs move to the general polity; and claim-making costs — organizing, litigation, imprisonment, deaths — fall on the excluded classes themselves, who must purchase their own admission.
% ABSENT_VOICES: Indigenous nations and non-citizen residents stand outside the conversation the arrangement governs: individual standing widens while collective sovereignty and non-citizen presence sit behind a perimeter the principle never interrogates. The enslaved generations who died before each wave are unremediable — later admissions reach descendants vicariously, never the injured parties themselves.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand millions of standing grants without warrant: every widening since 1868 cites the principle, and the Reconstruction amendments' interpretive settlements rest on it. Membership rules, judicial authority, and the legitimacy formula itself would require refounding — the precise crisis the arrangement exists to prevent.
% FOUNDING_PROBLEM: How does a republic founded with a proslavery, propertied, male-only franchise keep a single legitimacy formula across generations without either freezing its injustices forever or dissolving into repeated refounding crises? The Declaration sentence answered an imperial-legitimacy problem in 1776; the universalist reading repurposes it as a permanent membership-widening engine.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting seat: Frederick Douglass attested the principle as the excluded's saving force before his class gained anything from it (1852-1857); Martin Luther King Jr. attested the problem as live from a Birmingham cell in 1963, framing the promise as defaulted rather than fulfilled; federal judgeships across opposing coalitions have certified the arrangement's operative force for a century; and originalist dissents corroborate it adversarially — disputing the warrant while confirming the engine's grip on practice. The expansion reading was articulated first by the excluded, not by the arrangement's eventual beneficiaries.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on structural grounds stated independently of the metrics: the arrangement possesses a genuine coordination function (one trans-generational membership standard absorbing inclusion disputes without refounding crises), AND asymmetric transfer through the same structure (each wave moves status and access from identifiable incumbents to a newly admitted class, uncompensated), AND it requires active enforcement to hold (courts, federal power, at times military occupation). Metrics authored independently as descriptive judgments: extractiveness 0.52 at interval end — moderate in aggregate, violently asymmetric per seat once the engine scales by directionality. Suppression 0.55 is a raw structural property, unscaled by power or scope: the ratchet's one-way directionality plus the enforcement apparatus; the suppression_requirement series spikes at each crest (0.82 under Reconstruction occupation, 0.78 at the Voting Rights Act) and decays in dormancy (0.22 post-Redemption) without ever unwinding accumulated transfers. Theater_ratio 0.24 overall: ceremonial invocation outruns delivery in dormancy phases (0.45 at the Plessy-era nadir, 0.42 amid wartime rhetoric-internment gap) and collapses when enforcement is intensive (0.15-0.18 at crests) — theater here tracks the proclamation-practice gap, not vestigial function. Accessibility_collapse 0.55: once the universalist frame is understood, bounded-taxonomy alternatives become difficult to defend consistently (the textualist-paradox pressure), yet the originalist alternative remains live and institutionally powerful, so collapse is partial. Resistance 0.68: the expansion record doubles as a resistance record — civil war, Redemption, Massive Resistance, and the contemporary colorblind turn. All three series share one ten-point grid (1776-2026). Base extractiveness oscillates across four crest-trough cycles; the oscillation is punctuated constitutional change (war, amendment, or court decision, then dormancy and backlash), not intermittent reinforcement — although each crest leaves a consolidated transfer stock, which is why troughs never return to founding-era levels. The Shelby County weakening of Voting Rights Act machinery appears as enforcement decay (0.60 to 0.54) beneath continuing doctrinal expansion.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from identical data. From the planter-class and Jim Crow-operator seats the arrangement presents as near-total uncompensated taking enforced by armies and court orders — an experience with no visible coordination benefit. From the newly-included-member seat the same waves present as admission granted at predecessors' expense — coordination with a toll someone else paid. The federal judiciary's seat renders the frame self-authenticating, approaching duty-like phenomenology — which is why its opinions speak the language of discovery rather than decision. The originalist school's seat experiences ideological displacement: a revered text wielded against its own method. None of these is an error; each is the directionality-and-exit arithmetic run from a different position, and the divergence across seats is precisely the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster near the subsidy pole: newly_included_class_members (mobile exit, moderate power) sit nearest d=0; inclusion_movement_organizations derive low directionality from their beneficiary declaration, with their payer overlay — decades of organizing and martyrdom — keeping them clear of the pole; the arrangement pays them in standing and charges them in struggle. Victims derive high directionality: the planter class (trapped, land-bound) sits nearest the full-target end; Jim Crow operators and gender-exclusive institutions follow with constrained exits; the originalist school's identity_locked exit pushes its directionality toward the target end despite institutional power, since it cannot arbitrage into the winning frame. The wider polity nets near-symmetric: it finances enforcement and absorbs crisis costs, and receives the membership standard and legitimacy continuity. The excluded seats — indigenous nations, non-citizen residents — fall largely outside the transfer circuit, neither subsidized nor targeted by the waves; that placement is itself a finding, since it marks the perimeter the engine never interrogates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving membership disputes without refounding crises — remains live: new claimant classes keep arriving, and this reading's own thesis denies completion. With founding_problem_status 'live' paired with disappearance_verdict 'world_rearranges', no zombie mismatch fires; the arrangement's function is primary, its theater share modest, and no degraded-inertia signature applies. The mandatrophy risk here runs the opposite direction from atrophy: premature victory declaration. Each crest invites a declaration that the mandate is complete — 1896 and 1985 were exactly such declarations — and each was followed by a new wave. The universalist reading structurally resolves mandatrophy by refusing any terminal state; the price is baseline contestation that never falls below a floor (aggregate extraction bottoms near 0.26 even in deep dormancy), a permanent carrying cost the metrics register honestly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the universalist reading of the kernel ''all men are created equal''. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Conceptual, not empirical: the disagreement sits in who determines scope. Originalism locates scope-setting in authorial intent; the textualist-paradox reading treats universal language plus restricted application as performative self-refutation that voids the warrant; this reading locates scope-setting in the sentence''s universal content itself.',
    'Swapping the reading swaps the epsilon referent and the classification wholesale: the originalist instantiation would seat founder-taxonomy inheritors as beneficiaries and expansion claimants as violators; the paradox instantiation dissolves the warrant and with it the arrangement''s legitimacy. No data resolves this; only framework selection does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the equality kernel governs determines beneficiary and victim sets entirely.').

omega_variable(
    claim_cost_intrinsic_or_contingent,
    'Are the struggle-burdens imposed on claimant classes — decades of organizing, test-case litigation, imprisonments and deaths before each admission — intrinsic to a self-executing-warrant design, or contingent on the enforcement path each era happened to take?',
    'Comparative constitutional history: measure cost per unit of admission across top-down expansions (emancipation delivered by war) versus claimant-driven ones (suffage, marriage equality), controlling for opposition intensity.',
    'If intrinsic, the moderate aggregate extraction understates what the arrangement takes from its own beneficiary class, and the directionality picture shifts; if contingent, procedural reform could decouple admission from struggle cost without touching the principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claim_cost_intrinsic_or_contingent, empirical, 'Whether claimant-side costs are structural or path-dependent.').

omega_variable(
    ratchet_directionality_source,
    'Is one-way directionality — widings consolidate, reversals fail (Dred Scott, and the colorblind turn halting rather than unwinding) — structural to the principle, or an artifact of enforcement asymmetry between consolidating winners and regrouping losers?',
    'Examine failed and partial reversal attempts for whether they unwind transfers or merely stop new ones; track a full enforcement-reversal era if one occurs.',
    'If structural, the suppression figure reflects a permanent feature and the ratchet characterization holds; if artifactual, a reversed-enforcement era could begin unwinding, dropping suppression sharply and shifting the classification toward transient support structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_directionality_source, conceptual, 'Source of the arrangement''s irreversible directionality.').

omega_variable(
    individualist_perimeter_blindspot,
    'Does the widening engine necessarily erode collective standing — tribal sovereignty above all — as it extends individual standing, or can the two tracks be reconciled?',
    'Track whether successive waves ever accommodate collective-form claims where they conflict with individual equal-treatment demands (treaty rights versus individual title; tribal membership criteria versus antidiscrimination norms).',
    'If erosion is structural, a permanently unremedied injured class exists inside the reading''s blind spot — from that seat the arrangement computes as far more taking than the aggregate suggests, pulling the per-seat verdict toward pure extraction. If reconcilable, the perimeter is a curable omission rather than a design property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualist_perimeter_blindspot, conceptual, 'Whether universal individual equality structurally costs collective forms of standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.25).
narrative_ontology:measurement_basis(all__tr_t1776, observed).
narrative_ontology:measurement(all__tr_t1820, all_men_created_equal__universalist_reading, theater_ratio, 1820, 0.32).
narrative_ontology:measurement_basis(all__tr_t1820, observed).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__universalist_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement_basis(all__tr_t1868, observed).
narrative_ontology:measurement(all__tr_t1896, all_men_created_equal__universalist_reading, theater_ratio, 1896, 0.45).
narrative_ontology:measurement_basis(all__tr_t1896, observed).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement_basis(all__tr_t1920, observed).
narrative_ontology:measurement(all__tr_t1944, all_men_created_equal__universalist_reading, theater_ratio, 1944, 0.42).
narrative_ontology:measurement_basis(all__tr_t1944, observed).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__universalist_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement_basis(all__tr_t1965, observed).
narrative_ontology:measurement(all__tr_t1985, all_men_created_equal__universalist_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(all__tr_t1985, observed).
narrative_ontology:measurement(all__tr_t2015, all_men_created_equal__universalist_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement_basis(all__tr_t2015, observed).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__universalist_reading, theater_ratio, 2026, 0.24).
narrative_ontology:measurement_basis(all__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.3).
narrative_ontology:measurement_basis(all__be_t1776, observed).
narrative_ontology:measurement(all__be_t1820, all_men_created_equal__universalist_reading, base_extractiveness, 1820, 0.26).
narrative_ontology:measurement_basis(all__be_t1820, observed).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__universalist_reading, base_extractiveness, 1868, 0.78).
narrative_ontology:measurement_basis(all__be_t1868, observed).
narrative_ontology:measurement(all__be_t1896, all_men_created_equal__universalist_reading, base_extractiveness, 1896, 0.34).
narrative_ontology:measurement_basis(all__be_t1896, observed).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement_basis(all__be_t1920, observed).
narrative_ontology:measurement(all__be_t1944, all_men_created_equal__universalist_reading, base_extractiveness, 1944, 0.38).
narrative_ontology:measurement_basis(all__be_t1944, observed).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__universalist_reading, base_extractiveness, 1965, 0.74).
narrative_ontology:measurement_basis(all__be_t1965, observed).
narrative_ontology:measurement(all__be_t1985, all_men_created_equal__universalist_reading, base_extractiveness, 1985, 0.44).
narrative_ontology:measurement_basis(all__be_t1985, observed).
narrative_ontology:measurement(all__be_t2015, all_men_created_equal__universalist_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(all__be_t2015, observed).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__universalist_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(all__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.12).
narrative_ontology:measurement_basis(all__su_t1776, observed).
narrative_ontology:measurement(all__su_t1820, all_men_created_equal__universalist_reading, suppression_requirement, 1820, 0.16).
narrative_ontology:measurement_basis(all__su_t1820, observed).
narrative_ontology:measurement(all__su_t1868, all_men_created_equal__universalist_reading, suppression_requirement, 1868, 0.82).
narrative_ontology:measurement_basis(all__su_t1868, observed).
narrative_ontology:measurement(all__su_t1896, all_men_created_equal__universalist_reading, suppression_requirement, 1896, 0.22).
narrative_ontology:measurement_basis(all__su_t1896, observed).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement_basis(all__su_t1920, observed).
narrative_ontology:measurement(all__su_t1944, all_men_created_equal__universalist_reading, suppression_requirement, 1944, 0.36).
narrative_ontology:measurement_basis(all__su_t1944, observed).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__universalist_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement_basis(all__su_t1965, observed).
narrative_ontology:measurement(all__su_t1985, all_men_created_equal__universalist_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement_basis(all__su_t1985, observed).
narrative_ontology:measurement(all__su_t2015, all_men_created_equal__universalist_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement_basis(all__su_t2015, observed).
narrative_ontology:measurement(all__su_t2026, all_men_created_equal__universalist_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(all__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, fourteenth_amendment_equal_protection).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'all men are created equal' covers three structurally distinct constraints — the originalist reading (scope frozen at founder-era taxonomy), the textualist-paradox reading (universal language plus restricted application as performative self-refutation), and this universalist reading (scope set by universal content, expansion mandatory). Their epsilon values, beneficiary sets, and victim sets differ widely; forcing one story to span them would make classification observable-dependent, which is the signature of a mis-decomposed label. This file instantiates only the universalist reading and links its siblings; the fourteenth-amendment edge records the downstream interpretive structure this reading continuously reshapes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
