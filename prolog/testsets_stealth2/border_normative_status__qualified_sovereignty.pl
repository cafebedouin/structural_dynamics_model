% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty Standard for Border Control
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   States keep border authority but must exercise it through reviewable
 *   justification: every restrictive measure must serve a legitimate aim, be
 *   necessary, be proportionate, and fit human rights obligations, on pain of
 *   adverse judgment. This story is ONE READING of the contested kernel
 *   border_normative_status — the qualified_sovereignty reading — and per the
 *   epsilon-referent rule its epsilon is authored for the STANDING
 *   ARRANGEMENT under contest (actual border-control practice worldwide as
 *   governed and challenged under this standard), assessed by this reading's
 *   own lights: exclusion is legitimate only within proportionality limits,
 *   and much observed practice fails that test. The sibling readings
 *   (sovereignty_primary, freedom_primary) instantiate different constraints
 *   over the same arrangement and are separate files linked through
 *   network.affects_constraints; nothing about them is averaged into this
 *   story. KEY AGENTS (by structural relationship): signatory_states —
 *   agenda-setter and dual-positioned bearer (institutional/constrained),
 *   administers border control, bears the adjudication burden, collects
 *   legitimacy; international_human_rights_bodies — adjudicative beneficiary
 *   (institutional/constrained), collects jurisdiction and caseload;
 *   excluded_migrants — primary target (powerless/trapped), bears exclusion
 *   costs the standard both authorizes and condemns; displaced_citizens —
 *   secondary target (powerless/trapped), bears crowding-out and enforcement
 *   spillover; human_rights_ngos_and_litigators — enforcement entrepreneurs
 *   (organized/mobile); externalized_transit_states — paid intermediaries
 *   (moderate/constrained); comparative_migration_scholars — analytical
 *   observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.62).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.58).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty Standard for Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'e3e28cc4-efa5-49c7-a6f7-2ebdb750805b').
narrative_ontology:cs_kernel_codification('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', formalized).
narrative_ontology:cs_authority_grounding('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', lineage).
narrative_ontology:cs_interpretation_layer_present('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b').
narrative_ontology:cs_reading_relation('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', border_normative_status__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_axiom('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', foundational, exclusion_permissible_when_proportionate).
narrative_ontology:cs_axiom_status(exclusion_permissible_when_proportionate, holdable).
narrative_ontology:cs_axiom_grounding('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', exclusion_permissible_when_proportionate, deontological).
narrative_ontology:cs_axiom('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', foundational, burden_of_justification_on_state).
narrative_ontology:cs_axiom_status(burden_of_justification_on_state, holdable).
narrative_ontology:cs_axiom_grounding('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', burden_of_justification_on_state, conventional).
narrative_ontology:cs_reference_frame('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', postwar_rights_conditioned_sovereignty).
narrative_ontology:cs_drift_state('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e3e28cc4-efa5-49c7-a6f7-2ebdb750805b', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, signatory_states).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, international_human_rights_bodies).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, human_rights_ngos_and_litigators).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, externalized_transit_states).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, signatory_states).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, externalized_transit_states).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, proportionality_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, non_refoulement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain formal authority to decide who enters, on what terms, and who must leave. Ratified treaties oblige them to show that each restrictive measure pursues a legitimate aim, is necessary, and is proportionate, and to answer before courts and treaty bodies when challenged. Compliance brings international legitimacy and predictable relations with partners; defiance brings adverse judgments, reputational cost, and diplomatic friction. Formal withdrawal from the treaty framework is legally available but carries relational costs few governments accept.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, signatory_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, signatory_states, payer).

% Receive individual petitions and inter-state cases, review state reports, and issue judgments on whether border measures meet the justification, necessity, and proportionality tests. Their jurisdiction, caseload, budgets, and standing grow with every dispute referred to them; they exist only so long as states keep the adjudicative framework alive.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_bodies, beneficiary,
    institutional, generational, constrained, continental).

% Seek entry, asylum, or family reunification across borders they do not control. Those refused entry lose years, savings, and sometimes their lives in transit; those detained and removed bear the direct costs of enforcement. The same legal order that excludes them also gives them procedural tools — the right to challenge removal, protection against return to persecution — which they typically reach only through intermediaries, if at all.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, excluded_migrants, beneficiary).

% Citizens displaced by conflict, disaster, or persecution depend on their own state's protective capacity: reception, reintegration, consular help abroad. Admission-control priorities absorb budgets, personnel, and political attention; enforcement infrastructure concentrates in border regions where many displaced citizens and minorities live, exposing them to profiling and severed cross-border livelihoods. Their claims compete with, and usually lose to, the apparatus that decides who may enter.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, biographical, trapped, national).

% Monitor borders, document pushbacks, file petitions, and run the legal clinics through which migrants reach adjudicators. Mandates, funding, and professional standing scale with the volume of violations and cases; they can redirect effort to new fronts — externalization deals, detention conditions — when one front closes.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_ngos_and_litigators, beneficiary,
    organized, biographical, mobile, global).

% Accept payment, equipment, and diplomatic leverage in exchange for intercepting and hosting people bound for richer states. The money and leverage flow to their governments; the warehoused populations, strained services, and security spillover land on their territories and their neighbors. Dependence on the funding stream limits their room to renegotiate the terms.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, externalized_transit_states, beneficiary,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, externalized_transit_states, payer).

% Track how the proportionality standard performs across jurisdictions, document the gap between doctrine and practice, and publish assessments available to every party. They hold no enforcement power and collect nothing from the framework's operation.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, comparative_migration_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, international_human_rights_bodies).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how states that control admission can share a legitimate, contestable standard for doing so: a common battery of tests (legitimate aim, necessity, proportionality, consistency with human rights obligations) that lets democracies defend border measures to one another, lets courts review them, and lets affected people challenge them — replacing unilateral assertion with reviewable justification.
% TRANSFER_FUNCTION: Moves justificatory discretion from state executives to adjudicative review; moves the costs of exclusion onto aspiring migrants (including exclusions the standard authorizes) and enforcement spillover onto border-region residents; moves legitimacy to compliant states; moves enforcement funding and leverage to externalized transit partners; moves caseload, jurisdiction, and standing to courts, treaty bodies, and litigating organizations.
% ABSENT_VOICES: The migrants themselves — above all those intercepted before reaching any forum, and would-be entrants deterred extraterritorially by visa regimes and carrier sanctions — have no seat anywhere in the system: not in treaty negotiation, not in proportionality determination, not in state reporting. They appear only as case files through intermediary organizations. Displaced citizens likewise lack a dedicated voice; their interests surface only incidentally in domestic politics.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, interstate migration governance would reorganize around raw reciprocity and ad hoc bilateral deals; migrants would lose the procedural channel and the non-return floor; courts and treaty bodies would lose the doctrinal bridge connecting rights instruments to border practice; advocacy litigation would lose its target; and the sovereignty-first and freedom-first positions would contend openly for the vacated normative space instead of arguing inside a shared framework.
% FOUNDING_PROBLEM: Post-war reconstruction of legitimate statehood required reconciling territorial control with the new human rights commitments born of the war: how can states honor universal rights while retaining democratic control over who joins the polity?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: governments that reject the standard's reach nonetheless keep filing within it, and their objections are on record in treaty reviews and dissenting opinions; independent legal scholarship attacks the settlement from both directions and documents the unresolved tension; protection-gap reporting by the refugee agency and the continuing volume of adverse border-case judgments attest that the reconciliation the arrangement was built for has not been achieved. No party profits from declaring the problem live — states would prefer it settled in their favor, adjudicators would prefer compliance, advocates would prefer victory.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope: the standard coordinates a real problem — a shared vocabulary in which border measures can be justified, tested, and defended, plus a rights floor (protection against return to persecution, procedural challenge) that concretely protects people — while the same structure extracts asymmetrically: the heaviest costs fall on excluded migrants, including exclusions the standard itself authorizes, and on displaced citizens crowded out by the enforcement apparatus the standard legitimates. Holding the structure together requires active enforcement — courts, treaty bodies, NGO litigation — against constant sovereignty-side pressure, hence requires_active_enforcement. Extractiveness 0.62: substantial but capped, because the reading itself holds much exclusion legitimate; what remains is the cost of authorized exclusion plus the doctrine-practice gap. Suppression 0.58 is authored as a RAW STRUCTURAL property — interdiction, detention, carrier sanctions, externalization — and is deliberately not scaled by power or scope; the engine owns that arithmetic. Accessibility_collapse 0.45: alternatives do not collapse — the freedom-first discourse stays live and states keep unilateralist exits — but within the framework the proportionality test absorbs most challenges. Resistance 0.58: states resist findings, litigate, and denounce courts, while advocates attack the standard's legitimating function from the other side. Theater_ratio 0.30: reporting rituals and rights-washing of externalization are real but a minority of activity; adjudication still produces concrete outcomes. The measurement series runs on ONE SHARED GRID ({0,10,20,30,40,50,60}, roughly 1966-2026) with all three metrics authored at every point. suppression_requirement is tracked deliberately because the story's narrative includes the BUILD-UP of enforcement machinery (treaty bodies, growing court caseloads, pushback monitoring) — a rising trajectory modeling enforcement hardening, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From signatory_states the arrangement looks like a burden voluntarily accepted for legitimacy — compliance cost and lost discretion, tempered by the legitimacy dividend and the order the standard supplies. From excluded_migrants the same structure is a floor they cannot reach without intermediaries, beneath a ceiling of authorized exclusion. From international_human_rights_bodies it is mandate and caseload — the arrangement is their institutional world. From externalized_transit_states it is revenue with warehousing costs attached. Same rules, four different lived constraints. Identity-lock note: interior ministries and border agencies fuse professionally with the control mission — control becomes what the institution IS — so proportionality concessions read internally as institutional defeat, hardening state-side resistance beyond narrow interest calculation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for international_human_rights_bodies, human_rights_ngos_and_litigators, and externalized_transit_states (net collectors). signatory_states are genuinely dual-positioned: listed among beneficiaries for the legitimacy and order the standard supplies, carrying secondary_role payer for the adjudication burden — the honest expectation is a mid-range d rather than the beneficiary pole, and no directionality override is authored because the secondary-role declaration plus their constrained (not arbitrage) exit posture already states the true relationship. excluded_migrants and displaced_citizens anchor the target end: powerless, trapped, bearing the costs. The scholarly observer seat takes no directional position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling democratic admission control with post-war rights commitments — is live, so no mandatrophy is declared. The classification guards against two mislabels: calling the standard a rope ignores that its costs concentrate on voiceless outsiders while its benefits spread across insiders; calling it a snare ignores the genuine coordination achieved and the real protections delivered. The forward risk is piton drift: if adjudication decays into ritual (rising theater_ratio) while states ignore findings, the standard could persist as performance — the measurement series watches theater_ratio for exactly that signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the qualified_sovereignty reading of the border_normative_status kernel. Would the sibling readings assign different epsilon and different victim sets to the same standing border arrangements?',
    'Author the sibling stories (sovereignty_primary, freedom_primary) over the identical referent and compare computed classifications and victim sets across readings.',
    'Under sovereignty_primary, authorized exclusion stops counting as extraction and epsilon falls sharply with a smaller victim set; under freedom_primary, every exclusion counts and epsilon approaches the ceiling. Cross-reading comparison locates the disagreement in the DEFAULT PERMISSIBILITY OF EXCLUSION, not in the facts of border practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a shared kernel; sibling readings change the victim set and the epsilon value.').

omega_variable(
    implementation_gap_attribution,
    'Is the measured extraction attributable to the standard''s design (it authorizes broad exclusion that passes proportionality review) or to state non-compliance (practices the standard condemns persisting anyway)?',
    'Compare extraction indicators in high-compliance jurisdictions (strong domestic incorporation, dense court access) against low-compliance ones; if extraction persists where compliance is high, design shares the blame.',
    'Design-attributed extraction supports the tangled-rope reading with elevated epsilon; non-compliance attribution supports treating the standard as an under-enforced coordination device whose remedy is enforcement rather than redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_attribution, empirical, 'Whether epsilon reflects the standard''s authorization of exclusion or states'' evasion of it.').

omega_variable(
    proportionality_convergence,
    'Does proportionality review converge on consistent outcomes across courts and treaty bodies, or fragment by jurisdiction?',
    'Cross-jurisdictional coding of border-case outcomes (interdiction, detention, removal) against matched fact patterns.',
    'Fragmentation means the constraint operates as materially different constraints in different jurisdictions, complicating any single classification; convergence strengthens the unified tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_convergence, empirical, 'Uniformity of proportionality adjudication across forums.').

omega_variable(
    displaced_citizen_victim_boundary,
    'Do displaced citizens belong in the victim set through resource diversion and enforcement spillover, or do their harms trace to separate constraints (budget allocation, policing practice) that merely correlate with this one?',
    'Trace budgetary and siting decisions specifically justified by border-control obligations versus those independently driven; isolate harms that disappear when the proportionality apparatus is suspended.',
    'If the harms trace elsewhere, the victim set shrinks to excluded migrants and the extraction asymmetry sharpens; if they trace here, the constraint''s extraction is broader than the migrant-focused account suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_citizen_victim_boundary, conceptual, 'Boundary of the victim set regarding displaced citizens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__qualified_sovereignty, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__qualified_sovereignty, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__qualified_sovereignty, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(bord_tr_t40, observed).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__qualified_sovereignty, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(bord_tr_t50, observed).
narrative_ontology:measurement(bord_tr_t60, border_normative_status__qualified_sovereignty, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(bord_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t10, border_normative_status__qualified_sovereignty, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t20, border_normative_status__qualified_sovereignty, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t30, border_normative_status__qualified_sovereignty, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(bord_be_t40, observed).
narrative_ontology:measurement(bord_be_t50, border_normative_status__qualified_sovereignty, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(bord_be_t50, observed).
narrative_ontology:measurement(bord_be_t60, border_normative_status__qualified_sovereignty, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(bord_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t10, border_normative_status__qualified_sovereignty, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t20, border_normative_status__qualified_sovereignty, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t30, border_normative_status__qualified_sovereignty, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(bord_su_t40, observed).
narrative_ontology:measurement(bord_su_t50, border_normative_status__qualified_sovereignty, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(bord_su_t50, observed).
narrative_ontology:measurement(bord_su_t60, border_normative_status__qualified_sovereignty, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(bord_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate border control' decomposes into three readings of one kernel (border_normative_status), each a separate file with its own epsilon over the same standing arrangement. From THIS reading (qualified_sovereignty) to sovereignty_primary: influences — every proportionality judgment narrows what unconditional exclusion claims can justify, changing the sibling's legitimacy conditions without logically eliminating it (bounded-exercise and foundational-authority premises can coexist in hybrid positions). From THIS reading to freedom_primary: forecloses — default-permissible exclusion (proportionate restriction is ordinarily legitimate) and default-impermissible exclusion (ordinary restriction requires extraordinary justification) cannot both be core premises of a single coherent framework. Epsilon differs across the family because the readings index different assessments of the same referent, not because the referent differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
