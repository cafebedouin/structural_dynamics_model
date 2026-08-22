% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent Corpus — Pluralist Balancing Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The pluralist-balancing reading of the common law precedent corpus holds
 *   that precedent weight is neither fixed (strict stare decisis) nor
 *   adaptive without constraint (evolutionary framework), but rather
 *   context-dependent: courts balance the domain's need for stability against
 *   its need for legal adaptation. Constitutional law, commercial law, and
 *   family law have different calculi for when precedent should bind and when
 *   context change justifies departure. This reading licenses courts to
 *   decide on a domain-by-domain basis what counts as sufficient context
 *   change to warrant precedent departure, creating institutional flexibility
 *   but also unpredictability for litigants. The extractiveness comes from
 *   the appellate courts' control over the balancing apparatus itself — they
 *   set the domain-specific rules by which change is permitted, and litigants
 *   seeking change must navigate opaque, domain-sensitive criteria for when a
 *   case is 'distinguishable' versus when 'the context has evolved
 *   sufficiently.' The reading instantiates one interpretation of the kernel
 *   (common_law_precedent_corpus); it is NOT the strict stare decisis reading
 *   (which binds absolutely) nor the evolutionary reading (which permits
 *   broader reinterpretation). Measurement series run from the consolidation
 *   of English precedent reporting (mid-1700s) through contemporary practice
 *   (2026), showing extractiveness and theater rising as the precedent corpus
 *   expands and domain-sensitivity increases, making it harder for novel
 *   claims to predict when courts will permit departure.
 *
 * KEY AGENTS:
 *   - Appellate courts: set the domain-specific balancing criteria; control when precedent binds vs. permits evolution
 *   - Litigants seeking precedent change: powerful parties mounting appeals; face uncertain success under context-dependent criteria
 *   - Lower court judges: operate under binding precedent but must distinguish novel fact patterns; face reversal risk
 *   - Novel legal claims: structurally disadvantaged; must overcome skepticism that prior courts rejected them for principled reasons
 *   - Litigants relying on precedent: benefit from domain-specific stability; have better exit options by structuring around doctrine
 *   - Established legal doctrines: protected by pluralist reading's emphasis on context-dependent stability
 *   - Legal scholars: excluded from formal decision-making; influence is indirect, subject to dismissal
 *   - Prior courts: historical actors whose authority is invoked but not present; their framing shapes domain-context interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.62).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent Corpus — Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'c048282c-4b1a-4fbe-b851-fc43d9e8e752').
narrative_ontology:cs_kernel_codification('c048282c-4b1a-4fbe-b851-fc43d9e8e752', fixed_text).
narrative_ontology:cs_authority_grounding('c048282c-4b1a-4fbe-b851-fc43d9e8e752', lineage).
narrative_ontology:cs_interpretation_layer_present('c048282c-4b1a-4fbe-b851-fc43d9e8e752').
narrative_ontology:cs_reading_relation('c048282c-4b1a-4fbe-b851-fc43d9e8e752', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('c048282c-4b1a-4fbe-b851-fc43d9e8e752', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('c048282c-4b1a-4fbe-b851-fc43d9e8e752', foundational, precedent_domain_sensitive).
narrative_ontology:cs_axiom_status(precedent_domain_sensitive, holdable).
narrative_ontology:cs_axiom_grounding('c048282c-4b1a-4fbe-b851-fc43d9e8e752', precedent_domain_sensitive, instrumental).
narrative_ontology:cs_axiom('c048282c-4b1a-4fbe-b851-fc43d9e8e752', foundational, context_change_adaptive_trigger).
narrative_ontology:cs_axiom_status(context_change_adaptive_trigger, holdable).
narrative_ontology:cs_axiom_grounding('c048282c-4b1a-4fbe-b851-fc43d9e8e752', context_change_adaptive_trigger, empirically_contingent).
narrative_ontology:cs_reference_frame('c048282c-4b1a-4fbe-b851-fc43d9e8e752', principled_domain_context_balancing).
narrative_ontology:cs_drift_state('c048282c-4b1a-4fbe-b851-fc43d9e8e752', contemporary_precedent_volume_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c048282c-4b1a-4fbe-b851-fc43d9e8e752', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_courts).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, established_legal_doctrines).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_precedent_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, novel_legal_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, novel_legal_claims).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, litigants_relying_on_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_court_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Appellate judges set the operative precedent weight by deciding when stability demands adherence and when context demands evolution. They control the interpretive apparatus that resolves domain-specific balancing tests. They author the opinions that declare which precedents are binding, distinguishable, or subject to reconsideration. Their institutional interest is in maintaining docket control and judicial authority.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Powerful litigants (corporations, advocacy organizations, government entities) must mount appeals arguing for precedent change, knowing the pluralist reading permits but does not guarantee success. They invest in legal argumentation about why the domain context (commercial evolution, constitutional reinterpretation, technological change) warrants departure from settled precedent. Success is uncertain and context-dependent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_precedent_change, payer,
    powerful, biographical, constrained, national).

% Trial judges operate under precedent declared binding by appellate courts but must make domain-specific judgment calls about whether appellate authority covers the novel fact pattern before them. They bear the cost of distinguishing cases and face reversal risk if their distinction is deemed over-creative. They depend on appellate guidance that remains ambiguous across domains.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_court_judges, payer,
    moderate, biographical, constrained, national).

% Legal theories and constitutional claims that have no established precedent supporting them face structural disadvantage: they must overcome skepticism that prior courts chose not to recognize them for principled reasons, not just lack of imagination. They benefit from the pluralist reading's permission to argue context-change (e.g., 'technology did not exist in the prior era'), but the same reading permits courts to defer to prior judicial judgments about what contexts matter. They are trapped in the system's legitimacy framework.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, novel_legal_claims, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, novel_legal_claims, beneficiary).

% Doctrines that have achieved precedential status are protected by the pluralist reading's emphasis on context-dependent balancing: courts can appeal to domain-specific stability when doctrine is established, and to domain-specific change when it serves judicial authority. The reading vindicates doctrinal stability without committing courts to any single doctrine.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, established_legal_doctrines, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(common_law_precedent_corpus__pluralist_balancing, established_legal_doctrines).

% Parties whose legal positions are supported by established precedent benefit from the pluralist reading's permission to invoke domain-specific stability and predictability. They can argue that the domain context (commercial practice, constitutional tradition, regulatory framework) demands that settled law be honored. Their exit options are relatively better: they can structure transactions and claims around established doctrine.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants_relying_on_precedent, beneficiary,
    moderate, biographical, mobile, national).

% Law professors, practicing attorneys, and legal theorists who develop arguments for how precedent should be balanced across domains are excluded from the formal decision-making apparatus. They publish, litigate in briefs, and advise clients, but appellate courts retain control over whether context-dependent balancing is applied. Their influence is indirect and subject to dismissal as 'academic' or outside the judicial power.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_scholars_and_practitioners, excluded,
    organized, generational, mobile, national).

% Courts that issued the precedent being balanced are historical actors; their authority is invoked but not present. The pluralist reading requires current courts to interpret prior reasoning in light of new context, creating a narrative relationship between eras of jurisprudence. Prior courts' framing of domain and doctrine shapes what counts as within-domain stability versus cross-domain evolution.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, prior_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, appellate_courts).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The precedent corpus coordinates judicial authority across time and jurisdiction: by binding lower courts and future panels to reasoned decisions, it ensures that law does not shift arbitrarily case-by-case, permitting litigants and legal institutions to plan around doctrine rather than judicial whim.
% TRANSFER_FUNCTION: Moves interpretive control from litigants and novel legal theories to appellate courts and established doctrines. Litigants seeking precedent change must mount costly appeals and arguments about domain context; established doctrines are protected by default unless their domain context is proven to have shifted. The constraint transfers the burden of justifying change from stability-advocates to change-advocates.
% ABSENT_VOICES: Legal actors and theories that never achieved precedential foothold are structurally silent: they have no prior court decision to cite, no precedent to distinguish or reinterpret. Marginalized communities whose legal claims were rejected in prior eras cannot call historical witnesses; their absent voices are the forgotten precedent denials. Future legal theories not yet conceived are also absent.
% DISAPPEARANCE_RATIONALE: If the precedent corpus and its binding force vanished overnight, each case would be decided de novo without reference to prior reasoning. Transaction planning would become impossible; litigants could not predict outcomes; judicial authority would fragment into idiosyncratic rulings. Legal practice would reorganize around fresh arguments and fact-patterns rather than doctrine, and lower courts would lack guidance on how to interpret statutes and constitutions consistently.
% FOUNDING_PROBLEM: Oral legal tradition and inconsistent decisions across courts created uncertainty in common law practice: merchants, property owners, and future litigants could not rely on judicial reasoning from prior cases because courts disagreed on doctrine and reasoning was transmitted orally or inconsistently. The precedent corpus with binding authority was built to solve this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and scholars of the common law tradition (Holdsworth, Plucknett, Baker) attest the founding problem was live in medieval and early modern England — inconsistency, oral transmission, and lack of doctrinal accumulation. Contemporary legal scholars debate whether the founding problem persists: some argue precedent still solves unpredictability; others argue that precedent's volume and domain-sensitivity now replicate the original fragmentation problem at a higher level of complexity. Appellate courts assert the binding-precedent system is essential to rule of law; litigants seeking change argue it paralyzes adaptation.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because appellate courts control the balancing apparatus but the pluralist reading constrains them — they cannot simply overturn precedent on whim; they must show domain-context change. The constraint requires justification, which is enforcement structure. Suppression is slightly higher (0.62) because the pluralist reading makes it hard for novel claims to know when they meet the 'sufficient context change' threshold; lower courts are suppressed by reversal risk; litigants are suppressed by unpredictability. Theater rises from 1750 to 2013 (0.15 to 0.41) as the precedent corpus expands and domain-sensitivity becomes more elaborate — courts spend increasing effort on domain-specific balancing rhetoric without necessarily changing the outcome distribution. The measurement series shows extractiveness and theater converging toward the contemporary period, suggesting the constraint is stabilizing around a performative equilibrium: courts claim domain-specific balancing while maintaining appellate control. The plural reading sits structurally between strict stare decisis (which would show near-zero extractiveness, pure stability) and evolutionary framework (which would show higher extractiveness as courts claim freedom to reinterpret). This reading's intermediate position reflects its core claim: balancing, not binding, not freedom.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate court seat (agenda-setter), the pluralist reading is a principled framework that honors precedent while allowing necessary adaptation — extractiveness appears as the justified cost of coordination. From the litigant-seeking-change seat (payer), the same reading is opaque: courts claim to balance context-dependent criteria but the criteria are set by courts themselves, creating unpredictability and suppression. From the lower court seat, the reading is constraining: judges must navigate domain-specific balancing without clear rules, facing reversal risk. From the novel-claim seat (powerless), the reading is particularly extractive: new legal theories must overcome a presumption that prior courts rejected them for good reasons, not just lack of imagination. The engine computes these divergences from the structural data (power, exit_options, role); the authored claim (tangled_rope) does not adjudicate the divergence — it describes the structure that produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate courts benefit from the constraint (low d, near beneficiary end): it legitimates their authority to set balancing rules, protect doctrines they favor through domain-framing, and maintain appellate docket control. Litigants seeking change face moderate extraction (d moderately high): they must bear the cost of demonstrating context change within domain-specific criteria set by courts. Lower court judges face suppression from reversal risk and guidance ambiguity (d moderate-high). Novel claims face the highest extraction (d near target): they must overcome the prior-court presumption and satisfy domain-context criteria simultaneously. Established doctrines benefit from the framework (non-agent beneficiary, protected by pluralist balancing). Litigants relying on precedent have moderately high beneficiary status (d low-moderate): they can cite established doctrine and appeal to domain stability. The constraint coordinates on doctrine while extracting from those seeking change. It is not pure extraction (snare) because genuine coordination happens: courts do stabilize doctrine and lower courts do rely on binding precedent. It is tangled rope: coordination for some (doctrine-reliers, established law), asymmetric extraction from others (change-seekers, novel claims).
 *
 * MANDATROPHY ANALYSIS:
 *   The pluralist reading avoids classifying the precedent constraint as a pure Snare (extraction without coordination) by anchoring coordination function (doctrine stability, lower court guidance, litigant reliance) as genuinely operative. It also avoids pure Rope by acknowledging that the coordination is asymmetric: established doctrine and precedent-reliant parties benefit more than change-seekers and novel claims. The reading is Tangled Rope precisely because both the coordination and the extraction are structural: courts genuinely coordinate through precedent (real Rope component), AND courts genuinely extract control over when adaptation is permitted (real Snare component, concentrated in appellate hands). The constraint requires active enforcement (appellate courts reversal opinions, lower court compliance, doctrine protection through domain-framing) — this is a Tangled Rope marker. The reading prevents misclassifying precedent as pure coordination (which would ignore extraction) or pure extraction (which would ignore genuine doctrine stability). The mandatrophy risk appears as domain-context criteria becoming purely performative: courts claim domain-specific balancing while actually just protecting favored doctrines, at which point the coordination function (doctrine stability, lower court guidance) atrophies and the constraint becomes a Piton (extraction maintained by theatrical appeal to domain-context rather than real adaptation). Measurement shows theater_ratio rising to 0.41 — significant but not dominant — suggesting the constraint has not yet fully degraded to Piton status, though the trend merits monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_sensitivity_boundary,
    'What makes two legal domains distinct for purposes of precedent balancing? Is the boundary epistemic (the kind of knowledge the domain involves), institutional (which court system governs), practical (the consequences of error in the domain), or conventional (what lawyers call it)?',
    'Comparative jurisprudence: examine how appellate courts distinguish domains when invoking domain-specific balancing, and test whether the boundaries track epistemic, institutional, practical, or conventional criteria across a sample of precedent-change cases.',
    'If boundaries are epistemic or practical, domain-sensitivity is defensible as tracking genuine variation in context-change justification. If boundaries are merely conventional, domain-framing may be a rhetorical resource courts use to justify otherwise arbitrary precedent-change decisions, shifting the constraint toward Piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_sensitivity_boundary, conceptual, 'Whether domain-based balancing is epistemically grounded or conventionally constructed.').

omega_variable(
    context_change_measurement,
    'By what criteria do courts measure whether a domain''s context has changed sufficiently to justify precedent departure? Is the measurement objective (empirically verifiable facts about the domain), subjective (judicial judgment about salience), or path-dependent (prior decisions about what counts as relevant context)?',
    'Content analysis of appellate decisions invoking domain-specific context change: code the criteria courts cite (technological change, regulatory evolution, social attitudes, prior case law), quantify frequency, and test whether different domains use consistent criteria or invent domain-specific criteria post-hoc to justify preferred outcomes.',
    'If criteria are objective and consistent across cases, the pluralist reading is defensible as a principled framework. If criteria are subjective or path-dependent, courts are using context-change rhetoric to mask discretionary precedent-change decisions, raising extractiveness and theater toward Piton territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_change_measurement, empirical, 'Whether domain-context-change criteria are objective, subjective, or post-hoc.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.62) primarily structural (external barriers: reversal risk, procedural rules) or internalized (judges and litigants self-suppress anticipating appellate skepticism)?',
    'Post-exit trajectory: if lower court judges or litigants exit the precedent system (e.g., through legislative reform removing precedent-binding requirement, or judges retiring from appellate oversight), does their subsequent behavior show suppression persistence (internalized) or suppression dissipation (structural)? Alternatively, natural experiments from jurisdictions that weaken precedent-binding show whether adaptation-seeking behavior increases.',
    'If suppression is primarily structural, removing the appellate enforcement mechanism would reduce suppression. If suppression is internalized, removing the mechanism would not fully dissipate suppression — judges and litigants would continue anticipating appellate skepticism even without enforcement. Internalized suppression is harder to remedy and indicates deeper identity-fusion with the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in precedent-bound legal system.').

omega_variable(
    kernel_reading_contest,
    'Is the contest between strict_stare_decisis, pluralist_balancing, and evolutionary_framework a genuine jurisprudential disagreement about how precedent should function, or is it a cover for power competition between judicial coalitions (conservative judges preferring stare decisis, progressive judges preferring evolution, moderates claiming balancing)?',
    'Trace the historical adoption of each reading across judicial appointments, political coalitions, and doctrinal lines. If readings correlate strongly with judicial ideology and political appointments, the contest is power-driven; if readings track doctrinal logic independent of appointing authority, the contest is jurisprudential.',
    'If the reading contest is genuine jurisprudence, the pluralist reading is a legitimate middle position between two poles. If the contest is a cover for power competition, the pluralist reading may be a rhetorical resource that moderates use to claim neutrality while enabling the same extraction (precedent control) under a different framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel reading contest reflects jurisprudential disagreement or power competition.').

omega_variable(
    false_summit_risk,
    'Is the precedent corpus being presented as a natural constraint (the way courts must function to maintain rule of law and coherence) when it is actually a constructed institutional choice that benefits appellate courts and established doctrines at the cost of suppressing novel legal claims and adaptation?',
    'Historical counterfactual: what would legal systems look like if precedent-binding were substantially weaker (e.g., civil law systems, some administrative law regimes)? Do those systems show chaos/incoherence (supporting the naturality claim) or do they function reasonably while permitting more adaptation (supporting the false-summit reading)?',
    'If precedent-binding is truly natural to coherent legal systems, the constraint is a Mountain and classification would revert. If precedent-binding is a constructed choice with functional alternatives, the constraint is genuinely extractive (Snare or Tangled Rope) and the false-summit detection triggers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_risk, conceptual, 'Whether the precedent corpus is a natural constraint or a false-summit benefiting appellate courts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 1750, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1750, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(comm_tr_t1850, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1850, 0.22).
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1950, 0.31).
narrative_ontology:measurement(comm_tr_t2000, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(comm_tr_t2013, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2013, 0.41).
narrative_ontology:measurement(comm_tr_t2026, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(comm_be_t1750, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1750, 0.42).
narrative_ontology:measurement(comm_be_t1850, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1850, 0.48).
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1950, 0.54).
narrative_ontology:measurement(comm_be_t2000, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement(comm_be_t2013, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2013, 0.59).
narrative_ontology:measurement(comm_be_t2026, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1750, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1750, 0.48).
narrative_ontology:measurement(comm_su_t1850, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1850, 0.52).
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(comm_su_t2000, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(comm_su_t2013, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2013, 0.63).
narrative_ontology:measurement(comm_su_t2026, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel decomposes into three constraint stories, one per reading. Each reading instantiates a different ε, different beneficiary/victim structure, and different type. strict_stare_decisis emphasizes binding (low ε, Mountain candidate); evolutionary_framework emphasizes adaptation (high ε, Rope); pluralist_balancing emphasizes domain-specific context (intermediate ε, Tangled Rope). All three readings invoke the same kernel (precedent as binding authority) but interpret its function and extractiveness differently. They coexist in contemporary jurisprudence, held by different judicial coalitions and doctrinal traditions. The network links them to enable contamination and lifecycle analysis across the reading cluster.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
