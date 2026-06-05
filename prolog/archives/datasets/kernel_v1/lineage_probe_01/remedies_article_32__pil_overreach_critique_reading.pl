% ============================================================================
% CONSTRAINT STORY: remedies_article_32__pil_overreach_critique_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remedies_article_32__pil_overreach_critique_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remedies_article_32__pil_overreach_critique_reading
 *   human_readable: PIL Overreach: Judicial Administration as Parallel Governance
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   Public Interest Litigation (PIL) in Indian constitutional practice
 *   presents a readings contest around Article 32, which grants the Supreme
 *   Court power to issue writs for enforcement of fundamental rights. This
 *   story instantiates the 'overreach critique' reading: PIL's critics argue
 *   that what began as an exceptional remedy for the voiceless has evolved
 *   into a parallel governance apparatus where the judiciary, via continuing
 *   mandamus, administers forests, bus routes, budget allocations, and
 *   institutional management. The constraint's extractiveness lies in the
 *   relocation of policy discretion from elected executives to unelected
 *   judges. The suppression operates through the indefinite nature of
 *   continuing mandamus — the executive cannot exit the judicial supervision
 *   regime without abandoning its constitutional duty to deliver services.
 *   The theater ratio is moderate because PIL's performative content is not
 *   as high as pure ritual (piton): the courts are genuinely involved in
 *   detailed governance decisions, not merely performing review. The
 *   constraint is tangled rope because it simultaneously coordinates public
 *   accountability (genuine benefit) and extracts executive authority
 *   (genuine cost).
 *
 * KEY AGENTS:
 *   - The Executive Branch: Primary target (powerless/trapped) — subject to continuing mandamus in forests, public services, budgets; cannot exit supervision regime
 *   - Administrative Agencies: Secondary target (moderate/constrained) — operate under court-supervised implementation; face litigation exposure and loss of technical discretion
 *   - Public Interest Litigators and Petitioners: Primary beneficiary (institutional/arbitrage) — PIL grants standing and amplifies citizen voice; can petition repeatedly on governance issues
 *   - The Judiciary: Institutional actor (institutional/arbitrage) — exercises policy-making power framed as constitutional duty enforcement; maintains neutral-arbiter performance
 *   - Civil Society Coalitions: Organized actor (organized/constrained) — benefits from PIL's coordination mechanism but locked into litigation frames and dependent on judicial outcomes
 *   - The Separation of Powers Doctrine: Victim (analytical/analytical) — the structural principle is suppressed by PIL's expansion; doctrine cannot defend itself institutionally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remedies_article_32__pil_overreach_critique_reading, 0.58).
domain_priors:suppression_score(remedies_article_32__pil_overreach_critique_reading, 0.52).
domain_priors:theater_ratio(remedies_article_32__pil_overreach_critique_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remedies_article_32__pil_overreach_critique_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(remedies_article_32__pil_overreach_critique_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(remedies_article_32__pil_overreach_critique_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remedies_article_32__pil_overreach_critique_reading, tangled_rope).
narrative_ontology:human_readable(remedies_article_32__pil_overreach_critique_reading, "PIL Overreach: Judicial Administration as Parallel Governance").
narrative_ontology:topic_domain(remedies_article_32__pil_overreach_critique_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(remedies_article_32__pil_overreach_critique_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remedies_article_32__pil_overreach_critique_reading, '7c5600b4-567c-4eff-a20e-4512a0d80a1e').
narrative_ontology:cs_kernel_codification('7c5600b4-567c-4eff-a20e-4512a0d80a1e', formalized).
narrative_ontology:cs_authority_grounding('7c5600b4-567c-4eff-a20e-4512a0d80a1e', lineage).
narrative_ontology:cs_interpretation_layer_present('7c5600b4-567c-4eff-a20e-4512a0d80a1e').
narrative_ontology:cs_reading_relation('7c5600b4-567c-4eff-a20e-4512a0d80a1e', remedies_article_32__pil_epistolary_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c5600b4-567c-4eff-a20e-4512a0d80a1e', remedies_article_32__writ_arsenal_reading, influences).
narrative_ontology:cs_axiom('7c5600b4-567c-4eff-a20e-4512a0d80a1e', foundational, judicial_governance_is_overreach).
narrative_ontology:cs_axiom_status(judicial_governance_is_overreach, holdable).
narrative_ontology:cs_axiom_grounding('7c5600b4-567c-4eff-a20e-4512a0d80a1e', judicial_governance_is_overreach, deontological).
narrative_ontology:cs_axiom('7c5600b4-567c-4eff-a20e-4512a0d80a1e', foundational, separation_of_powers_is_structurally_rigid).
narrative_ontology:cs_axiom_status(separation_of_powers_is_structurally_rigid, holdable).
narrative_ontology:cs_axiom_grounding('7c5600b4-567c-4eff-a20e-4512a0d80a1e', separation_of_powers_is_structurally_rigid, deontological).
narrative_ontology:cs_reference_frame('7c5600b4-567c-4eff-a20e-4512a0d80a1e', article_32_as_emergency_remedy).
narrative_ontology:cs_drift_state('7c5600b4-567c-4eff-a20e-4512a0d80a1e', contemporary_pil_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c5600b4-567c-4eff-a20e-4512a0d80a1e', '').
narrative_ontology:cs_kernel_id(remedies_article_32__pil_overreach_critique_reading, remedies_article_32).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remedies_article_32__pil_overreach_critique_reading, court_supervised_governance_apparatus).
narrative_ontology:constraint_victim(remedies_article_32__pil_overreach_critique_reading, executive_discretionary_authority).
narrative_ontology:constraint_victim(remedies_article_32__pil_overreach_critique_reading, separation_of_powers_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXECUTIVE AUTHORITY (SNARE) — Faces continuing mandamus that suppresses discretionary action in budgeting, resource allocation, and policy implementation. The executive cannot exit the judicial supervision regime without abandoning constitutional obligation. No alternative path to administrative authority. High experienced extraction as policy decisions are relocated to the bench's docket.
constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADMINISTRATIVE AGENCIES (TANGLED ROPE) — Operate under court-supervised governance with genuine coordination function (PIL ensures environmental compliance, budget accountability, public service delivery) alongside asymmetric extraction (judicial control of implementation details, constant litigation exposure, loss of technical discretion). Constrained exit: agencies can lobby for legislative amendment but face reputational cost of appearing to resist accountability.
constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC INTEREST LITIGATORS (ROPE) — PIL provides institutional standing to petition for governance. The constraint enables coordinated accountability: citizens and advocates can articulate grievances via the courts. Beneficiary position with arbitrage options — can choose litigation venues, frame issues strategically. Experienced extraction minimal because the constraint grants them agency and amplifies their voice.
constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY AS NEUTRAL ARBITER (PITON) — The courts see PIL as legitimate constitutional remedy application, but in practice the judiciary has become a governance actor without legislative mandate or budget answerability. The theater persists: PIL frames itself as enforcing existing duties (mandamus for existing statutory obligations) but functionally redistributes authority to the bench. The judicial system maintains the neutral-arbiter performance despite operating as an extractive policy-making apparatus.
constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY COALITIONS (TANGLED ROPE) — Genuine coordination benefit: PIL enables organized collective voice on forest conservation, public transportation, budget equity. But also extraction: dependence on litigation creates resource asymmetry (wealthy NGOs with legal teams dominate), judicial outcomes are opaque to democratic input, and coalitions become locked into adversarial litigation frames rather than negotiation.
constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — A civilizational view might see PIL as an immutable feature of constitutional rights enforcement: wherever fundamental rights require protection, courts must have standing to intervene. This naturalizes judicial governance as constitutionally inherent. However, the structural data contradicts the mountain classification — beneficiaries exist (court-supervised governance apparatus), victims exist (executive discretion), and active enforcement is required. The engine will identify this as a false summit, revealing that 'judicial accountability is fundamental' naturalizes a contingent institutional arrangement.
constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remedies_article_32__pil_overreach_critique_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remedies_article_32__pil_overreach_critique_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(remedies_article_32__pil_overreach_critique_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(remedies_article_32__pil_overreach_critique_reading, TR),
    TR >= 0.70.

:- end_tests(remedies_article_32__pil_overreach_critique_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. PIL extracts executive discretion (relocation to bench) and suppresses alternative remedial pathways (executive reform, legislative intervention). The extraction is not as severe as pure predatory mechanisms because PIL does enforce genuine rights and produce real governance improvements (environmental compliance, public service accountability). The measured value reflects that extraction coexists with coordination function. Suppression (0.52): Moderate-high. The executive cannot easily exit the PIL regime — continuing mandamus means indefinite judicial supervision. But suppression is not total because executives retain some discretion (within judicial parameters) and can lobby for legislative amendment or constitutional change (high-cost but possible). Theater ratio (0.48): Moderate. PIL's performative content is lower than ritual-bound institutions because courts are genuinely engaged in detailed governance. But theater is non-negligible because the framing of these orders as 'enforcement of duties' (rather than 'judicial governance') is partly performative — courts exercise discretion that looks like duty-application. The theater has increased slightly over the 30-year interval as PIL orders have become more intrusive and detailed.
 *
 * PERSPECTIVAL GAP:
 *   The structural gap divides primarily on the axis of power and exit. The executive branch (powerless/trapped) experiences this as a snare: complete loss of discretion with no escape. Organized civil society (organized/constrained) experiences it as tangled rope: genuine benefit from accountability mechanism but locked into litigation paths. The judiciary (institutional/arbitrage) experiences it as rope: coordinate benefit (legitimate rights enforcement) with minimal cost (judicial labor is internal to the institution). The analytical observer risks misclassifying this as a mountain (constitutional inevitability) when it is actually a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. The executive branch bears victim status and faces trapped exit (no escape from mandamus regime), yielding high d (0.92-0.95) and high f(d) (1.35+), producing high experienced extractiveness χ. Public interest litigators enjoy beneficiary status and arbitrage exit (can choose litigation venues and strategies), yielding low d (0.10-0.15) and low/negative f(d) (-0.02 to 0.10), producing low/negative χ. Administrative agencies face victim-adjacent status with constrained exit (can resist costly but possible), yielding medium-high d (0.65-0.75) and moderate f(d) (0.90-1.05). The scope modifier σ(S) applies national scope (1.0), so χ = ε × f(d) × 1.0 for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that PIL genuinely coordinates public accountability while also extracting executive discretion. The coordination function is real: PIL creates mechanisms for citizen petition, enforces service delivery standards, and prevents bureaucratic inaction. The extraction is also real: continuing mandamus relocates policy decisions to unelected judges, suppresses executive discretion, and shifts the forum for governance questions from legislatures to courts. The constraint is neither pure coordination (rope) nor pure extraction (snare) because both functions are structural and irreducible. The tangled rope classification reflects this hybrid reality. The perspectival gaps (snare from executive view, rope from PIL perspective, piton from judiciary) are not resolved by picking one type — they are resolved by recognizing that different institutional actors experience genuinely different extraction patterns from the same structural mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandamus_scope_boundary,
    'Where does enforcement of existing duties (legitimate mandamus) end and creation of new judicial policy begin?',
    'Systematic review of PIL orders: categorization by whether the order enforces pre-existing statute/constitutional duty vs. creates new affirmative obligations (e.g., forest management plans, bus fleet specifications, budget reallocation formulas). Statistical analysis of boundary cases.',
    'If boundary is clear and enforced: PIL remains a writs mechanism (Rope to many perspectives). If boundary is permeable or ignored: PIL is functionally judicial legislation (Snare or Tangled Rope to all perspectives). Affects crisis classification under ''mandamus overreach'' omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandamus_scope_boundary, empirical, 'Boundary between enforcing duties and creating new judicial policy').

omega_variable(
    separation_of_powers_constitutional_elasticity,
    'Is separation of powers a rigid structural requirement or a flexible principle that yields to fundamental rights enforcement?',
    'Jurisprudential analysis: track how courts justify PIL orders that intrude on executive function. If justified as exceptions to separation of powers, elasticity is limited. If justified as consistent with judicial role, elasticity is high. Comparative constitutional law: how other democracies handle tension between rights enforcement and institutional boundaries.',
    'If rigid: PIL overreach is constitutional violation (Snare from all perspectives except judiciary). If elastic: PIL is legitimate adaptation (Tangled Rope with varying perspectives). This is a conceptual/axioms omega — where the reading disagrees with siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(separation_of_powers_constitutional_elasticity, conceptual, 'Whether separation of powers is rigid or elastic under PIL pressure').

omega_variable(
    executive_remedy_adequacy_counterfactual,
    'If executive and legislative branches had responsive mechanisms (public interest ombudsmen, environmental ministries with real authority, budget courts), would PIL orders decline in scope?',
    'Retrospective analysis: jurisdictions that created parallel administrative remedies (environmental courts, budget advisory bodies, ombudsman offices) and tracked PIL litigation trends. Do PIL orders decline or shift to different administrative actions?',
    'If PIL is responsive to executive remedy gaps: extractiveness is lower than measured (courts fill vacuum, not exploit it) — reclassify as low-extraction Rope. If PIL persists despite remedies: extractiveness is genuine (judges prefer discretionary power) — Snare from executive perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_remedy_adequacy_counterfactual, empirical, 'Whether PIL overreach reflects remedy gaps or judicial preference for discretion').

omega_variable(
    reading_contest_kernel_identity,
    'What kernel of Article 32 is being read in this critique? Is it the same kernel as the epistolary and arsenal readings?',
    'Textual analysis: identify which aspects of Article 32 this reading emphasizes (mandamus as ongoing supervisory mechanism vs. episodic relief vs. arsenal of prerogative writs). Map the overlaps and divergences with sibling readings.',
    'If same kernel: readings genuinely coexist and compete for legitimacy. If different kernels: the ''kernel contest'' is actually three constraints with different ε values, and network decomposition is appropriate. This omega documents the committer-frame uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether this reading shares the contested kernel with sibling readings or points to a different constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remedies_article_32__pil_overreach_critique_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pil_overreach_tr_t0, remedies_article_32__pil_overreach_critique_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pil_overreach_tr_t15, remedies_article_32__pil_overreach_critique_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(pil_overreach_tr_t30, remedies_article_32__pil_overreach_critique_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(pil_overreach_be_t0, remedies_article_32__pil_overreach_critique_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pil_overreach_be_t15, remedies_article_32__pil_overreach_critique_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(pil_overreach_be_t30, remedies_article_32__pil_overreach_critique_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pil_overreach_su_t0, remedies_article_32__pil_overreach_critique_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(pil_overreach_su_t15, remedies_article_32__pil_overreach_critique_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(pil_overreach_su_t30, remedies_article_32__pil_overreach_critique_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remedies_article_32__pil_overreach_critique_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remedies_article_32__pil_overreach_critique_reading, remedies_article_32__pil_epistolary_reading).
narrative_ontology:affects_constraint(remedies_article_32__pil_overreach_critique_reading, remedies_article_32__writ_arsenal_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the Article 32 kernel contest. The three readings (epistolary, overreach critique, arsenal) represent different institutional readings of the same constitutional text. They are NOT decomposed by the ε-invariance principle (they are not observationally different measurements of one constraint) but rather by the kernel-reading principle: one text, three structurally distinct readings held by different institutional actors. Each reading produces a different extractiveness value and beneficiary/victim set because each reads Article 32's role differently. This is not a case where alternative observables change ε for the same constraint; it is a case where different stakeholders literally disagree on what Article 32 is for.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
