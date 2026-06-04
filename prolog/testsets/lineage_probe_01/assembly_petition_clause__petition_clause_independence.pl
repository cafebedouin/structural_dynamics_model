% ============================================================================
% CONSTRAINT STORY: assembly_petition_clause__petition_clause_independence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_assembly_petition_clause__petition_clause_independence, []).

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
 *   constraint_id: assembly_petition_clause__petition_clause_independence
 *   human_readable: Petition Clause Independence: Doctrinal Merger and the Forgotten Right to Demand Answer
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The petition clause—'the right of the people...to petition the Government
 *   for a redress of grievances'—is textually distinct from speech and
 *   assembly protections in the First Amendment. Yet modern constitutional
 *   doctrine has submerged petition into speech analysis, treating petition
 *   claims as variants of expressive-activity protection rather than as a
 *   structural right to demand government response. This reading instantiates
 *   petition clause independence: the claim that petition is a distinct
 *   constitutional guarantee whose suppression by doctrinal merger into
 *   speech doctrine constitutes extractive subordination of a separate right
 *   to beneficiaries of unified speech analysis. The kernel
 *   (assembly_petition_clause) is contested across three readings: (1)
 *   expressive_association_doctrine reads assembly as the right to organize
 *   and associate for expressive purposes; (2) permit_system_limits reads
 *   assembly as encounter with state licensing and the constitutional limits
 *   thereon; (3) petition_clause_independence reads petition as the forgotten
 *   right to demand answer, distinct from and suppressed by speech doctrine.
 *   This constraint focuses on reading (3): the structural suppression of
 *   petition independence by doctrinal merger. The extractiveness measurement
 *   traces the increasing absorption of petition language into speech
 *   framework across the 20th–21st centuries (t0=pre-merger framework,
 *   t100=current state), while theater ratio tracks the performative
 *   invocation of petition clause in judicial opinions despite its functional
 *   dormancy.
 *
 * KEY AGENTS:
 *   - Petition Claimants (Citizens): Victims (powerless/trapped) — cannot articulate petition-distinct injury; no standing when merger into speech doctrine eliminates petition cause of action
 *   - Marginalized Advocacy Groups: Secondary victims (moderate/constrained) — benefit from speech protections (no permit required) but lose distinct petition right (redress demand unenforceable)
 *   - Unified Speech Doctrine / Federal Courts: Beneficiary (institutional/arbitrage) — benefits from merger (simpler analysis, consistency, reduced litigation burden); can shift doctrine without bearing suppression costs
 *   - Government / Executive Branch: Institutional beneficiary (institutional/constrained) — avoids explicit petition-response duties (buried in speech doctrine); maintains regulatory authority under speech framework
 *   - Legal Doctrine / Judicial Institutions: Inertial beneficiary (institutional/arbitrage) — maintains merger through doctrinal momentum and institutional convenience; performatively invokes petition while functionalizing speech
 *   - Analytical Observer / Constitutional Scholars: Analytical position (analytical/analytical) — risks naturalizing contingent doctrinal choice as inevitable constitutional law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(assembly_petition_clause__petition_clause_independence, 0.52).
domain_priors:suppression_score(assembly_petition_clause__petition_clause_independence, 0.68).
domain_priors:theater_ratio(assembly_petition_clause__petition_clause_independence, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(assembly_petition_clause__petition_clause_independence, extractiveness, 0.52).
narrative_ontology:constraint_metric(assembly_petition_clause__petition_clause_independence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(assembly_petition_clause__petition_clause_independence, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(assembly_petition_clause__petition_clause_independence, tangled_rope).
narrative_ontology:human_readable(assembly_petition_clause__petition_clause_independence, "Petition Clause Independence: Doctrinal Merger and the Forgotten Right to Demand Answer").
narrative_ontology:topic_domain(assembly_petition_clause__petition_clause_independence, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(assembly_petition_clause__petition_clause_independence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(assembly_petition_clause__petition_clause_independence, 'f32e62be-4f37-475e-bb75-dd5001c894e8').
narrative_ontology:cs_kernel_codification('f32e62be-4f37-475e-bb75-dd5001c894e8', fixed_text).
narrative_ontology:cs_authority_grounding('f32e62be-4f37-475e-bb75-dd5001c894e8', lineage).
narrative_ontology:cs_interpretation_layer_present('f32e62be-4f37-475e-bb75-dd5001c894e8').
narrative_ontology:cs_reading_relation('f32e62be-4f37-475e-bb75-dd5001c894e8', assembly_petition_clause__expressive_association_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('f32e62be-4f37-475e-bb75-dd5001c894e8', assembly_petition_clause__permit_system_limits, influences).
narrative_ontology:cs_axiom('f32e62be-4f37-475e-bb75-dd5001c894e8', foundational, petition_right_structurally_distinct).
narrative_ontology:cs_axiom_status(petition_right_structurally_distinct, holdable).
narrative_ontology:cs_axiom_grounding('f32e62be-4f37-475e-bb75-dd5001c894e8', petition_right_structurally_distinct, deontological).
narrative_ontology:cs_axiom('f32e62be-4f37-475e-bb75-dd5001c894e8', foundational, response_duty_enforceability).
narrative_ontology:cs_axiom_status(response_duty_enforceability, holdable).
narrative_ontology:cs_axiom_grounding('f32e62be-4f37-475e-bb75-dd5001c894e8', response_duty_enforceability, conventional).
narrative_ontology:cs_reference_frame('f32e62be-4f37-475e-bb75-dd5001c894e8', petition_as_distinct_guarantee).
narrative_ontology:cs_drift_state('f32e62be-4f37-475e-bb75-dd5001c894e8', contemporary_doctrinal_merger, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('f32e62be-4f37-475e-bb75-dd5001c894e8', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(assembly_petition_clause__petition_clause_independence, assembly_petition_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(assembly_petition_clause__petition_clause_independence, unified_speech_doctrine).
narrative_ontology:constraint_victim(assembly_petition_clause__petition_clause_independence, petition_right_distinct_guarantees).
narrative_ontology:constraint_victim(assembly_petition_clause__petition_clause_independence, redress_access_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PETITION CLAIMANT / NO STANDING (SNARE) — Citizens demanding answer from government have no recognized legal remedy when merged into speech doctrine. The right to petition is submerged; the claimant cannot articulate injury distinct from speech suppression. Trapped by doctrinal closure that eliminates the petition cause of action. Maximum experienced extraction — the distinct guarantee (response/redress) is unenforceable.
constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED ADVOCACY GROUP (TANGLED ROPE) — Organizations seeking government response to grievances benefit from speech protections (can speak without licensing) but lose the distinct petition right (no enforceable duty of government response). Constrained by the merged doctrine: they can exercise speech without permit, but demand-right is litigated as speech claim, losing the structural force of petition. Mixed coordination (speech framework enables expression) and extraction (petition guarantee is sacrificed to that framework).
constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIFIED SPEECH DOCTRINE / CONSTITUTIONAL INTERPRETATION (ROPE) — Legal doctrine benefits from merger of petition into speech: simplifies analysis, reduces litigation burden on courts, and provides consistent framework for all expression rights. The institutional position (federal courts, academic interpretation) experiences the constraint as coordination—the unified framework enables consistent adjudication. Beneficiary with arbitrage options (can shift doctrine if needed without bearing costs of the suppression).
constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RESPONSIVE GOVERNMENT / ADMINISTRATIVE (TANGLED ROPE) — Government benefits from the doctrinal merger because it eliminates a distinct legal duty to respond to petitions (buried in speech doctrine, petition claims are harder to win). But government also coordinates through the speech framework (can regulate speech subject to First Amendment limits, establishing predictable boundaries). The constraint enables government to avoid explicit petition-response duties while maintaining speech-regulation authority. Constrained by need to justify regulations under speech doctrine rather than petition doctrine.
constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL DOCTRINE / INSTITUTIONAL INERTIA (PITON) — The merger of petition into speech is largely maintained through doctrinal momentum and institutional convenience rather than principled adjudication. Courts invoke the unified framework performatively; the original petition clause remains cited but functionally dormant. The theatrical performance (invoking petition's historical importance while submerging its application) sustains the merged doctrine without addressing the structural suppression of the distinct right. Theater ratio high because the doctrine performs constitutional fidelity while eliminating a distinct cause of action.
constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, petition-as-demand-for-response could appear as a natural structural feature of legitimate government: any system needs mechanisms for subjects to demand answer. But this reading risks naturalizing a contingent doctrinal choice. The engine's false summit detector will flag this as naturalization of the speech-doctrine merger, not as an immutable constitutional law.
constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(assembly_petition_clause__petition_clause_independence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(assembly_petition_clause__petition_clause_independence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(assembly_petition_clause__petition_clause_independence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(assembly_petition_clause__petition_clause_independence, TR),
    TR >= 0.70.

:- end_tests(assembly_petition_clause__petition_clause_independence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The merger of petition into speech doctrine produces real extraction: petition claimants lose a distinct legal right (the demand-right with response/redress guarantees) and unified speech doctrine benefits (simpler analysis, institutional consistency, reduced litigation burden). However, extractiveness is not maximal (0.72+) because speech protections themselves provide real protection—claimants are not left with zero recourse, merely with a less-powerful cause of action. The extraction is the difference between petition-distinct standing and speech-only standing: some claims that would succeed under petition doctrine fail under speech doctrine. Suppression (0.68): Moderate-high. The doctrinal merger suppresses petition's distinct guarantees through several mechanisms: (1) standing doctrine requires injury in-fact framed as speech suppression, not as unresponsiveness; (2) remedy for petition claims is litigated as speech remedy (injunction against licensing, not injunction for government response); (3) historical petition practice (direct petitions for redress, audience with decision-maker) is not recognized as the right's core content; (4) courts cite petition clause but treat it as redundant with speech. Suppression is not total (petition is still textually present, occasionally cited) but substantial. Theater ratio (0.55): Moderate. The constraint exhibits performative invocation: judicial opinions cite petition clause to show respect for founding-era rights while functionalizing speech doctrine and rendering petition claims non-distinct. The theater is increasing over time (t0 to t100) as merger doctrine becomes standard and petition-specific language disappears from new opinions, replaced by speech-merger language. Theater is not as high as piton-level (0.70+) because the suppression mechanism is still active—courts are not maintaining a purely theatrical distinction, but rather actively merging petition into speech. The theater consists in the gap between textual presence (petition clause is still there) and functional absence (no distinct cause of action).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between petition claimant (Snare) and speech doctrine beneficiary (Rope) is maximal. From the claimant's perspective, the merged doctrine is extractive—they lose a distinct guarantee. From the speech doctrine institutional perspective, the merger is coordinating—it provides consistent framework and enables predictable adjudication. The marginalized advocacy group experiences mixed extraction (Tangled Rope): they gain speech protections (no permit) but lose petition redress. Government experiences Tangled Rope: it avoids explicit response duties (extraction benefit) but still must justify speech regulations (coordination constraint). The doctrinal system itself (Piton) maintains the merger through performative invocation. The analytical observer (Mountain) risks naturalizing the merger as inevitable—this is where false summit detection applies. The gap reveals that petition independence is a live constitutional question, not settled doctrine, and the merger benefits unified speech analysis at the cost of suppressing a distinct right.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation flows from structural position: petition claimants are victims of the merger (high d, high experienced extraction). Unified speech doctrine is the beneficiary (low d, negative/low experienced extraction). Marginalized groups are partly victim, partly beneficiary (moderate d, mixed extraction). Government is beneficiary-constrained (low-moderate d, moderate extraction from its perspective). The derivation is straightforward: the merger allocates doctrinal convenience to the unified framework beneficiaries (courts, doctrine, government simplification) and allocates costs to petition-claimant victims (lost standing, lost distinct remedy). The suppression mechanism (standing doctrine, remedy limitation) is institution-specific—how federal courts adjudicate claims—and activates differential extraction for different agents depending on their structural position relative to the courts.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint instantiates the petition clause independence reading and resolves the mandatrophy through doctrinal specificity. The question 'Is petition independent or merged into speech?' is not yet settled in constitutional law—it is precisely the unlitigated question. The constraint's extractiveness (0.52) reflects the measured suppression of petition-distinct guarantees without collapse to pure snare (0.72+) because the underlying speech protections are real and functional. The constraint is Tangled Rope: genuine coordination function (unified framework enables consistent adjudication) exists alongside asymmetric extraction (petition guarantees are sacrificed to that framework). The constraint resolves mandatrophy by showing that petition suppression is a specific, litigable question, not a false dilemma between 'petition is speech' and 'petition is utterly lost.' The third reading (this one) opens the recovery question: can courts restore petition independence while preserving speech doctrine? Can petition and speech coexist as distinct protections? The unlitigated status of these questions is why the constraint's extractiveness is measured precisely: it reflects the real structural pressure (merger suppresses petition) without settled resolution (the doctrine is still drifting).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    petition_distinct_right_scope,
    'Does the petition clause guarantee a distinct right to demand government response, separate from freedom of speech protections?',
    'Comparative analysis of constitutional text (explicit petition clause + separate speech protection); historical practice pre-merger showing distinct petition remedies; modern litigation establishing whether petition claims produce distinct standing or only speech-suppression standing.',
    'If distinct: current doctrine suppresses a constitutional right (extraction confirmed, extractiveness > 0.60). If merged-by-nature: petition is coordinate protection within speech, and merger is coordination not extraction (extractiveness < 0.35, reclassifies to Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(petition_distinct_right_scope, empirical, 'Whether petition right is structurally distinct from speech protections or merged by constitutional design').

omega_variable(
    government_response_duty_enforceability,
    'Can litigants establish government duty to respond to petitions as a distinct, enforceable right separate from speech-suppression claims?',
    'Survey of appellate decisions: Do courts recognize petition-distinct claims? Can litigants win on petition grounds without speech-suppression grounds? Do standing doctrines permit petition standing where speech standing would fail?',
    'If enforceable: petition independence is a live constitutional question (Tangled Rope confirmed). If unenforceable: petition is fully absorbed into speech doctrine (Snare/Piton confirmed; no distinct cause of action exists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_response_duty_enforceability, empirical, 'Whether petition-distinct claims have independent legal enforceability').

omega_variable(
    doctrinal_merger_intent_vs_necessity,
    'Was the merger of petition into speech doctrine a deliberate interpretive choice by courts, or an inevitable consequence of constitutional structure?',
    'Historical analysis of Supreme Court doctrine: explicit statements about petition clause scope; comparison of petition-distinct opinions to speech-merged opinions; identification of moment doctrinal merger became standard (likely 20th century); analysis of alternative frameworks that could preserve petition independence.',
    'If deliberate choice: the merger is a contingent doctrinal arrangement benefiting unified framework analysis (extractive, Tangled Rope confirmed). If necessity: the merger reflects constitutional structure, and petition independence is not recoverable (mountain-ish, low extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_merger_intent_vs_necessity, conceptual, 'Whether petition-speech merger is deliberate doctrinal choice or constitutional necessity').

omega_variable(
    petition_clause_redress_content,
    'What specific redress or response does the petition clause guarantee: acknowledgment, explanation, remedy, or merely access to decision-maker?',
    'Historical petition practice (petitions to medieval monarchs, colonial assemblies); textual analysis of ''petition for redress of grievances'' language; comparative constitutional analysis of petition rights in other democracies (EU Charter right to petition, Canadian petition mechanisms).',
    'If robust redress required: extractiveness of suppression is higher (government loses duty, citizens lose remedy). If access-only: extractiveness is moderate (procedural right is lost, but substantive obligation was never required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(petition_clause_redress_content, empirical, 'Scope of redress content guaranteed by petition clause').

omega_variable(
    false_summit_naturalness_test,
    'Is the suppression of petition clause independence a false summit—naturalized as inevitable constitutional law—when it is actually a contingent doctrinal merger?',
    'Cross-reading analysis: comparison of this reading (petition independence) with sibling readings (speech doctrine, permit systems); historical counterfactual analysis showing alternative doctrinal paths that would preserve petition distinction.',
    'If false summit: the mountain classification at analytical perspective should trigger FSM engine detection, flagging the ''naturalness'' as doctrinal choice not constitutional law. The constraint is Tangled Rope, not Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness_test, conceptual, 'Whether petition suppression is naturalized doctrinal choice or inevitable constitutional necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(assembly_petition_clause__petition_clause_independence, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(petition_indep_theater_t0, assembly_petition_clause__petition_clause_independence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(petition_indep_theater_t50, assembly_petition_clause__petition_clause_independence, theater_ratio, 50, 0.48).
narrative_ontology:measurement(petition_indep_theater_t100, assembly_petition_clause__petition_clause_independence, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(petition_indep_extract_t0, assembly_petition_clause__petition_clause_independence, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(petition_indep_extract_t50, assembly_petition_clause__petition_clause_independence, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(petition_indep_extract_t100, assembly_petition_clause__petition_clause_independence, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(petition_indep_suppress_t0, assembly_petition_clause__petition_clause_independence, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(petition_indep_suppress_t50, assembly_petition_clause__petition_clause_independence, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(petition_indep_suppress_t100, assembly_petition_clause__petition_clause_independence, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(assembly_petition_clause__petition_clause_independence, enforcement_mechanism).
narrative_ontology:affects_constraint(assembly_petition_clause__petition_clause_independence, assembly_petition_clause__expressive_association_doctrine).
narrative_ontology:affects_constraint(assembly_petition_clause__petition_clause_independence, assembly_petition_clause__permit_system_limits).

% DUAL FORMULATION NOTE:
% Petition clause independence is one reading of the contested kernel assembly_petition_clause. The kernel describes the constitutional text and its modern life in doctrine; three readings instantiate three distinct constraints reflecting different structural understandings. (1) Expressive association doctrine: assembly = right to associate for expression; (2) permit system limits: assembly = licensed activity with constitutional boundaries; (3) petition independence (this constraint): petition = forgotten right to demand response, distinct from speech. Each reading is a separate constraint with its own ε, perspectives, and beneficiary/victim structure. The three readings coexist as different parties' constitutional interpretations; they are linked via network to show the kernel contest. See commentary.kernel_context for the full reading relations and axiom set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
