% ============================================================================
% CONSTRAINT STORY: expert_authority_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expert_authority_asymmetry, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: expert_authority_asymmetry
 *   human_readable: Expert Authority Asymmetry
 *   domain: epistemic/governance/institutional
 *
 * SUMMARY:
 *   Expert authority asymmetry operates as a structural constraint across all
 *   knowledge-intensive domains: medicine, law, finance, science,
 *   engineering, environmental policy. The constraint creates systematic
 *   extraction from lay decision-makers who depend on expert judgment but
 *   cannot verify claims, while simultaneously coordinating genuine epistemic
 *   functions (knowledge verification, quality control, liability
 *   management). The tension between legitimate specialization and artificial
 *   credential maintenance generates six distinct experienced realities:
 *   powerless agents trapped in mandatory deference (snare), moderate
 *   professionals constrained by licensing requirements (tangled rope),
 *   experts benefiting from gatekeeping (rope), alternative knowledge
 *   communities suppressed by regulation (tangled rope), credentialist
 *   institutions maintaining performative rituals (piton), and civilizational
 *   observers tempted to naturalize extraction as inherent complexity (false
 *   summit). The theater ratio has increased from 0.40 to 0.68 over the
 *   interval, indicating credential inflation and proliferation of
 *   certifications whose epistemic content has decayed relative to their
 *   gatekeeping function.
 *
 * KEY AGENTS:
 *   - Lay Decision-Makers: Primary victim (powerless/trapped) — dependent on expert judgment for medical, financial, legal, environmental decisions; cannot verify claims; no exit option
 *   - Credentialed Experts: Primary beneficiary (institutional/arbitrage) — capture deference monopoly, licensing rents, and epistemic authority; experience constraint as legitimate coordination
 *   - Professional Non-Experts: Secondary victim (moderate/constrained) — face licensing requirements and deference mandates; benefit from credential access but constrained by expert gatekeeping
 *   - Alternative Knowledge Communities: Secondary victim (organized/constrained) — produce competing knowledge (indigenous practices, participatory science, craft expertise); suppressed through regulatory prohibition and institutional devaluation
 *   - Credentialist Institutions: Institutional actor (institutional/arbitrage) — licensing boards, professional associations, academic institutions maintain credential barriers through performative rituals; experience constraint as inertial maintenance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent extraction as inherent epistemic complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expert_authority_asymmetry, 0.58).
domain_priors:suppression_score(expert_authority_asymmetry, 0.62).
domain_priors:theater_ratio(expert_authority_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expert_authority_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(expert_authority_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(expert_authority_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expert_authority_asymmetry, tangled_rope).
narrative_ontology:human_readable(expert_authority_asymmetry, "Expert Authority Asymmetry").
narrative_ontology:topic_domain(expert_authority_asymmetry, "epistemic/governance/institutional").

domain_priors:requires_active_enforcement(expert_authority_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expert_authority_asymmetry, credentialed_experts).
narrative_ontology:constraint_beneficiary(expert_authority_asymmetry, knowledge_gatekeepers).
narrative_ontology:constraint_victim(expert_authority_asymmetry, lay_decision_makers).
narrative_ontology:constraint_victim(expert_authority_asymmetry, non_expert_stakeholders).
narrative_ontology:constraint_victim(expert_authority_asymmetry, alternative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY DECISION-MAKER (SNARE) — Dependent on expert judgment for critical life decisions (medical, financial, legal, environmental) yet cannot verify expert claims. No meaningful exit: must defer or face catastrophic consequences from uninformed choices. Trapped by asymmetric information and structural prohibition on operating without expert sanction.
constraint_indexing:constraint_classification(expert_authority_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROFESSIONAL NON-EXPERT (TANGLED ROPE) — Constrained by credentialism requirements and professional licensing standards that require deference to experts. Benefits from the expertise system through access to vetted knowledge; bears extraction through restricted professional mobility and mandatory compliance with expert authority. Can exit only at high career cost.
constraint_indexing:constraint_classification(expert_authority_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALED EXPERT (ROPE) — Experiences expert authority as coordination mechanism: peer review, professional standards, and disciplinary gatekeeping solve genuine epistemic problems (verification, quality control, liability). Net beneficiary — extraction runs toward this agent through deference, licensing requirements, and monopoly on recognized knowledge claims.
constraint_indexing:constraint_classification(expert_authority_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE KNOWLEDGE COMMUNITY (TANGLED ROPE) — Organized agents (indigenous knowledge holders, participatory science networks, craft practitioners) coordinate real knowledge production outside credentialist frameworks. Extraction occurs through institutional devaluation and legal prohibition on unlicensed practice; coordination occurs through shared epistemology and mutual recognition. Constrained by regulatory barriers that prevent market-testing alternative approaches.
constraint_indexing:constraint_classification(expert_authority_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALIST INSTITUTION (PITON) — Licensing boards, professional associations, and academic institutions maintain the credential barrier through performative rituals (thesis defenses, certification exams, peer review committees) whose primary function has atrophied relative to their ceremonial overhead. Theater ratio high (0.65): much institutional activity is reputation-maintenance rather than actual knowledge verification. Persists through inertia and mutual institutional protection rather than demonstrated epistemic superiority.
constraint_indexing:constraint_classification(expert_authority_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some epistemic authority asymmetry is inherent to knowledge production: specialized domains have irreducible complexity that makes lay judgment unreliable, and verification requires training. This perspective risks naturalizing what is actually a contingent institutional arrangement maintained by credential gatekeeping. Engine false summit detection should flag this as attempted naturalization of extraction.
constraint_indexing:constraint_classification(expert_authority_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expert_authority_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expert_authority_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expert_authority_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expert_authority_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expert_authority_asymmetry, TR),
    TR >= 0.70.

:- end_tests(expert_authority_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The original research group captures career and funding benefits during the verification window, but this assessment reflects genuine tension between justified specialization and artificial gatekeeping. The extractiveness value reflects the net extraction after accounting for real coordination benefits of professional standards. The upward trajectory (0.35 → 0.62) indicates credential inflation outpacing epistemic necessity. Suppression (0.62): High. Multiple barriers prevent lay agents from operating without expert sanction: legal prohibition on unlicensed practice, technical opacity, epistemological incompleteness (lay agents cannot access sufficient information to verify), reputational penalties for expert challenge, and institutional gatekeeping. However, some barriers are epistemic necessities (surgical complexity) while others are artificial (professional jargon, licensing requirement expansion). Theater ratio (0.65): Moderate-high. Institutional credentialism exhibits significant performative content: degree inflation (degrees now required for positions previously requiring on-the-job training), certification proliferation (each credential adds bureaucratic layer without clear competence improvement), and professional ritual maintenance (board exams, thesis defenses, peer review ceremonies that function primarily for credential-holder signaling rather than knowledge verification). The trajectory indicates theater ratio increasing faster than extractiveness, diagnostic of piton dynamics: institutional function degrading while ceremonial overhead increases.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence across structural perspectives. Expert experiences rope (legitimate coordination of knowledge verification); lay agent experiences snare (mandatory deference with no exit); non-expert professional experiences tangled rope (mixed coordination benefit and constraint); alternative knowledge system experiences tangled rope with suppression (competitors systemically devalued); institution experiences piton (performative ritual maintenance); analytical observer risks mountain (naturalizes contingent extraction). The gap reveals that expert authority asymmetry is not a natural law but a contingent institutional arrangement with growing extraction component (theater ratio trajectory) and declining epistemic necessity justification (credential inflation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Lay decision-makers are trapped victims with no exit (d ≈ 0.95, f(d) ≈ 1.42) experiencing maximum extraction chi. Credentialed experts are beneficiaries with arbitrage options (d ≈ 0.05, f(d) ≈ -0.12) experiencing negative extraction (they are the extraction beneficiaries). Professional non-experts are constrained victims (d ≈ 0.65, f(d) ≈ 1.00) experiencing moderate extraction. Alternative knowledge communities are organized but constrained (d ≈ 0.55, f(d) ≈ 0.75) experiencing moderate-high extraction from regulatory suppression. Credentialist institutions are beneficiary arbitragers (d ≈ 0.15, f(d) ≈ -0.01) with extracted rents flowing toward them. The piton classification derives from theater gate (theater_ratio ≥ 0.70) rather than high chi at generational timescale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that all six types are legitimate perspectival readings until the credential inflation dynamics are measured. The base case shows mixed tangled rope (coordination + extraction at institutional level) and snare (pure extraction from lay perspective). The temporal measurement trajectory (theater ratio 0.40 → 0.68, extractiveness 0.35 → 0.62) shows theater increasing faster than extractiveness, diagnostic of piton emergence: the constraint is degrading from tangled rope toward piton as credentialism becomes more performative. The analytical observer's mountain classification is a false summit: from civilizational timescale, expertise does require specialization, but the credential inflation trajectory reveals that the constraint's primary function is increasingly institutional self-maintenance rather than epistemic necessity. The mandatrophy resolves by tracking measurement divergence: if theater and extractiveness remained coupled (both rising proportionally), the constraint would remain tangled rope. Their divergence indicates piton transition, revealing that institutional credentialism is increasingly extractive theater masquerading as necessary specialization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specialization_necessity_threshold,
    'What proportion of expert authority is necessary specialization vs. artificially maintained credential barrier?',
    'Comparative analysis of barrier effects: contrast credential-requiring domains (medicine, law, engineering) with low-barrier high-stakes domains (investment advisory, journalism) and high-barrier low-stakes domains (cosmetology, barber licensing); measure adverse outcome rates and public satisfaction across credential tightness',
    'If threshold > 0.70 necessary: most expert authority is justified specialization (mountain gates marginally). If threshold < 0.40 necessary: most authority is credential maintenance (snare classification becomes dominant across perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialization_necessity_threshold, empirical, 'Proportion of expert authority that is necessary specialization vs. artificial barrier').

omega_variable(
    alternative_epistemic_viability,
    'Do alternative knowledge systems (indigenous, participatory science, craft expertise) produce measurably comparable or superior outcomes when not suppressed by regulatory prohibition?',
    'Comparative outcome measurement in domains where alternatives have market access (e.g., agricultural practices, traditional medicine outcomes in permissive jurisdictions); identify whether suppression is epistemically justified or economically motivated',
    'If alternatives produce superior outcomes: expert authority asymmetry is pure extraction (snare from lay perspective). If alternatives produce comparable outcomes: asymmetry is coordination with residual extraction (tangled rope). If alternatives produce worse outcomes: asymmetry is justified necessity (mountain gates apply).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_epistemic_viability, empirical, 'Comparative viability of alternative knowledge systems').

omega_variable(
    credential_signal_decay,
    'Do academic credentials and professional licenses actually signal competence or have they become pure signals of credential-acquisition capacity?',
    'Regression analysis of credential status on actual performance; compare performance predictiveness of credentials vs. demonstrated work portfolio; measure credential inflation over time (grade inflation, degree proliferation, certification creep)',
    'If signal decay > 0.60: credentials function as pure barrier maintenance (piton + snare). If decay < 0.30: credentials remain valid knowledge signals (rope justification holds). Medium decay: mixed mechanism (tangled rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_signal_decay, empirical, 'Whether credentials signal competence or credential-acquisition capacity').

omega_variable(
    verification_accessibility_engineering,
    'Is expert authority inherently unverifiable to lay agents, or is unverifiability engineered through technical opacity and jargon gatekeeping?',
    'Experimental comparison: present expert claims in technical language vs. simplified explanation to comparable lay audiences; measure comprehension rates and quality of reasoning; identify which aspects of complexity are epistemic necessity vs. artificial obscuration',
    'If engineered > 0.60: suppression is artificial (snare strengthened). If engineered < 0.30: complexity is genuine (mountain gates stronger). Medium engineering: suppression is partially structural (tangled rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_accessibility_engineering, empirical, 'Whether expert authority unverifiability is structural or engineered').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expert_authority_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eaa_tr_t0, expert_authority_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(eaa_tr_t5, expert_authority_asymmetry, theater_ratio, 5, 0.52).
narrative_ontology:measurement(eaa_tr_t10, expert_authority_asymmetry, theater_ratio, 10, 0.65).
narrative_ontology:measurement(eaa_tr_t15, expert_authority_asymmetry, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(eaa_be_t0, expert_authority_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eaa_be_t5, expert_authority_asymmetry, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eaa_be_t10, expert_authority_asymmetry, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(eaa_be_t15, expert_authority_asymmetry, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expert_authority_asymmetry, information_standard).
narrative_ontology:boltzmann_floor_override(expert_authority_asymmetry, 0.08).
narrative_ontology:affects_constraint(expert_authority_asymmetry, professional_licensure_rent_seeking).
narrative_ontology:affects_constraint(expert_authority_asymmetry, epistemic_closure_institutional).
narrative_ontology:affects_constraint(expert_authority_asymmetry, alternative_knowledge_suppression).

% DUAL FORMULATION NOTE:
% Expert authority asymmetry decomposes into three downstream constraints: professional licensure as rent-seeking mechanism, epistemic closure enabling institutional capture, and institutional suppression of alternative knowledge systems. Each has distinct epsilon values and structural dynamics. This story captures the aggregate constraint; downstream stories model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expert_authority_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
