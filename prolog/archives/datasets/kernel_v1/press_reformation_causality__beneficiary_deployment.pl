% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__beneficiary_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__beneficiary_deployment, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__beneficiary_deployment
 *   human_readable: Press-Reformation Causality: Beneficiary Deployment Reading
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the BENEFICIARY-DEPLOYMENT reading of
 *   the press-reformation causality kernel. The question is not whether the
 *   printing press and Protestant Reformation are historically connected —
 *   they clearly are — but WHY and HOW that connection was causal. The
 *   beneficiary-deployment reading claims: reformers and printing merchants
 *   had concrete structural stakes in breaking the Latin ecclesiastical
 *   monopoly on textual authority and reproduction. They did not discover
 *   printing technology and realize its revolutionary potential; they
 *   strategically deployed an available technology to solve a coordination
 *   problem (how to distribute reformed doctrine at scale, how to bypass
 *   Rome's gatekeeping). This reading contrasts with the
 *   technological-inevitability reading (printing technology naturally
 *   disrupts monopolistic control and necessarily produces decentralization)
 *   and the precondition-convergence reading (press and reformation were
 *   independent causal chains that intersected). The beneficiary-deployment
 *   reading frames the press-reformation connection as a TANGLED ROPE
 *   constraint: genuine coordination function (reformers and printers solve
 *   the distribution problem together), embedded extraction (reformers
 *   extract from the ecclesiastical monopoly's loss of textual control;
 *   printers extract profit from reformation demand; both extract from
 *   scribal manuscript producers' craft obsolescence). The constraint
 *   exhibits measurable extraction accumulation over time
 *   (base_extractiveness rising from 0.12 to 0.42 over 45 years) reflecting
 *   the acceleration of reformist consolidation and print market dominance.
 *   Theater ratio remains low throughout (0.15-0.38), indicating that the
 *   coordination function is genuine rather than performative — the press
 *   delivers actual decentralization, not theatrical simulation of it.
 *
 * KEY AGENTS:
 *   - Protestant Reformers (Martin Luther, Huldrych Zwingli, John Calvin): Primary beneficiaries (organized/arbitrage) — deploy press to distribute reformed doctrine at continental scale; capture authority from Rome through textual control.
 *   - Printing Merchants (Johann Gutenberg's successors, Froben, Aldus Manutius, Basel & Strasbourg printers): Primary beneficiaries (organized/arbitrage) — profit from reformation demand; establish commercial printing infrastructure; extract from manuscript obsolescence.
 *   - Latin Ecclesiastical Monopoly (Roman Catholic Church, papal authority, Latin scholastic tradition): Primary victim (institutional/constrained) — loses monopoly on textual reproduction and doctrinal gatekeeping; forced into adaptation or suppression (both costly).
 *   - Scribal Manuscript Producers (monastery scriptoria, independent scribes, guild apprentices): Secondary victims (moderate/constrained) — face craft obsolescence, wage collapse, apprenticeship devaluation; some transition to printing, others exit the trade.
 *   - Secular Princes & Merchants (Charles V, Henry VIII, merchant republics): Secondary beneficiaries (powerful/mobile) — gain strategic advantage from decentralized religious authority (reduces papal leverage), expand book trade commerce, strengthen state control of domestic religious narrative.
 *   - Established Church Hierarchy (bishops, cathedral chapters, monastic orders): Tertiary victims/secondary beneficiaries (institutional/constrained) — lose centralized authority over interpretation but benefit from reformed organizational efficiency and expanded pastoral reach; experience the constraint as forced adaptation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__beneficiary_deployment, 0.38).
domain_priors:suppression_score(press_reformation_causality__beneficiary_deployment, 0.42).
domain_priors:theater_ratio(press_reformation_causality__beneficiary_deployment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__beneficiary_deployment, extractiveness, 0.38).
narrative_ontology:constraint_metric(press_reformation_causality__beneficiary_deployment, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__beneficiary_deployment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__beneficiary_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__beneficiary_deployment, "Press-Reformation Causality: Beneficiary Deployment Reading").
narrative_ontology:topic_domain(press_reformation_causality__beneficiary_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__beneficiary_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__beneficiary_deployment, 'f8da8607-5e45-465c-8bdb-37fe798bc36e').
narrative_ontology:cs_kernel_codification('f8da8607-5e45-465c-8bdb-37fe798bc36e', distributed).
narrative_ontology:cs_authority_grounding('f8da8607-5e45-465c-8bdb-37fe798bc36e', distributed).
narrative_ontology:cs_reading_relation('f8da8607-5e45-465c-8bdb-37fe798bc36e', press_reformation_causality__technological_inevitability, influences).
narrative_ontology:cs_reading_relation('f8da8607-5e45-465c-8bdb-37fe798bc36e', press_reformation_causality__precondition_convergence, coexists_with).
narrative_ontology:cs_axiom('f8da8607-5e45-465c-8bdb-37fe798bc36e', foundational, beneficiary_agency_primacy).
narrative_ontology:cs_axiom_status(beneficiary_agency_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f8da8607-5e45-465c-8bdb-37fe798bc36e', beneficiary_agency_primacy, empirically_contingent).
narrative_ontology:cs_axiom('f8da8607-5e45-465c-8bdb-37fe798bc36e', foundational, extraction_coordination_hybrid).
narrative_ontology:cs_axiom_status(extraction_coordination_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('f8da8607-5e45-465c-8bdb-37fe798bc36e', extraction_coordination_hybrid, empirically_contingent).
narrative_ontology:cs_reference_frame('f8da8607-5e45-465c-8bdb-37fe798bc36e', monopoly_gatekeeping_equilibrium).
narrative_ontology:cs_drift_state('f8da8607-5e45-465c-8bdb-37fe798bc36e', post_reformation_consolidation, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('f8da8607-5e45-465c-8bdb-37fe798bc36e', '2025-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(press_reformation_causality__beneficiary_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__beneficiary_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__beneficiary_deployment, printing_merchants).
narrative_ontology:constraint_beneficiary(press_reformation_causality__beneficiary_deployment, vernacular_literacy_advocates).
narrative_ontology:constraint_victim(press_reformation_causality__beneficiary_deployment, latin_ecclesiastical_monopoly).
narrative_ontology:constraint_victim(press_reformation_causality__beneficiary_deployment, scribal_manuscript_production).
narrative_ontology:constraint_victim(press_reformation_causality__beneficiary_deployment, hierarchical_church_authority_distribution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATIN ECCLESIASTICAL MONOPOLY (SNARE) — Trapped by structural dependency on manuscript scarcity and clerical gatekeeping of text reproduction. Press technology creates asymmetric extraction: reformers and printers capture the coordination benefit (decentralized text distribution) while the monopoly bears full suppressive cost (loss of epistemic control, fragmentation of doctrine). No exit option for the institution — the technology makes their control mechanism obsolete. Maximum experienced extraction.
constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFORMERS & PRINTERS COALITION (ROPE) — Organized actors with direct structural stake in decentralizing religious text. Press technology is the coordination mechanism that solves their collective action problem: how to bypass Rome's monopoly on canonical text distribution and authority adjudication. Beneficiary perspective: the constraint appears as pure coordination enabling their shared goal (vernacular scripture access, doctrinal independence). Effective extraction is present but experienced as legitimate — they are breaking an unjust monopoly, not extracting without justification. Arbitrage exit means they could abandon the press and return to manuscript economy, but the press solution is superior to their alternatives.
constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: SCRIBAL MANUSCRIPT PRODUCERS (TANGLED ROPE) — Moderately powerful craft guild with high barriers to exit (specialized training, established contracts with ecclesiastical patrons). Press technology creates mixed dynamics: genuine coordination function (manuscript producers begin transition to print, participate in early publishing), but embedded extraction (sudden collapse of manuscript scarcity premium, apprenticeship obsolescence, wage pressure). Some scripts and illumination remain valuable for high-status manuscripts; mass-market text production shifts to printing. Constrained exit — abandoning the trade carries severe apprenticeship sunk costs and craft identity loss, but adaptation is possible at significant retraining cost.
constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED CHURCH HIERARCHY (TANGLED ROPE) — Institutional actor facing coordinated extraction from reformers and printers. The press enables genuine coordination function (faster text distribution increases pastoral reach, improves doctrine transmission to clergy), but the same mechanism enables extraction: loss of monopoly on textual authority, fragmentation of doctrine interpretation, challenge to hierarchical gatekeeping. Constrained exit — the church cannot abandon the technology without ceding all authority to reformers; must adapt ecclesiastical structure to manage decentralized text availability. High enforcement costs to suppress printing without complete institutional collapse.
constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: SECULAR PRINCES & MERCHANTS (ROPE) — Powerful actors with strategic interest in reducing ecclesiastical authority and enabling print commerce. Press technology serves their coordination: consolidate state authority by controlling domestic religious narratives, expand book trade as economic asset, enable literacy-based bureaucracy. Beneficiaries with mobile exit options — they could suppress printing (and some initially did) but found economic advantage in permitting it. Experience the constraint as coordination enabling their own state-building and mercantile interests.
constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: TECHNOLOGICAL INEVITABILITY (FALSE SUMMIT MOUNTAIN) — Civilizational analytical view that treats the press-reformation connection as a natural law: printing technology inevitably disrupts monopolistic text control; decentralization follows technologically from the mechanics of reproducibility. This framing naturalizes what is actually a contingent beneficiary coordination. The press did NOT inevitably cause reformation — it enabled it only because reformers and printers had structural stakes in deploying it against the monopoly. Italian Renaissance presses did not produce reformation; the same technology in the hands of different agents (purely commercial, without reformist stakes) would have produced book commerce, not doctrinal revolution. The mountain perspective obscures beneficiary agency by attributing causality to the technology itself rather than to the coalition that weaponized it.
constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__beneficiary_deployment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(press_reformation_causality__beneficiary_deployment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(press_reformation_causality__beneficiary_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The reformer-printer coalition extracts substantial value from the ecclesiastical monopoly: reduction of Rome's epistemic authority, circumvention of papal gatekeeping, capture of doctrinal interpretation by reformist groups. However, extractiveness is not maximum (>0.60 snare level) because the coordination function is genuine and partly orthogonal to extraction — the press actually solves distribution problems that are costly for everyone. The coalition is not pure parasites; they are providing a service (accessible vernacular scripture) that many communities legitimately demand. The extraction is economic (reformers capture status and power; printers capture profit) and political (decentralization of authority), not purely coercive. Suppression (0.42): Moderate. Barriers to exit from the ecclesiastical monopoly are institutional (Rome's hierarchical structure makes adaptation slow and painful) and economic (investment in manuscript infrastructure becomes worthless), but not total physical coercion. The church retains capacity for Counter-Reformation, can suppress printing in some regions (Spain, parts of Italy), can adapt to print-distributed doctrine (Council of Trent doctrinal standardization). The suppression mechanisms are enforcement (ecclesiastical suppression of heretical printing, Index of Prohibited Books) and institutional inertia (adapting centuries-old manuscript-based knowledge transmission to print distribution). Theater ratio (0.35, rising): Low-to-moderate. The coordination function of the press is substantially genuine — it actually distributes texts efficiently, makes scripture accessible to non-Latin readers, enables doctrinal debate across geographic distance. The theatrical component is moderate: reformers use printed texts not just to distribute information but to perform doctrinal authority (printing legitimizes claims through reproducible proof), and printers use printing as status marker (association with reformation, learning, modernity). Theater rises over the interval (0.15→0.38) as printing becomes culturally normalized — the revolutionary shock of text reproduction fades, and printing becomes an expected technology rather than a novelty, allowing performative use (printed books as status objects, print runs as markers of legitimacy) to grow.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The beneficiary coalition (reformers, printers, secular princes) perceive the press as enabling their goals — rope perspective dominates their indexical classification. The victims (ecclesiastical monopoly, scribal producers) perceive it as extractive constraint with high suppression and no exit. The analytical observer must choose between mountain-level technological determinism (hiding beneficiary agency) and tangled-rope recognition (revealing the coalition's strategic deployment). The gap reveals that the press-reformation connection is NOT a natural law but a contingent historical outcome dependent on specific agents' interests and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Reformers and printers are beneficiaries with arbitrage exit options (could stay in manuscript distribution but choose press as superior alternative) — this produces low d (~0.15), meaning f(d) approaches zero or negative, and they experience effective extraction as negative (the constraint subsidizes them). The ecclesiastical monopoly is a victim with constrained exit (cannot abandon institutional structure without collapse) — this produces high d (~0.85), meaning f(d) approaches maximum, and they experience effective extraction as very high. Scribal producers are moderately powerful victims with constrained exit — this produces moderate-high d (~0.70), intermediate experience. Secular princes are powerful beneficiaries with mobile exit — this produces very low d (~0.10), and they experience the constraint as highly subsidizing their interests. The analytical observer is analytical position with analytical exit — canonical d (~0.72) applies, producing moderate f(d). The perspectival gap in chi values reflects these directionality differences: beneficiaries' χ ≤ 0.0, victims' χ ≥ 0.50, observers' χ ≈ 0.55. The range of chi across perspectives (negative to high positive) is diagnostic of a tangled rope with genuine beneficiary coordination and asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that beneficiary-deployment reading is not explaining the same causal object as technological-inevitability reading — they are two different claims about the press-reformation nexus. The beneficiary-deployment reading focuses on AGENT AGENCY in deploying technology strategically. The technological-inevitability reading focuses on STRUCTURAL INEVITABILITY of how print disrupts monopolies. Both can be true: the press structure makes monopoly disruption possible (technological inevitability is a precondition), but only beneficiary deployment makes it actual (reformers and printers must choose to use it). The mandatrophy is resolved by recognizing the asymmetry: without beneficiary agency, the press alone produces book commerce and literacy expansion (technological contingency), but not reformation (ideological outcome). Without the press, reformation could theoretically occur through alternative text distribution (manuscript networks, oral preaching), but would be geographically and temporally limited. The two readings are not in contradiction — they describe different causal layers (structure vs. agency) that together account for the historical outcome. The analytical observer must recognize both contributions: the press made decentralization technologically possible (mountain-level inevitability of certain textual effects), but reformer-printer agency made it ideologically and institutionally actual (tangled-rope coordination and extraction). Mandatrophy dissolves when we recognize this complementarity rather than seeking a single causal explanation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_direction_attribution,
    'Did printing technology cause reformation, or did reformation cause the strategic deployment of printing technology?',
    'Historical sequence analysis: documented reformist intent prior to large-scale press adoption (Luther''s 95 Theses predate systematic printing strategy by 15+ years); examination of print adoption patterns in pre-reformist contexts (Renaissance Italy, commercial Antwerp); comparison of reformation intensity in regions with/without established printing industries at time of doctrinal challenge.',
    'If technology-caused: constraint is mountain (press naturally disrupts monopolies). If beneficiary-deployed: constraint is tangled rope (reformers + printers extract from monopoly while coordinating doctrinal revolution). If bidirectional feedback: constraint remains tangled rope but mandatrophy analysis shifts to emphasize coordination feedback loop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_direction_attribution, empirical, 'Causal direction between printing technology and reformation movement').

omega_variable(
    alternative_reformist_technologies,
    'What alternative technologies or distribution mechanisms could have achieved similar doctrinal decentralization without printing press (e.g., manuscript networks, oral preaching, underground copying)?',
    'Historical counterfactual analysis: examine reformation attempts in pre-press regions; study alternative text-distribution networks in late medieval monasteries and universities; assess transmission speed and scale reach of non-press reformist movements (Lollardy, Waldensian, early Hussite networks before print adoption).',
    'If viable alternatives existed: press was contingent tool, not technological necessity. Strengthens beneficiary-deployment reading — reformers chose press among available options. If no viable alternatives: press represents unique technological enablement, supporting technological inevitability reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reformist_technologies, empirical, 'Whether alternative technologies could have achieved doctrinal decentralization').

omega_variable(
    printer_reformist_alignment_authenticity,
    'Were printers genuine believers in Protestant doctrine, or opportunistic merchants weaponizing doctrinal conflict for profit?',
    'Biographical analysis of major printing merchants (Froben, Gutenberg''s immediate successors, Aldus); examination of printing investment patterns (did they fund technical improvements that lowered religious text costs, or only profitable secular texts?); analysis of printing location patterns (correlation between reformist centers and established printing industries).',
    'If genuine alignment: coalition is ideologically unified (beneficiary extraction is justified by shared religious conviction). If opportunistic: extraction is more purely economic (merchants extract profit from reformists'' demand; constitutes separate tangled-rope constraint). If mixed: most historically plausible — coalition is held together by both belief alignment and profit incentive, making coordination both genuine and extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(printer_reformist_alignment_authenticity, empirical, 'Authenticity of printer-reformer doctrinal alignment versus opportunism').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of a single kernel (press-reformation causality) or does it represent a fundamentally different constraint than the technological_inevitability reading?',
    'Committer-frame analysis: Does the beneficiary-deployment reading FORECLOSE technological inevitability (logically incompatible), COEXIST WITH it (both defensible in different frameworks), or INFLUENCE it (changes conditions without ruling it out)? The answer depends on whether ''beneficiary deployment'' and ''technological inevitability'' describe the same causal object or different ones.',
    'If FORECLOSES: only one reading can be correct; the kernel adjudicates between them. If COEXISTS: both readings are live; reformation outcomes compatible with both causal framings. If INFLUENCES: beneficiary deployment creates conditions that technological inevitability scholars mistake for determinism. Current assessment: COEXISTS WITH (different parties hold different readings; neither logically eliminates the other).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Logical relationship between beneficiary-deployment and technological-inevitability readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__beneficiary_deployment, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_tr_t0, press_reformation_causality__beneficiary_deployment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(press_ref_tr_t15, press_reformation_causality__beneficiary_deployment, theater_ratio, 15, 0.22).
narrative_ontology:measurement(press_ref_tr_t30, press_reformation_causality__beneficiary_deployment, theater_ratio, 30, 0.35).
narrative_ontology:measurement(press_ref_tr_t45, press_reformation_causality__beneficiary_deployment, theater_ratio, 45, 0.38).

% Extraction over time
narrative_ontology:measurement(press_ref_be_t0, press_reformation_causality__beneficiary_deployment, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(press_ref_be_t15, press_reformation_causality__beneficiary_deployment, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(press_ref_be_t30, press_reformation_causality__beneficiary_deployment, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(press_ref_be_t45, press_reformation_causality__beneficiary_deployment, base_extractiveness, 45, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(press_ref_su_t0, press_reformation_causality__beneficiary_deployment, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(press_ref_su_t15, press_reformation_causality__beneficiary_deployment, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(press_ref_su_t30, press_reformation_causality__beneficiary_deployment, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(press_ref_su_t45, press_reformation_causality__beneficiary_deployment, suppression_requirement, 45, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__beneficiary_deployment, resource_allocation).
narrative_ontology:affects_constraint(press_reformation_causality__beneficiary_deployment, press_reformation_causality__technological_inevitability).
narrative_ontology:affects_constraint(press_reformation_causality__beneficiary_deployment, press_reformation_causality__precondition_convergence).
narrative_ontology:affects_constraint(press_reformation_causality__beneficiary_deployment, vernacular_literacy_decentralization).
narrative_ontology:affects_constraint(press_reformation_causality__beneficiary_deployment, ecclesiastical_monopoly_dissolution).

% DUAL FORMULATION NOTE:
% The press-reformation nexus decomposes into three constraint stories per the ε-invariance principle: (1) beneficiary-deployment (this story, ε=0.38, tangled rope) focuses on agent agency in strategic deployment; (2) technological-inevitability (ε=0.08, mountain candidate) focuses on structural inevitability of textual disruption; (3) precondition-convergence (ε=0.30, tangled rope) focuses on independent causal chains intersecting. Each story instantiates a different claim about the same historical phenomenon. The ε values differ because they measure different observables: base_extractiveness from beneficiary stakes, structural inevitability of printing's effects, and contingency of causal convergence. All three stories linked per network.affects_constraints to represent the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__beneficiary_deployment, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
