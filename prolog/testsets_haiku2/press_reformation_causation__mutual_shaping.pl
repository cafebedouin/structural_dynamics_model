% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Press-Reformation Mutual Shaping (Bidirectional Causation Reading)
 *   domain: history/technology/religious
 *
 * SUMMARY:
 *   Between 1450 and 1550, the printing press and the Protestant Reformation
 *   shaped each other through mutual reinforcement. This is not the reading
 *   that the press 'caused' the Reformation (technological determinism) or
 *   that reformers merely 'used' an available tool (strategic deployment as
 *   if technology were neutral). Rather, the press created new possibilities
 *   for theological distribution — enabling structure, not determining
 *   content — and reformers' organized exploitation of those possibilities
 *   fed back into how printing technologies, markets, and distribution
 *   networks developed. Reformers chose which texts to print, in which
 *   languages, at what scale; those choices shaped whether printing scaled
 *   into religion or remained marginal. Printers responded to demand,
 *   developed technologies for vernacular fonts and rapid reproduction, and
 *   created business models that depended on reformer networks. The
 *   constraint is CLAIMED as scaffold (transitional, with sunset) because the
 *   mutual shaping ends once the Reformation becomes established — by 1550,
 *   printing has become institutionalized and the Reformation has won enough
 *   ground that the active coupling between technological innovation and
 *   reformist agency attenuates.
 *
 * KEY AGENTS:
 *   - Reformed theological movements (organizing, text-selection, distribution network building)
 *   - Printing industry operators (technology scaling, format/language choices, market strategy)
 *   - Catholic institutional authority (enforcement, attempted suppression, cost-bearer of lost monopoly)
 *   - Manuscript copyists (displaced by scaling, but some transition to printing roles)
 *   - Vernacular reading communities (new audience enabled by both press and reformer intent)
 *   - Historians of technology (analytical seat, measuring co-evolution against determinism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.42).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.38).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Press-Reformation Mutual Shaping (Bidirectional Causation Reading)").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history/technology/religious").

narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'fcd0f69a-1563-41b3-8c92-8e5aa430ed34').
narrative_ontology:cs_kernel_codification('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', distributed).
narrative_ontology:cs_authority_grounding('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', practice).
narrative_ontology:cs_interpretation_layer_present('fcd0f69a-1563-41b3-8c92-8e5aa430ed34').
narrative_ontology:cs_reading_relation('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', press_reformation_causation__technological_determinism, influences).
narrative_ontology:cs_reading_relation('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', foundational, bidirectional_causation_operative).
narrative_ontology:cs_axiom_status(bidirectional_causation_operative, holdable).
narrative_ontology:cs_axiom_grounding('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', bidirectional_causation_operative, empirically_contingent).
narrative_ontology:cs_axiom('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', foundational, technology_and_agency_mutually_constitutive).
narrative_ontology:cs_axiom_status(technology_and_agency_mutually_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', technology_and_agency_mutually_constitutive, deontological).
narrative_ontology:cs_reference_frame('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', pre_reformation_monopoly_state).
narrative_ontology:cs_drift_state('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', post_reformation_pluralism_state, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fcd0f69a-1563-41b3-8c92-8e5aa430ed34', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformed_theological_movements).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printing_industry_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_reading_communities).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_institutional_authority).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, manuscript_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the press's capacity to distribute vernacular scripture and theological arguments at scale. Prior to the press, theological innovation faced immediate suppression by the Catholic institutional monopoly. The press created a distribution channel their organizational energy could exploit — alternatives to hand-copying and oral dissemination became available. Their agency (selecting texts, framing arguments, building networks of readers) shaped which printing technologies and distribution strategies developed; they were not merely passive beneficiaries of inevitable technological advance.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformed_theological_movements, beneficiary,
    organized, generational, mobile, continental).

% Benefit from the demand reformers created for printed religious texts, vernacular Bibles, and polemical tracts. Without that demand, printing presses would have remained marginal to manuscript production. Operators shaped the technologies (page formats, typeface choices, print run sizes) in response to what religious movements actually wanted to distribute. They made strategic choices about which texts to print, which languages to develop fonts for, and how to manage supply chains — choices that reflected both market demand and their own institutional interests.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printing_industry_operators, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, printing_industry_operators, agenda_setter).

% Bear the cost of lost monopoly over theological authority. The press made their exclusive control over scripture interpretation untenable — not inevitably (they could have adopted printing themselves earlier), but because the technology's affordances aligned with reformer priorities in ways institutional authority had not anticipated or invested in. They attempted suppression (the Index of Prohibited Books, censorship infrastructure) which increased the enforcement cost of maintaining theological hegemony.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_institutional_authority, payer,
    institutional, generational, constrained, continental).

% Face displacement as printing scaled. Their craft knowledge becomes less valuable when mechanical reproduction reduces the labor input per copy. Some adapt to printing-shop work; others exit the market. The transition is driven by both technological capability and by the demand reformers generate — no reformer demand for mass-produced theological texts, no printing scale, no displacement pressure.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, manuscript_copyists, payer,
    moderate, biographical, constrained, local).

% Gain access to scripture and theological argument in native languages rather than Latin clergy monopoly. The press makes this possible; the reformers make it intentional. Without reformer agency selecting which texts to print in vernacular, the press could have served scholarship (Greek and Latin editions) or commerce (ledgers, contracts) instead. Without the press, reformer intent to distribute vernacular scripture would have been constrained to hand-copying networks.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_reading_communities, beneficiary,
    powerless, generational, mobile, continental).

% Different reformation movements (Lutheran, Reformed, Anabaptist, etc.) competed for printing capacity and reader attention. The press enabled pluralism but did not determine which movement would dominate — that outcome depended on competing organized agency and social uptake. Each movement shaped printing priorities differently; the technology was enabling structure, not determining force.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, competing_reformation_branches, excluded,
    organized, generational, mobile, continental).

% Examine whether the press 'caused' the Reformation or the Reformation 'used' the press. This reading instantiates the co-evolution framing: neither side of that binary is structurally accurate. The press and reformation movements shaped each other; the explanatory unit is the coupled system, not either component alone.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historians_of_technology, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables distributed, parallel theological innovation and scriptural debate by making text reproducible at scale and distributable across language communities. Prior coordination was either centralized (Catholic hierarchy) or small-scale (manuscript networks). The press creates a commons of readable copies that multiple reformers can reference, cite, and build upon simultaneously.
% TRANSFER_FUNCTION: Transfers theological authority from the Catholic institutional monopoly to distributed communities of readers and printing operators. Reformers gain the capacity to reach mass audiences; printing operators gain markets; Catholic authority loses exclusivity. No money necessarily transfers — the fundamental transfer is of capacity to author, copy, and distribute meaning.
% ABSENT_VOICES: Illiterate populations: even vernacular printing presumes literacy. Reformers in regions without printing infrastructure (Eastern Europe, much of the Ottoman sphere during the early period). Printing operators in regions where Catholic political authority banned or heavily taxed the press. These absences shape the Reformation's geography and pace.
% DISAPPEARANCE_RATIONALE: If the printing press had never been invented, theological reform would likely have proceeded more slowly and with different geographic spread — hand-copying networks could not sustain the information density the Reformation achieved. If the Reformation had never occurred, the press would have developed for commerce and scholarship, not primarily religious distribution — market demand shapes which technologies get scaled. The reading contests determinism: vanishing either component changes the outcome, but neither component unilaterally determines the other.
% FOUNDING_PROBLEM: Catholic institutional authority faces theological challenge from within Christian tradition (indulgence theology, scriptural interpretation, clerical celibacy); reformers lack scalable means to distribute alternative arguments widely; printing technology exists but is marginal to established economic and information systems.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation (Eisenstein, Pettegree, Lindberg, Oberman) document that reformers actively chose to print, made deliberate decisions about which texts to prioritize, and competed for printing capacity. Historians of printing technology (Febvre and Martin, Chartier) document that printers made business decisions responding to market demand and shaped typeface/format choices in response to reader needs. The founding problem is attested across multiple scholarly traditions outside any single reformer or printer advocacy seat.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.42-0.48 at the peak (1510-1530), then slightly declines to 0.42 by 1550. This pattern reflects the constraint as a transitional enabling structure: early on, printing is still marginal and extraction minimal (Catholic authority faces cost only if it tries suppression). As reformers scale their use of printing, extraction increases because Catholic authority must invest heavily in censorship and enforcement to maintain theological monopoly. By 1550, extraction stabilizes and begins to decline because the Reformation is sufficiently established that the need for constant technological innovation in printing for religious distribution decreases — the constraint's function transitions from enabling innovation to maintaining an already-established system. Theater ratio (0.08 to 0.25 peak, back to 0.22) reflects the balance between functional innovation (real technological adaptation to reformer needs) and performative activity (enforcement and counter-enforcement theater). Suppression requirement tracks similarly: starts low because early printing is not yet a direct threat, rises as reformers scale adoption, declines as institutional adjustment occurs. The shared measurement grid ensures every metric is authored at every time point, enabling temporal analysis of the coupling dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The reformer and printing operator seats see the constraint as mutual enablement — technology made what they wanted to do possible, and their demand shaped what technology developed. The Catholic institutional seat sees the same constraint as enforced loss of authority — the press undermined their monopoly through an unstoppable technological-social coupling they could not control. The engine computes these divergent seat types from the structural data: reformers (beneficiaries, organized, mobile, continental scope) compute as rope-adjacent; Catholic authority (payer, institutional, constrained, continental scope) computes as snare-adjacent. Neither is wrong; the seat divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers benefit from the enabling structure but are not merely passive — their agency determines whether the press is used for theology or commerce. They sit near 0.3-0.4 on the directionality scale: beneficiaries of an enabling structure, but bearing the cost of active organizing and risk of suppression. Printing operators benefit from market demand but are constrained by their dependence on capital and geographic safety — they sit near 0.35-0.45: moderate beneficiaries with constrained exits. Catholic institutional authority is the target, losing exclusive control over theological interpretation — they sit near 0.65-0.75: bearing substantial extraction cost (lost authority monopoly, enforcement costs). The divergence across seats is structural: from the reformer seat, this is enabling coordination; from the Catholic seat, it is extractive institutional capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophy-resolved. The founding problem (theological pluralism enabled by technological distribution and constrained by Catholic monopoly) remains live through 1550 and beyond — the Reformation does not abolish theological debate, and printing does not resolve it. What CHANGES is the mode of the constraint: it transitions from 'enabling structure that actively shapes technology' to 'institutionalized technology supporting established institutional actors.' That transition is captured by the sunset clause (the constraint's justification IS the transition, not the steady state), not by mandatrophy. If the problem had been 'how to distribute copies,' and copies became easy to distribute such that the printing press was no longer needed, that would be mandatrophy. Instead, the problem ('how does theological pluralism exist within Christendom') persists, and printing becomes the normal technology for addressing it — the constraint's function atrophies as enabling innovation and stabilizes as maintenance of an established system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causation_directionality_ambiguity,
    'What is the correct causal direction: did the press enable the Reformation, or did the Reformation create demand that scaled the press?',
    'Comparative historical analysis of regions where printing existed but reformation movements were weak, and regions where reformation movements emerged before printing infrastructure. Also: counterfactual analysis of what printing would have developed into absent reformer demand (manuscript scholarship, commerce, administrative copying).',
    'If the press was purely enabling (no directionality to causation), the constraint is closer to rope/coordination. If reformer demand was primary and press was tool, the constraint is closer to snare (reformers extracting capacity from technology). If mutual reinforcement is demonstrated, the reading as scaffold is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_directionality_ambiguity, empirical, 'Whether causation runs one direction or is genuinely bidirectional.').

omega_variable(
    technological_determinism_vs_social_agency,
    'Could the same printing technology have developed without the Reformation, and could the Reformation have proceeded without printing?',
    'Historical counterfactuals (Eisenstein, McHale): examine printing trajectories in non-Christian societies (Islamic, Chinese, Jewish) absent Reformation demand. Examine reformation movements that succeeded despite printing scarcity (Eastern European reform, some Anabaptist networks in pre-printing regions).',
    'If printing proceeds without reformation demand, then technology is more autonomous than this reading claims — shifts toward technological determinism reading. If reformation movements require printing to succeed at scale, then society shapes technology more than technology shapes society — shifts toward strategic deployment reading. Mutual shaping asserts both succeed but differently and at smaller scale alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_vs_social_agency, empirical, 'Whether press and reformation are truly coupled or independently capable.').

omega_variable(
    co_evolution_vs_contingency,
    'Is the mutual shaping of press and reformation a structural necessity (co-evolution) or a historical contingency that could have gone differently?',
    'Trace the decision points where reformers chose to print (or not) and where printers chose religious texts as a market. Could reformers have succeeded with hand-copying? Could printers have thrived on law books and commerce? What made religious printing the dominant market?',
    'If mutual shaping is structural and necessary, the constraint is a genuine scaffold (enabling structure without which the outcome is impossible). If contingent (could have organized differently), then the constraint is weaker — more rope-like (coordination) or more snare-like (opportunistic capture).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(co_evolution_vs_contingency, conceptual, 'Whether co-evolution is structural or contingent on historical circumstances.').

omega_variable(
    kernel_reading_uncertainty,
    'Is this reading (mutual shaping, bidirectional causation) the most accurate account, or does one of the sibling readings (technological determinism or strategic deployment) better explain the historical record?',
    'Historiographical consensus (Eisenstein, Pettegree, Lindberg, Chartier, Febvre & Martin) and close reading of evidence about what reformers and printers explicitly chose vs. what was forced by technological capacity. Examine whether the press made censorship ''impossible'' (determinism) or merely difficult (mutual shaping), and whether reformers treated printing as neutral tool (strategic) or actively shaped its development (mutual).',
    'This reading instantiates co-evolution; if the evidence better supports determinism or strategic deployment, the reading is inaccurate and should be reclassified or replaced. Different readings produce different constraint types (determinism → mountain, strategic → rope, mutual → scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_uncertainty, empirical, 'Whether the mutual-shaping reading is correct relative to alternative causal framings of press-reformation relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.08).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.12).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causation__mutual_shaping, theater_ratio, 1510, 0.18).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.25).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__mutual_shaping, theater_ratio, 1550, 0.22).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.28).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causation__mutual_shaping, base_extractiveness, 1510, 0.42).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.48).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__mutual_shaping, base_extractiveness, 1550, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.22).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__mutual_shaping, suppression_requirement, 1480, 0.31).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causation__mutual_shaping, suppression_requirement, 1510, 0.42).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.48).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__mutual_shaping, suppression_requirement, 1550, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.12).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% The 'press_reformation_causation' kernel decomposes into three structurally distinct constraint readings: (1) MUTUAL_SHAPING (this story) — bidirectional causation, both components shape each other, scaffold type; (2) TECHNOLOGICAL_DETERMINISM — press causes Reformation by making censorship impossible, mountain type; (3) STRATEGIC_DEPLOYMENT — reformers strategically exploit neutral technology as a tool, rope type. Each reading has different ε (extractiveness), different beneficiary/victim structures, and different type classification. The three are linked via network.affects_constraints to show constraint family kinship: mutual-shaping is upstream to both determinism and strategic-deployment because it reframes the causal question itself, potentially foreclosing or influencing how the other readings are applied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
