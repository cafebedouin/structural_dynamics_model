% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition as Strategic Authority-Bypass Mechanism
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the beneficiary_agency_reading of the
 *   technology_reformation_causality kernel: the printing press is read as a
 *   tool deployed strategically by reformers and commercial printers to route
 *   around Church licensing and censorship, not as a technology whose mere
 *   existence made the Reformation's scale and speed inevitable. Under this
 *   reading, the coalition of reform clergy, printing houses, and
 *   reform-sympathetic princes forms a tangled_rope — a genuine, novel
 *   coordination structure (rapid vernacular distribution) that
 *   simultaneously extracts authority, revenue, and interpretive control from
 *   the Roman curia and from parish clergy who had no comparable press
 *   access. The press itself, on this reading, functions as a scaffold: a
 *   transitional means whose justification lies in the transition it enabled
 *   (bypassing a specific censorship apparatus), not in a claimed permanent
 *   or natural function. Sibling readings — technological_determinism_reading
 *   (press-as-cause) and co_constitution_reading (mutual shaping) — are NOT
 *   part of this file; they carry their own ε and their own stakeholder sets
 *   and are linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.68).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.71).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition as Strategic Authority-Bypass Mechanism").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde').
narrative_ontology:cs_kernel_codification('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', distributed).
narrative_ontology:cs_authority_grounding('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', distributed).
narrative_ontology:cs_reading_relation('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', foundational, agency_precedes_technology_thesis).
narrative_ontology:cs_axiom_status(agency_precedes_technology_thesis, holdable).
narrative_ontology:cs_axiom_grounding('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', agency_precedes_technology_thesis, empirically_contingent).
narrative_ontology:cs_axiom('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', secondary, technology_as_selectable_instrument).
narrative_ontology:cs_axiom_status(technology_as_selectable_instrument, holdable).
narrative_ontology:cs_axiom_grounding('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', technology_as_selectable_instrument, conventional).
narrative_ontology:cs_reference_frame('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', episcopal_licensing_primacy).
narrative_ontology:cs_drift_state('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', post_reformation_confessionalization, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('bc7e317a-0fc4-4f7a-a9b1-934b5bae3cde', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printing_house_owners).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, territorial_princes_supporting_reform).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, roman_curia).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, local_parish_clergy_loyal_to_rome).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, lay_readers_exposed_to_polemical_distortion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, lay_readers_exposed_to_polemical_distortion).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, agency_precedes_technology_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selected which tracts to write, which printers to commission, and which vernacular translations to authorize, deliberately routing doctrine around episcopal licensing and university censorship. They chose the press as one tool among several (preaching, pamphleteering, diplomatic alliance) and could and did switch tactics when printing was suppressed in a given territory.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy, beneficiary).

% Commercial operators who found reform pamphlets more profitable and faster-turnover than devotional or scholastic texts under Church license; they moved workshops between cities and princely jurisdictions to follow favorable patronage and evade guild or episcopal printing controls, extracting steady revenue from the controversy itself.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, printing_house_owners, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, printing_house_owners, agenda_setter).

% Granted printing privileges and physical protection to reform presses within their territories, converting a doctrinal dispute into leverage against imperial and papal authority over taxation, appointment, and jurisdiction; their exit from Rome's authority was made cheaper by the coalition's propaganda output.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, territorial_princes_supporting_reform, beneficiary,
    institutional, generational, arbitrage, regional).

% Bore the cost of the bypass directly: loss of licensing control, loss of tithe and indulgence revenue in defecting territories, and loss of the monopoly on doctrinal interpretation it had held through manuscript and pulpit control. Its available responses (indices of prohibited books, counter-printing, excommunication) lagged the coalition's distribution speed and jurisdictional mobility.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, roman_curia, payer,
    institutional, civilizational, constrained, continental).

% Found their pulpits and manuscript authority undercut by pamphlets circulating faster than they could be answered or refuted locally; many had no printing access themselves and no territorial protector, leaving them to absorb reputational and material loss (lost tithes, lost congregational loyalty) with no comparable countermeasure.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, local_parish_clergy_loyal_to_rome, payer,
    powerless, biographical, trapped, local).

% Gained vernacular access to scripture and controversy but also received polemical caricature, forged attributions, and inflammatory woodcuts optimized for sale rather than accuracy; many had no means to verify claims against either side's primary sources and bore the social costs of confessional violence that followed.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, lay_readers_exposed_to_polemical_distortion, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, lay_readers_exposed_to_polemical_distortion, beneficiary).

% Catholic-aligned printers attempting counter-pamphleteering were structurally disadvantaged by slower ecclesiastical approval processes and less permissive princely patronage in reform-leaning territories; their objection — that the coalition weaponized a commercial medium under cover of doctrinal necessity — rarely reaches the standard historiographical account.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, rival_confessional_printers, excluded,
    moderate, biographical, constrained, regional).

% Analyze archival print runs, patronage records, and correspondence to assess whether printing volume tracked reformer strategy decisions (supporting agency) or reformer strategy tracked printing capacity growth (supporting determinism); their reconstructions depend heavily on which archives survived confessional conflict.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, printing_house_owners).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers and printers coordinated a genuinely novel distribution channel — pooling doctrinal content, commercial capital, and princely protection — to move religious argument outside the reach of episcopal licensing faster than any single party could have managed alone.
% TRANSFER_FUNCTION: Moves doctrinal authority, tithe revenue, congregational loyalty, and interpretive control away from the Roman curia and loyalist parish clergy toward reform clergy, printing houses, and reform-aligned princes; also moves confessional volatility and interpretive risk onto lay readers with no means to adjudicate competing claims.
% ABSENT_VOICES: Rival Catholic printers and loyalist parish clergy would object that the coalition's 'bypass' framing understates how much the press was actively weaponized rather than merely used, and that vernacular access came bundled with propaganda they had no comparable channel to counter; loyalist clergy in particular are largely absent from surviving printed archives because they did not control presses.
% DISAPPEARANCE_RATIONALE: If the printer-reformer coalition's strategic deployment vanished but the press itself remained a neutral technology, reform advocates argue doctrine would still have spread via preaching networks and manuscript circulation, only more slowly; determinist historians argue the specific speed and scale of the confessional rupture depended on this coalition's deliberate exploitation of print economics, and would not have occurred in the same form without it. The two camps do not converge on a single counterfactual.
% FOUNDING_PROBLEM: Reform clergy needed a distribution mechanism fast enough to outpace episcopal censorship and university theological review, which could take doctrine off the street before a rebuttal or ban was issued.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Catholic polemicists (e.g. Cochlaeus's complaints about pamphlet speed) attest from outside the reform coalition that the bypass function was real and effective at the time; modern archival economic historians studying print-shop ledgers corroborate that pamphlet volume tracked doctrinal urgency rather than pure market demand, supporting the founding problem's historical reality. The problem itself (out-running a single-diocese censorship apparatus) no longer describes any live constraint in a post-print, post-Reformation religious landscape.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.68 across the interval as the coalition's use of print shifted from occasional tactical pamphleteering (1517-1522) to a sustained, institutionalized distribution apparatus with dedicated reform presses, princely subsidy, and organized pamphlet economies (by 1555). Suppression tracks a parallel rise (0.40 to 0.71) as the Church's countermeasures (indices, licensing crackdowns, excommunication) intensified in response, and as reform territories hardened their own printing protections against Catholic counter-publication. Theater ratio rises moderately (0.20 to 0.42) reflecting the growing share of print output that was polemical performance (woodcuts, caricature, forged attribution) rather than substantive doctrinal argument — consistent with the lay-reader payer situation. All three series share the same time grid (1517, 1522, 1529, 1536, 1546, 1555).
 *
 * PERSPECTIVAL GAP:
 *   From the reform clergy and printer seats, the structure reads as legitimate coordination bypassing an illegitimate censorship monopoly. From the Roman curia and loyalist clergy seats, the identical structure reads as coordinated extraction of authority and revenue conducted under cover of a doctrinal dispute. The engine computes both per-seat readings from the same structural data; this divergence is exactly the seat-divergence the tangled_rope classification is designed to preserve rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform clergy and printing house owners are declared beneficiaries because the coalition structure was one they actively built, chose tactics within, and could exit (relocate presses, change territories, switch tactics) when a given avenue was blocked — this places them near the beneficiary end of directionality. The Roman curia and loyalist parish clergy are declared victims because the same structure extracted authority and revenue from them with no comparable countermeasure at matching speed; parish clergy in particular are trapped (no printing access, no territorial protector), placing them near the full-target end. Lay readers are dual-positioned: real beneficiaries of vernacular access, real payers of distortion costs they could not verify or resist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (out-running single-diocese censorship) is dead in the present — no live constraint requires bypassing episcopal print licensing today — but the coalition structure it produced (a durable print-based challenge to centralized doctrinal authority) persisted institutionally well past the original bypass need, hardening into permanent confessional print infrastructure by the 1550s. Classifying this as tangled_rope rather than pure snare or pure rope prevents two mislabelings: it would be wrong to call the entire arrangement extraction-only (the coordination function — rapid vernacular distribution — was real and is separately corroborated by Catholic contemporaries), and it would be wrong to call it pure coordination (the Roman curia and loyalist clergy bore concentrated, asymmetric, non-consensual costs through the identical mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_capacity_ordering,
    'Did reformer strategic decisions drive printing volume and distribution patterns, or did existing/growing printing capacity constrain and shape what strategies reformers could even consider — i.e., which is upstream, the choice or the infrastructure?',
    'Compare timing of strategic decisions (recorded in reformer correspondence and sermons) against independently-dated print-shop capacity expansion records; if strategic pivots consistently precede capacity availability, agency-first is supported; if capacity expansion consistently precedes and enables strategic pivots, determinism is supported.',
    'This is the located disagreement between this reading (beneficiary_agency_reading) and technological_determinism_reading. If capacity-first is empirically supported, the sibling reading''s classification (press as mountain-adjacent infrastructural cause) would be strengthened and this reading''s tangled_rope/scaffold framing would need revision toward treating the coalition as more constrained by technology than the reading currently allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_capacity_ordering, conceptual, 'Whether reformer agency or print capacity is the upstream causal variable — the core disagreement located between beneficiary_agency_reading and technological_determinism_reading.').

omega_variable(
    coalition_naturalness_vs_construction,
    'Is the reformer-printer-prince coalition better read as an emergent alliance of independently-motivated actors who happened to converge (closer to natural social dynamics), or as a deliberately constructed extraction apparatus that reformers and printers built and maintained for mutual benefit?',
    'Archival evidence of explicit contractual or patronage agreements between reform clergy and specific printing houses, versus evidence of parallel-but-uncoordinated individual decisions; correspondence showing deliberate coordination (e.g., commissioned translations, exclusive printing arrangements) versus opportunistic convergence.',
    'If the coalition is shown to be substantially deliberate and coordinated (contractual patronage, exclusive arrangements), the tangled_rope classification with requires_active_enforcement is strongly supported. If convergence is shown to be largely opportunistic and uncoordinated, the constraint may be closer to an emergent rope with incidental victims rather than a deliberately maintained extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_naturalness_vs_construction, empirical, 'Whether the coalition was deliberately constructed extraction infrastructure or emergent, uncoordinated convergence of independent actors.').

omega_variable(
    counterfactual_reformation_scale,
    'Under the beneficiary_agency_reading''s own premises, would a comparably-scaled confessional rupture have occurred via manuscript circulation and preaching networks alone, absent strategic print deployment, only more slowly — or would the absence of coordinated print strategy have kept the dispute within a containable, negotiable ecclesiastical framework?',
    'Comparative study of pre-print heretical/reform movements (Lollards, Hussites) that lacked equivalent print infrastructure, assessing whether their eventual containment or suppression correlates with absence of a comparable distribution coalition, controlling for other variables (political protection, timing, doctrinal content).',
    'If prior movements without print coalitions were reliably contained while this one was not, it strengthens the claim that the coalition''s deliberate exploitation of print (not press availability alone) was the decisive extraction/bypass mechanism, supporting this reading''s tangled_rope classification over the determinist alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_reformation_scale, empirical, 'Whether coalition-driven print strategy, rather than press availability, was decisive for the Reformation''s scale relative to earlier uncontained reform movements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1517, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(tech_tr_t1522, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1522, 0.28).
narrative_ontology:measurement(tech_tr_t1529, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1529, 0.34).
narrative_ontology:measurement(tech_tr_t1536, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1536, 0.38).
narrative_ontology:measurement(tech_tr_t1546, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1546, 0.4).
narrative_ontology:measurement(tech_tr_t1555, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1555, 0.42).

% Extraction over time
narrative_ontology:measurement(tech_be_t1517, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(tech_be_t1522, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1522, 0.48).
narrative_ontology:measurement(tech_be_t1529, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1529, 0.58).
narrative_ontology:measurement(tech_be_t1536, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1536, 0.63).
narrative_ontology:measurement(tech_be_t1546, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1546, 0.66).
narrative_ontology:measurement(tech_be_t1555, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1555, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1517, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(tech_su_t1522, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1522, 0.52).
narrative_ontology:measurement(tech_su_t1529, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1529, 0.6).
narrative_ontology:measurement(tech_su_t1536, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1536, 0.65).
narrative_ontology:measurement(tech_su_t1546, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1546, 0.69).
narrative_ontology:measurement(tech_su_t1555, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1555, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.12).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings reading the technology_reformation_causality kernel. technological_determinism_reading treats the press as a mountain-like infrastructural cause with negligible agent-side extraction (different ε, different stakeholder set keyed to inherent press properties). co_constitution_reading treats causality as genuinely mutual, likely landing as a rope or scaffold with more diffuse beneficiary structure and lower extractiveness than this reading's tangled_rope. All three share the historical interval and overlapping named agents (reform clergy, printers, curia) but assign different structural weight to agency versus technology, producing different ε values and different classifications — per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
