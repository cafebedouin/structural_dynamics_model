% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition as Strategic Authority-Bypass Alliance (Beneficiary-Agency Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   Between the Ninety-Five Theses (1517) and the Peace of Augsburg (1555),
 *   an alliance of reforming theologians and commercial printers built a
 *   cross-border production-and-distribution channel that moved religious
 *   argument faster than any authority could suppress it. This story
 *   instantiates the beneficiary_agency_reading of the
 *   technology_reformation_causality kernel: the press was a strategic
 *   instrument deployed by identifiable agents, not an autonomous cause. The
 *   colloquial label 'print caused the Reformation' conflates three
 *   structurally distinct claims, decomposed per the epsilon-invariance
 *   principle into three readings of one kernel plus a separate
 *   infrastructure story; this file carries only the agency reading, with
 *   epsilon attaching to the coalition's operation (risk asymmetry, privilege
 *   rents, authority transfer), never to the press as artifact. KEY AGENTS
 *   (by structural relationship): - magisterial_reformers: agenda-setting
 *   beneficiary (organized/identity_locked) — supplies doctrine, directs
 *   deployment, cannot exit without self-annihilation -
 *   major_protestant_printers: coordinating beneficiary
 *   (moderate/constrained) — owns the infrastructure, holds privileges, bears
 *   capital risk - catholic_church_hierarchy: primary external target
 *   (institutional/trapped) — the mediation monopoly being routed around -
 *   subordinate_printers_and_tract_authors: risk-bearing payer
 *   (powerless/trapped) — absorbs confiscation and execution risk, abandoned
 *   when convenient - imperial_authorities: enforcement payer
 *   (institutional/trapped) — funds suppression and fights the wars the
 *   pamphlet war inflames - protestant_territorial_rulers: mobile beneficiary
 *   (powerful/mobile) — collects autonomy, land, and leverage -
 *   vernacular_reading_public: dual beneficiary/payer (moderate/constrained)
 *   — gains access, absorbs polarization - illiterate_oral_communities:
 *   excluded voice (powerless/trapped) — rearranged by a medium they cannot
 *   enter - media_historians: analytical observer (analytical/analytical) —
 *   sees the full structure from outside confessional commitment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.66).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.6).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition as Strategic Authority-Bypass Alliance (Beneficiary-Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history of technology / religious history / media studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'd3b3029e-8a60-4c88-8920-3aeb0746f478').
narrative_ontology:cs_kernel_codification('d3b3029e-8a60-4c88-8920-3aeb0746f478', distributed).
narrative_ontology:cs_authority_grounding('d3b3029e-8a60-4c88-8920-3aeb0746f478', expertise).
narrative_ontology:cs_interpretation_layer_present('d3b3029e-8a60-4c88-8920-3aeb0746f478').
narrative_ontology:cs_reading_relation('d3b3029e-8a60-4c88-8920-3aeb0746f478', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('d3b3029e-8a60-4c88-8920-3aeb0746f478', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('d3b3029e-8a60-4c88-8920-3aeb0746f478', foundational, print_was_strategic_instrument_not_autonomous_cause).
narrative_ontology:cs_axiom_status(print_was_strategic_instrument_not_autonomous_cause, holdable).
narrative_ontology:cs_axiom_grounding('d3b3029e-8a60-4c88-8920-3aeb0746f478', print_was_strategic_instrument_not_autonomous_cause, empirically_contingent).
narrative_ontology:cs_axiom('d3b3029e-8a60-4c88-8920-3aeb0746f478', secondary, coalition_value_derives_from_authority_bypass).
narrative_ontology:cs_axiom_status(coalition_value_derives_from_authority_bypass, holdable).
narrative_ontology:cs_axiom_grounding('d3b3029e-8a60-4c88-8920-3aeb0746f478', coalition_value_derives_from_authority_bypass, instrumental).
narrative_ontology:cs_reference_frame('d3b3029e-8a60-4c88-8920-3aeb0746f478', agent_centered_strategic_deployment).
narrative_ontology:cs_drift_state('d3b3029e-8a60-4c88-8920-3aeb0746f478', contemporary_revisionist_debate, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('d3b3029e-8a60-4c88-8920-3aeb0746f478', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, magisterial_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, major_protestant_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, protestant_territorial_rulers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, subordinate_printers_and_tract_authors).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, imperial_authorities).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University theologians and preachers who supply the movement's doctrine, plan what gets published and when, grant favored shops exclusive rights to print authoritative editions, and decide which allies receive protection and which are publicly disowned when politics turn dangerous. Several live under imperial ban; leaving the movement would mean recanting the beliefs that constitute their public selves, so they do not leave.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, magisterial_reformers, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, magisterial_reformers, beneficiary).

% Workshop owners in Wittenberg, Strasbourg, and Frankfurt who sink capital into presses, type, paper, and stock, compete for reformist manuscripts, and hold exclusive printing privileges on flagship titles such as the Luther Bible. Pamphlet runs sell out in days; the same presses can be confiscated and their owners imprisoned when authorities catch up with a shipment. Leaving the trade means abandoning the fastest-growing market in European publishing along with the sunk equipment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, major_protestant_printers, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, major_protestant_printers, agenda_setter).

% The clergy and curia whose teaching office, indulgence traffic, and pulpit monopoly the coalition routes around. They answer with bans, prohibited-book lists, counter-pamphlets, and heresy proceedings, and they cannot stop responding without conceding the mediating authority that defines the office. Over the interval they pay in enforcement spending, institutional legitimacy, and finally the northern churches themselves.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Small jobbing printers, journeymen, and freelance pamphleteers who take the riskiest commissions: peasant-war manifestos, radical tracts, smuggling runs into hostile territory. When crackdowns come they face seizure of type and stock, prison, and occasionally execution, while the movement's leading voices denounce their causes and the privileged shops keep their monopolies intact. Flight means losing workshop, guild standing, and livelihood at once.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, subordinate_printers_and_tract_authors, payer,
    powerless, immediate, trapped, regional).

% The emperor's chancery, imperial diets, and municipal councils charged with keeping religious peace. They issue bans such as the Edict of Worms, fund searches and seizures, and ultimately fight the wars the pamphlet war inflames. Restoring uniformity proves impossible and abandoning the attempt would dissolve the empire's own legitimating idea, so they pay continuously in treasury and blood until the 1555 settlement concedes permanent division.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, imperial_authorities, payer,
    institutional, generational, trapped, continental).

% Electors, princes, and city councils that shelter the presses, endow the preachers, and collect the returns: church lands confiscated, appointments controlled, leverage over the emperor secured. Their protection is conditional and reversible — several hedge or switch confession when dynastic interest dictates — so the movement's security rides on their continuing calculation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, protestant_territorial_rulers, beneficiary,
    powerful, generational, mobile, continental).

% Urban artisans, clergy, students, and merchants who buy pamphlets and vernacular Bibles, gaining direct access to scripture and a share in argument previously reserved to latinate clergy. They also absorb slanders, confessional labeling, and the escalating conflict the pamphlet war feeds; once their ruler settles a confession, their local options narrow to conformity or exile.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public, payer).

% The rural and urban majority who meet the transformed religious world through mandated pulpits, posted edicts, visitations, and parish discipline rather than through reading. Decisions about what may be said, printed, and taught reorder their worship and allegiance, and they have no seat in the councils or the marketplace where those decisions are made.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, illiterate_oral_communities, excluded,
    powerless, biographical, trapped, continental).

% Archival scholars working centuries later from printers' ledgers, privilege grants, confiscation inventories, and correspondence. They reconstruct prices, run sizes, and risk distributions without confessional commitment, and their reconstructions are the seat from which this story's numbers are authored.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, magisterial_reformers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of producing and distributing heterodox religious content at scale across hostile jurisdictions faster than authorities can suppress it: reformers supply doctrinal content and moral legitimacy, printers supply capital, presses, and trade networks, and together they operate a channel neither party could build alone.
% TRANSFER_FUNCTION: Moves four things at once: money (pamphlet and Bible revenue from readers to printers), attention (a mass reading public assembled from scattered literate households), authority (teaching office from the hierarchy to reformer-led churches), and risk (enforcement exposure pushed down onto expendable printers and authors while privileged insiders enjoy protection).
% ABSENT_VOICES: The illiterate oral majority, women outside the print trades, and radical dissenters whose presses the coalition helped crush would object that the celebrated liberation of speech was a re-partitioning of who may speak, not its universalization. They are absent from the coalition's councils; their objection survives only in persecution records and later historiography.
% DISAPPEARANCE_RATIONALE: Without the coalition, reform proposals remain university disputations inside the incumbent's channels; the hierarchy retains its mediation monopoly for generations longer; vernacular scripture reaches readers only through licensed and slower paths; no pamphlet war, a different confessional map, and a delayed literacy norm. The information environment of Latin Christendom rearranges wholesale.
% FOUNDING_PROBLEM: How do you propagate a religious reform when the incumbent authority controls every sanctioned channel — pulpit, license, university, and border? The arrangement was built to circumvent centralized doctrinal gatekeeping.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Catholic controversialists (Johannes Eck, Johannes Cochlaeus) attested in real time that the movement's reach was a print-enabled bypass of lawful gatekeeping; imperial police and confiscation records attest the enforcement problem the arrangement posed; modern secular historiography (Pettegree, Edwards) attests both the founding problem and its death at the 1555 settlement from a non-confessional seat. No corroboration for the 'problem is dead' half comes from inside the coalition — its heirs commemorate the founding as eternal, which is itself signal.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.66 reflects three stacked flows: the authority value transferred away from the hierarchy, monopoly privilege rents inside the coalition (exclusive rights on flagship editions taxed the movement's own market), and risk dumped onto expendable partners. Suppression 0.60 is the enforcement machinery the arrangement required — ban-evasion logistics, privilege policing, and hardening internal orthodoxy discipline — not external coercion of the coalition, which appears instead in resistance 0.80 (bans, index lists, confiscations, two wars). Accessibility_collapse 0.55: manuscript and oral channels persisted throughout, but for mass-scale propagation across hostile jurisdictions the print channel left no workable alternative at the required speed. Theater_ratio 0.40 at interval end: function dominated early (real presses, real shipments), performance share grew as the founding urgency decayed into anniversary culture and martyrology. The temporal series run on one shared eleven-point grid (every tracked metric authored at every point, 1517-1555). The series oscillate rather than drift monotonically: the 1525 spike is crisis extraction concentration — when the Peasants' War turned, the core coalition publicly disowned radical allies and subordinate printers absorbed the crackdown alone, an intermittent-reinforcement pattern in which each crisis re-concentrates gains upward; the 1546-1550 bump is wartime requisition and Interim-era contraction concentrating benefits among survivors. Base_properties scalars are anchored to the 1555 endpoint of the same grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical structural data. From the reformer and privilege-holding printer seats, this is heroic coordination they built at personal risk. From the subordinate printer seat, it is risk extraction with abandonment clauses. From the imperial and hierarchical seats, it is subversion whose suppression cost more than concession. From the reading public's seat it is simultaneously gift (scripture access) and harm (confessional polarization, war). The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: magisterial_reformers, major_protestant_printers, and protestant_territorial_rulers sit near the beneficiary end (the arrangement subsidizes them; rulers' mobility and printers' privileges damp their effective load further). catholic_church_hierarchy, imperial_authorities, and subordinate_printers_and_tract_authors sit near the full-target end — trapped exit and identity or capital lock push them there. vernacular_reading_public sits near symmetric: genuine access benefit, diffuse indirect cost. Continental spatial scope matters mechanically: cross-border smuggling and jurisdictional arbitrage made verification of enforcement hard for targets, which scales effective extraction upward for the hierarchical and imperial seats. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already produce the correct per-seat positions, and the one dual-positioned agent (vernacular_reading_public) is correctly near-symmetric by derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical misreadings. Calling the coalition pure coordination (the heroic-liberation narrative) erases the measurable extraction: privilege rents taxing the movement's own market, risk asymmetry visible in prosecution records, and the 1525 abandonment. Calling it pure extraction (cynical-media narrative) erases the genuine coordination achievement: the channel really solved a collective-action problem no participant could solve alone, and readers really gained access. The tangled classification holds both because both are structurally present in the same arrangement. On genealogy: the founding problem — how to propagate a message when the incumbent controls every sanctioned channel — was genuinely solved by 1555, and the arrangement converted into established territorial churches rather than dissolving; mandatrophy is therefore resolved, the founding problem is dead, and the rising theater ratio marks the transition from functioning bypass machinery to commemorated origin. Whether that residue is capture-maintenance or legitimate retirement is left open as an omega rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the beneficiary_agency_reading of the technology_reformation_causality kernel. Would the technological_determinism_reading or the co_constitution_reading change the classification, and where exactly is the disagreement located?',
    'Not resolvable by data: the three readings partition one archive by causal ontology. Adopting determinism relocates the extracting structure from the coalition to the press itself; adopting co-constitution dissolves the coalition boundary into mutual shaping of medium and movement. Resolution means choosing a framework, not measuring.',
    'Under the determinism sibling, the press-infrastructure family member (printing_press_reformation_scaffold) carries the extraction and this coalition story collapses toward pure coordination; under co-constitution, the tangled boundary spreads to the medium, lowering this story''s attributable extraction and raising the infrastructure family member''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the causality kernel; disagreement located in causal ontology (agent vs. medium vs. co-production).').

omega_variable(
    monopoly_dissolution_as_extraction,
    'Does routing around the Church''s mediation monopoly count as extraction from the hierarchy, or as liberation that merely ends a position the hierarchy had no right to hold?',
    'Normative adjudication of whether pre-Reformation ecclesiastical authority was a legitimate interest or an unearned position. Contemporary testimony cuts both ways: Catholic controversialists describe plunder of their office; reformers describe liberation of conscience.',
    'If the hierarchy''s losses are not extraction, the victim set shrinks, epsilon falls materially, and the arrangement trends toward pure coordination; if they are, the tangled classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_dissolution_as_extraction, preference, 'Whether the primary external target''s losses are extraction or de-renting.').

omega_variable(
    subordinate_risk_distribution,
    'How much of the coalition''s realized enforcement risk fell on subordinate printers and tract-authors rather than being shared proportionally across partners?',
    'Quantify confiscations, prosecutions, and executions by shop size and movement alignment from imperial police records, city archives, and printers'' ledgers.',
    'If risk was roughly proportional, the mutual-extraction reading weakens toward coordination; if concentrated downward as the record suggests, the tangled classification and the 1525 spike in the measurement series are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinate_risk_distribution, empirical, 'Internal risk asymmetry measurement supporting the mutual-extraction claim.').

omega_variable(
    medium_counterfactual_substitution,
    'Could manuscript and oral networks have carried the reform program at scale without print, making the coalition''s dependence on printers — and hence the printers'' internal leverage — contingent rather than structural?',
    'Compare pre-1517 heterodox propagation rates (Devotio moderna, Waldensian, Hussite channels) against post-1517 velocity; model saturation ceilings for scrip-copy reproduction.',
    'If substitution was viable, the press-scaffold family member''s necessity claim weakens and printer pricing power inside the coalition shrinks; if not, printer leverage and the privilege rents are structural features of the arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medium_counterfactual_substitution, empirical, 'Counterfactual viability of non-print propagation for the reform program.').

omega_variable(
    post_settlement_zombie_status,
    'After the 1555 settlement kills the founding bypass problem, is the arrangement''s persistence capture-and-commemoration (anniversary culture, founding myth) or legitimate conversion into territorial churches?',
    'Track theater_ratio and enforcement function past 1555: if commemorative output grows while the bypass function is gone and enforcement continues under new owners, zombie maintenance is confirmed.',
    'Zombie confirmation dates a tangled_rope-to-piton transition and rewires the mandatrophy flags; the conversion reading instead closes the story as a successfully retired transitional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_settlement_zombie_status, conceptual, 'Post-1555 persistence: capture versus conversion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1517, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1517, 0.14).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1521, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1521, 0.3).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1525, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1525, 0.33).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1529, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1529, 0.29).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1531, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1531, 0.27).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1534, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1534, 0.31).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1538, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1538, 0.34).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1542, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1542, 0.33).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1546, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1546, 0.39).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1550, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1550, 0.37).
narrative_ontology:measurement(trca_beneficiary_agency_tr_t1555, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1555, 0.4).

% Extraction over time
narrative_ontology:measurement(trca_beneficiary_agency_be_t1517, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1517, 0.34).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1521, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1521, 0.46).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1525, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1525, 0.67).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1529, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1529, 0.58).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1531, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1531, 0.56).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1534, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1534, 0.61).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1538, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1538, 0.64).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1542, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1542, 0.62).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1546, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1546, 0.7).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1550, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1550, 0.73).
narrative_ontology:measurement(trca_beneficiary_agency_be_t1555, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1555, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(trca_beneficiary_agency_su_t1517, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1517, 0.18).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1521, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1521, 0.44).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1525, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1525, 0.6).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1529, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1529, 0.54).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1531, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1531, 0.5).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1534, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1534, 0.53).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1538, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1538, 0.57).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1542, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1542, 0.59).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1546, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1546, 0.69).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1550, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1550, 0.71).
narrative_ontology:measurement(trca_beneficiary_agency_su_t1555, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1555, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, printing_press_reformation_scaffold).

% DUAL FORMULATION NOTE:
% The colloquial label 'print caused the Reformation' decomposes, per the epsilon-invariance principle, into three readings of one kernel plus a separate infrastructure story. This file is the agency reading: epsilon 0.66 attaches to the coalition's operation (risk asymmetry, privilege rents, authority transfer). The determinism sibling attaches extraction to the medium itself; the co-constitution sibling distributes it across the entangled system; the scaffold family member (printing_press_reformation_scaffold) carries the transitional-infrastructure claim with its own sunset logic, since the press network outlived the bypass war it was built for. Chronology of the readings runs determinism (oldest, cited as settled by older textbooks) to co-constitution to agency refinement; upstream readings are cited as evidence by downstream ones, which is why this story links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
