% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Constitutional Authority
 *   domain: constitutional theory/political philosophy/comparative law
 *
 * SUMMARY:
 *   This story generates the popular sovereignty reading of the
 *   constitutional text kernel: the claim that neither courts nor legislature
 *   hold final interpretive authority, and that constituent power resting in
 *   the demos — exercised through amendment, convention, or, in extremis,
 *   revolution — is the ultimate ground of constitutional meaning. This is
 *   one of three sibling readings of the same kernel
 *   (judicial_supremacy_reading, legislative_sovereignty_reading being the
 *   other two, each authored as separate constraint files). The ε authored
 *   here (0.42) is specific to THIS reading's structural operation — the
 *   transfer of legitimating authority to mobilized popular expression and
 *   the corresponding devaluation of doctrinal stability and minority
 *   protections — and is not averaged against or hedged by the siblings' ε
 *   values. Where courts serve as the primary check against majoritarian
 *   overreach in the sibling judicial_supremacy_reading, this reading treats
 *   that very check as illegitimate insulation of unelected authority from
 *   popular will.
 *
 * KEY AGENTS:
 *   - mobilized_citizen_movements: Primary beneficiary (organized/constrained) — gains legitimacy and mobilizational standing from the reading
 *   - institutional_continuity_actors: Primary target (institutional/trapped) — bears the cost of perpetual constitutional revisability
 *   - professional_legal_class: Secondary target (powerful/constrained) — expertise devalued by the reading's premise
 *   - minority_rights_claimants: Most vulnerable target (powerless/trapped) — loses the durable judicial check the reading treats as subordinate
 *   - comparative_constitutional_scholars: Analytical observer — sees the full comparative structure across judicial, legislative, and popular sovereignty systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.42).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.31).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Popular Sovereignty Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional theory/political philosophy/comparative law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '3998bdd7-826f-49d6-88e7-f0773b865c73').
narrative_ontology:cs_kernel_codification('3998bdd7-826f-49d6-88e7-f0773b865c73', distributed).
narrative_ontology:cs_authority_grounding('3998bdd7-826f-49d6-88e7-f0773b865c73', distributed).
narrative_ontology:cs_reading_relation('3998bdd7-826f-49d6-88e7-f0773b865c73', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3998bdd7-826f-49d6-88e7-f0773b865c73', constitutional_text__legislative_sovereignty_reading, influences).
narrative_ontology:cs_axiom('3998bdd7-826f-49d6-88e7-f0773b865c73', foundational, constituent_power_precedes_created_institutions).
narrative_ontology:cs_axiom_status(constituent_power_precedes_created_institutions, holdable).
narrative_ontology:cs_axiom_grounding('3998bdd7-826f-49d6-88e7-f0773b865c73', constituent_power_precedes_created_institutions, deontological).
narrative_ontology:cs_axiom('3998bdd7-826f-49d6-88e7-f0773b865c73', secondary, extra_institutional_mobilization_carries_interpretive_authority).
narrative_ontology:cs_axiom_status(extra_institutional_mobilization_carries_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('3998bdd7-826f-49d6-88e7-f0773b865c73', extra_institutional_mobilization_carries_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('3998bdd7-826f-49d6-88e7-f0773b865c73', constituent_assembly_founding_moment).
narrative_ontology:cs_drift_state('3998bdd7-826f-49d6-88e7-f0773b865c73', contemporary_institutionalized_democracies, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3998bdd7-826f-49d6-88e7-f0773b865c73', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, constitutional_convention_delegates).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_amendment_campaigns).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_continuity_actors).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, professional_legal_class).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, minority_rights_claimants).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_amendment_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize petitions, referenda campaigns, and constitutional conventions to assert that ultimate interpretive authority sits with the demos, not courts or legislatures. They collect legitimacy and mobilizational power from this reading — every successful popular amendment or convention vindicates the claim that ordinary institutional channels are subordinate to direct democratic expression. Their exit from the reading itself would mean returning to reliance on courts or parliament, which is precisely what the movement exists to route around.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, mobilized_citizen_movements, agenda_setter).

% Gain standing and authority only because the popular sovereignty reading treats convention assembly as a legitimate constraint-generating mechanism superior to ordinary legislative or judicial process. Their entire role depends on the reading holding; if judicial or legislative supremacy prevailed, conventions would be advisory at best.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_convention_delegates, beneficiary,
    moderate, biographical, constrained, national).

% Draft and circulate amendment proposals justified by appeal to popular constituent power rather than legislative supermajority or judicial approval. They benefit from the reading's premise that the people's will, expressed through amendment or revolutionary founding moments, outranks both other branches.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_amendment_campaigns, beneficiary,
    organized, generational, constrained, national).

% Courts, standing legislatures, and administrative agencies whose settled precedent, procedural stability, and institutional memory are treated as provisional and revisable at any moment popular mobilization reaches critical mass. They cannot exit the reading's jurisdiction — it applies to the entire constitutional order they operate within — and every popular sovereignty invocation threatens to unmake settled law regardless of institutional investment in continuity.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_continuity_actors, payer,
    institutional, civilizational, trapped, national).

% Judges, constitutional scholars, and appellate advocates whose expertise in doctrinal interpretation is devalued whenever the reading asserts that untrained popular expression can override trained legal judgment. Their institutional capital erodes each time a convention or amendment campaign succeeds by appeal to raw popular will rather than to legal craft; they cannot leave the jurisdiction since their professional identity is constituted by it.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, professional_legal_class, payer,
    powerful, biographical, constrained, national).

% Individuals and groups whose rights protections were historically secured through judicial doctrine precisely because they lacked numbers to prevail in popular votes. Under the popular sovereignty reading, courts cannot serve as a durable check against majoritarian amendment or convention-driven revision — their protections are only as stable as the next popular mobilization, and they have no exit from the political community whose majority can revise the terms.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Study how popular sovereignty readings play out across jurisdictions with conventions, referenda, and revolutionary refoundings, comparing outcomes to judicial supremacy and legislative sovereignty systems. They document the pattern without occupying any of the contesting seats.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading solves the problem of institutional ossification: it provides a mechanism for constitutional meaning to be revised when courts and legislatures are captured, calcified, or unresponsive to the polity's actual will, by routing ultimate authority to conventions, amendment processes, and — in extremis — revolutionary refounding.
% TRANSFER_FUNCTION: Interpretive and legitimating authority moves away from settled institutions (courts' doctrinal continuity, legislatures' procedural supermajorities) toward mobilized popular expression — conventions, amendment campaigns, and constituent assemblies — whenever such expression can be organized and sustained.
% ABSENT_VOICES: Minority rights claimants and long-horizon institutional actors who depend on doctrinal stability rarely have a seat in convention design or amendment-threshold debates; their objection — that popular majorities are the very force constitutional rights protections exist to check — is voiced mainly by the professional legal class on their behalf, a proxy that this reading treats as suspect.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty reading vanished overnight — if courts and legislatures were universally accepted as jointly exhaustive of constitutional authority — conventions would lose their claimed legitimacy, amendment campaigns would need to route entirely through existing supermajority procedures, and revolutionary or extra-institutional constitutional change would become simply illegal rather than a live category of legitimate action. Popular mobilizations organized around constituent power would have no doctrinal anchor.
% FOUNDING_PROBLEM: Written constitutions face a legitimacy problem: if courts or legislatures are the final word on their own authority, the constitution cannot check the very institutions it creates, and there is no answer to who authorized the founding generation's constitution-writing in the first place. The popular sovereignty reading answers both by locating authority prior to and outside all created institutions, in the constituent power of the people themselves.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional theorists in the republican and popular-constitutionalism traditions (outside the mobilized movements themselves) attest the founding problem — self-authorizing institutions cannot ground their own legitimacy — remains live. Comparative constitutional scholars observing minority-rights outcomes in convention-heavy and referendum-heavy systems attest that in practice the reading is frequently invoked instrumentally by well-organized majorities rather than in genuine constitutional crisis, suggesting the founding problem's live status is asserted more often than demonstrated.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a genuine coordination function — resolving the self-authorization paradox — coupled with real costs imposed on institutional actors and minority claimants whenever popular mobilization actually displaces settled doctrine. It is lower than a pure extraction mechanism because the reading does perform real coordination work (breaking institutional ossification) some of the time. Theater ratio rises across the interval (0.35→0.55) because as the reading becomes more institutionally routine — conventions and amendment campaigns developing their own procedural theater, invocation of 'the people's will' becoming a rhetorical move deployed by well-resourced organizers rather than a genuine constitutional-crisis response — an increasing share of its invocation is performative appeal to legitimacy rather than a response to actual institutional failure. Suppression is comparatively low and rises only modestly (0.20→0.31): the reading does not depend heavily on coercing acceptance, because its persuasive force comes from appeal to democratic legitimacy rather than institutional command — though this rises as organized movements develop repeat-invocation playbooks.
 *
 * PERSPECTIVAL GAP:
 *   From the mobilized-movement seat, this reading is coordination in its purest form: it is the mechanism by which an unresponsive or captured institutional order is made answerable to the people who are supposed to be its ultimate authors. From the institutional-continuity and minority-rights seats, the same structure computes as extraction: authority that was settled and defensible is rendered perpetually contestable by whoever can organize the largest mobilization, and protections that existed specifically because they were insulated from majority preference are stripped of that insulation. The engine should compute a tangled-rope classification from institutional and minority seats and something closer to a rope from the mobilized-movement seat — that divergence is the structural fact the story is documenting, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobilized citizen movements, convention delegates, and amendment campaigns sit near the beneficiary end: the reading is the doctrinal ground on which their standing and legitimacy claims rest, and they collect political capital each time constituent power successfully overrides institutional resistance. Institutional continuity actors and the professional legal class sit near the target end: their claim to final or authoritative interpretation is structurally subordinated by this reading, and they cannot exit the constitutional order in which the reading operates. Minority rights claimants sit at the most extreme target position — trapped, powerless — because the specific protective function the reading displaces (durable judicial insulation from majoritarian revision) was disproportionately load-bearing for exactly this group.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem — the self-authorization paradox of institutional power — remains live in the abstract (any system with courts and legislatures as the sole authorities does face a genuine grounding problem), but its practical invocation increasingly serves well-organized political movements pursuing ordinary substantive goals rather than genuine constitutional emergencies. This is the mandatrophy risk: a doctrine justified by extraordinary founding-moment necessity being routinely invoked for garden-variety political contestation, which risks converting a genuine constituent-power safety valve into a standing tool for majoritarian override of minority protections. The tangled_rope classification captures this precisely — coordination function present and real, but shot through with asymmetric extraction from institutional stability and minority rights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_ontological_status,
    'Is ''the constituent power of the demos'' a coherent, identifiable political agent capable of authorizing constitutional revision, or is it a legitimating fiction invoked by whichever faction can currently claim to speak for it?',
    'Comparative study of convention outcomes across jurisdictions: does invocation of constituent power correlate with genuinely broad-based, cross-factional mobilization, or predominantly with narrow but well-organized interest coalitions claiming to represent ''the people''?',
    'If constituent power functions mainly as a legitimating label for organized factions, the reading''s coordination claim weakens substantially and the classification should move toward snare (extraction dressed as popular legitimacy); if it tracks genuine broad mobilization, the tangled_rope classification''s coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_power_ontological_status, conceptual, 'Whether constituent power is a real collective agent or a rhetorical vehicle for factional claims.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the popular sovereignty reading''s claim that neither courts nor legislature are supreme logically foreclose the legislative_sovereignty_reading''s claim that parliament has final say, or can both be held by different factions without contradiction within the same constitutional order?',
    'Doctrinal analysis of whether any actual constitutional system has successfully operated with both readings held simultaneously by different institutional actors without one displacing the other in practice, versus systems where adoption of one reading has historically preceded exclusion of the other.',
    'If the readings can coexist (different factions holding different theories without institutional collapse), coexists_with is the right relation; if adoption of popular sovereignty as operative doctrine has historically required abandoning legislative-supremacy claims as the final word, forecloses may be more accurate — this affects how the reading''s expected structural delta is modeled in the cs_structure edges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether popular sovereignty and legislative sovereignty readings are logically compatible or mutually exclusive within one constitutional framework.').

omega_variable(
    minority_protection_alternative_mechanisms,
    'Do popular-sovereignty-reading systems develop alternative durable protections for minorities (supermajority amendment thresholds, entrenched rights clauses immune even to conventions) that substitute for the judicial insulation this reading displaces, or does displacement of judicial supremacy leave a genuine protection gap?',
    'Empirical survey of minority rights outcomes in jurisdictions with strong popular-constitutionalism traditions versus strong judicial-supremacy traditions, controlling for other factors (federalism, electoral system, civil society strength).',
    'If substitute protections reliably emerge, the victim-class severity for minority_rights_claimants is overstated; if no substitute reliably emerges, the extractiveness score for this reading may be understated relative to its actual cost to that group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_alternative_mechanisms, empirical, 'Whether the reading generates compensating protective mechanisms for minorities or leaves a genuine gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__popular_sovereignty_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__popular_sovereignty_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__popular_sovereignty_reading, theater_ratio, 24, 0.49).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__popular_sovereignty_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t8, constitutional_text__popular_sovereignty_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(cons_be_t16, constitutional_text__popular_sovereignty_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(cons_be_t24, constitutional_text__popular_sovereignty_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(cons_be_t32, constitutional_text__popular_sovereignty_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t8, constitutional_text__popular_sovereignty_reading, suppression_requirement, 8, 0.23).
narrative_ontology:measurement(cons_su_t16, constitutional_text__popular_sovereignty_reading, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(cons_su_t24, constitutional_text__popular_sovereignty_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(cons_su_t32, constitutional_text__popular_sovereignty_reading, suppression_requirement, 32, 0.3).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint files decomposing the natural-language concept 'ultimate constitutional interpretive authority' per the ε-invariance principle: constitutional_text__popular_sovereignty_reading (this file), plus sibling files for judicial_supremacy_reading and legislative_sovereignty_reading. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed_type — they are not the same constraint measured three ways, but three structurally distinct constraints sharing a contested kernel (constitutional_text). All three link to each other via affects_constraints to preserve the family structure for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
