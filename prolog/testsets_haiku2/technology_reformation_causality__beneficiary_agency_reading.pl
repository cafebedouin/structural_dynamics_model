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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)
 *   domain: history_of_technology/religious_authority/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'beneficiary agency' reading of the
 *   contested kernel 'technology_reformation_causality.' The kernel asks:
 *   what caused the Reformation—printing technology, human strategic choice,
 *   or co-evolution of both? This reading privileges the strategic agency of
 *   the reformer-printer coalition. It asserts that reformers and commercial
 *   printers made deliberate choices to deploy printing technology as a tool
 *   to bypass Church authority, rather than treating printing as a
 *   determining cause or as co-evolving neutrally with social forces. The
 *   reading models the constraint as a tangled_rope: mutual benefit
 *   (reformers gain authority bypass, printers gain profitable market)
 *   combined with asymmetric extraction (the Church loses authority monopoly
 *   and economic returns; scriptoria lose market share) and active
 *   enforcement (Church suppression of specific texts and printers). The
 *   claim/metric gap is intentional per the kernel-reading frame: this
 *   reading's ε (0.68, high extractiveness) and type (tangled_rope, requiring
 *   active enforcement) differ from the technological_determinism sibling
 *   (which would produce lower ε, rope type) and the co_constitution sibling
 *   (which would distribute credit differently). These divergences are not
 *   measurement errors; they are the reading's signal.
 *
 * KEY AGENTS:
 *   - Protestant reformers (Luther, Zwingli, Calvin's circles): organized, identity-locked, acting from theological conviction and institutional rivalry with Rome; chose which texts to print and which printers to patronize
 *   - Commercial printers (Gutenberg, Froben, Koberger networks): organized, mobile, acting from profit motive; chose to serve the reform market and locate strategically to avoid Church pressure
 *   - Catholic Church hierarchy: institutional, trapped (cannot destroy printing without destroying other institutional functions), bearing suppression costs and authority loss
 *   - Manuscript scriptoria: moderate power, constrained exit, displaced by coalition's fast cheap reproduction strategy
 *   - Common literacy-seeking audiences: powerless, incidentally benefited by coalition's extraction from the Church
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
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_authority/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '88d54f5a-43d4-4f5d-9e0c-42306f7a299d').
narrative_ontology:cs_kernel_codification('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', distributed).
narrative_ontology:cs_authority_grounding('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', lineage).
narrative_ontology:cs_reading_relation('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', technology_reformation_causality__technological_determinism_reading, influences).
narrative_ontology:cs_reading_relation('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', foundational, human_strategic_choice_necessary_for_causation).
narrative_ontology:cs_axiom_status(human_strategic_choice_necessary_for_causation, holdable).
narrative_ontology:cs_axiom_grounding('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', human_strategic_choice_necessary_for_causation, empirically_contingent).
narrative_ontology:cs_axiom('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', foundational, technology_is_tool_not_determining_force).
narrative_ontology:cs_axiom_status(technology_is_tool_not_determining_force, holdable).
narrative_ontology:cs_axiom_grounding('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', technology_is_tool_not_determining_force, instrumental).
narrative_ontology:cs_reference_frame('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', church_monopoly_on_scriptural_authority).
narrative_ontology:cs_drift_state('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', post_reformation_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('88d54f5a-43d4-4f5d-9e0c-42306f7a299d', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, commercial_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, manuscript_scriptoria).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, literacy_expansion_beneficiaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sought to bypass Church monopoly on scripture interpretation and distribution. Made deliberate strategic choices about what texts to commission, which printers to patronize, which vernacular translations to promote, and how to distribute them outside formal Church channels. Their theological agenda drove printer selection and text selection; the printing technology was the execution mechanism they chose and weaponized strategically.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers, beneficiary).

% Seized market opportunity created by reformer demand for vernacular scripture and anti-Catholic polemic. Profited substantially from high-volume, fast-turnaround runs of reform texts. Made business decisions about which manuscripts to accept, which cities to operate in to minimize Church pressure, and how to distribute through non-traditional channels. Their profit motive aligned with reformer strategy; they were not passive vessels but active coalition members choosing to serve this market.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, beneficiary).

% Faced coordinated, strategically deployed text production that undermined its authority monopoly over scripture. Could not suppress the printing technology itself; its suppression efforts had to target specific texts, specific printers, and distribution networks. Bears the cost of loss of authority over scriptural interpretation and the institutional revenue consequences of vernacular scripture circulation outside its control.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Lost market share and institutional patronage as printed books displaced manuscript production. Monastic and secular scriptoria that had controlled the supply of expensive, slowly-produced religious texts saw demand collapse as reformers flooded the market with cheap printed vernacular versions. Bore the economic cost of technological substitution weaponized by the reformer-printer coalition.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, manuscript_scriptoria, payer,
    moderate, biographical, constrained, regional).

% Church authority figures and allied printers who might have controlled religious printing were preempted by the reformer-printer coalition's capture of the fastest-growing market segment. Had no say in strategic choices about text selection, distribution, or printing volume; were structurally prevented from competing for this market.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, church_printing_monopoly_aspirants, excluded,
    institutional, generational, trapped, regional).

% Common people gained access to scripture in vernacular languages at affordable cost. The coalition's strategic deployment produced a genuine public good: literacy expansion and demystification of religious authority. These beneficiaries did not drive the strategy but benefited from it; they are incidental beneficiaries whose existence depends on the coalition's extraction from the Church.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, literacy_expansion_beneficiaries, beneficiary,
    powerless, biographical, constrained, continental).

% Historians and media scholars examining the causal structure of the Reformation-technology nexus. Assess whether printing made the Reformation inevitable, whether reformer-printer strategy was decisive, or whether both co-evolved. This reading privileges human agency and strategic choice; sibling readings emphasize technological affordance or co-constitution.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, temporal_authority_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers coordinated printer expertise, capital, and distribution networks to solve the problem of reaching audiences the Church controlled: fast reproduction of vernacular scripture, rapid distribution across multiple jurisdictions to outpace suppression, and deployment of polemic that attacked Church authority directly. Printers coordinated theological expertise, market intelligence, and capital to solve the problem of finding profitable text markets in a changing religious landscape. The coalition solved the coordination problem of translating theological strategy into distributed textual reach.
% TRANSFER_FUNCTION: Moves authority and institutional revenue from the Catholic Church hierarchy to the reformer-printer coalition: the Church loses monopoly on scripture interpretation and the economic returns from controlling manuscript scarcity; reformers and printers gain legitimacy, market access, and profit. A secondary flow moves literacy and scriptural access from privileged clergy to common people, a genuine good that rides on the extraction from the Church.
% ABSENT_VOICES: Church authorities who wished to control or monopolize printing of religious texts had no seat at the coalition's strategic table; their positions were pre-empted by reformer-printer capture of the fastest-growing market. Manuscript scriptoria workers and those invested in the old system of scarcity-based knowledge control were excluded from decisions about transition speed and scope.
% DISAPPEARANCE_RATIONALE: If the coalition's strategic coordination vanished overnight—if reformers had not chosen to deploy printing and printers had not chosen to serve that market—the Reformation would have unfolded at a different pace and scale. Without coordinated mass text production, reformer ideas would have circulated through older networks (letter, sermon, manuscript), reaching narrower audiences more slowly. The Church's authority monopoly would have degraded differently. This is not to say printing would not have been invented or used; it is to say the *strategic deployment* mattered: the coalition shaped the *when*, *what*, and *how* of printing's application to religious authority.
% FOUNDING_PROBLEM: Church monopoly on scriptural interpretation, enforced through control of expensive manuscript production and clerical gatekeeping of text access. Reformers' theological claims required reaching audiences the Church controlled. The printing technology existed; the problem was deploying it to bypass Church authority structures.
% FOUNDING_PROBLEM_CORROBORATION: Reformer correspondence and commission records document strategic choices about which texts to print and which printers to patronize. Printer guild records and business accounts show profitable expansion into reform texts. Church suppression records document targeted banning of specific printed texts and printers, not general prohibition of printing. Institutional historians outside the reformer tradition (secular media scholars, Catholic historians) confirm that reformer-printer coordination was deliberate and strategically consequential, even where they dispute whether printing was determining.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.15 to 0.68 over the century as the coalition captures an increasingly large share of the text market and the Church's authority monopoly degrades. This is not because printing became more powerful; it is because the coalition's strategic deployment deepened and the Church's ability to suppress specific targets (while maintaining its own use of printing) became inadequate. Suppression follows a similar arc (0.12→0.71), tracking the escalation of targeted enforcement: the Church could not suppress printing technology itself without harming its own institutional needs, so it had to suppress specific texts and printers, requiring escalating institutional effort. Theater ratio is low-to-moderate (0.08→0.42) because enforcement is largely real (actual book burning, printer persecution, text banning) rather than performative, though late-stage theater increases as suppression becomes routinized and less effective. Accessibility_collapse (0.58) is moderate because the coalition's texts circulated, but alternatives (Latin, clerical gatekeeping) persisted and the choice set was still constrained by literacy and geography. Resistance (0.54) is moderate-high: the coalition faced real Church opposition; the Church faced real coalition coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer-printer coalition's position, this is strategic coordination solving a genuine problem (reaching audiences the Church monopolized). From the Church's position, it is coordinated extraction of authority and institutional revenue. The payer (Church) experiences this as enforced extraction; the agenda-setters experience it as justified bypass of unjust monopoly. The engine computes these divergences from structural data (beneficiary/victim, power atoms, exit options, enforcement flags) without endorsing either seat's moral framing. The analytical seat (historians) disputes whether the causal structure supports the coalition's or technology's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are beneficiaries: they gain authority (reformers) and profit (printers) from the coalition's success. Directionality for both is low (near full-beneficiary end), though constrained by suppression and identity-lock (reformers) vs. mobility (printers). The Church is the target: it loses authority monopoly and suppression costs its treasury. Directionality is high (near full-target end), modulated only by the fact that the Church retains institutional legitimacy and can suppress specific threats (incomplete capture). Common people are incidental beneficiaries whose literacy and access derive from coalition extraction; their directionality is near zero (full beneficiary) but their exit is constrained by literacy and geography. Manuscript scriptoria are payers displaced by technology substitution weaponized by the coalition; their directionality is high (targets) but their power is eroded and organizational cohesion weak.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding_problem (Church monopoly on scriptural interpretation) remains live at interval end (1550), and its disappearance_verdict is world_rearranges: the coalition's strategic deployment mattered. The constraint will not resolve into a piton because the reformers and printers remain motivated by both theology (reformers) and profit (printers) and the Church continues suppression. The constraint is genuinely tangled_rope (mutual coordination benefit + asymmetric extraction from the Church) requiring active enforcement (Church suppression of texts and printers). The coordination function is real; the extraction is not cover for failed coordination but the asymmetric benefit distribution from a real coordination problem (reaching audiences the Church controlled).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1450, 0.08).
narrative_ontology:measurement_basis(tech_tr_t1450, observed).
narrative_ontology:measurement(tech_tr_t1475, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1475, 0.15).
narrative_ontology:measurement_basis(tech_tr_t1475, observed).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1490, 0.24).
narrative_ontology:measurement_basis(tech_tr_t1490, observed).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1510, 0.35).
narrative_ontology:measurement_basis(tech_tr_t1510, observed).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1530, 0.4).
narrative_ontology:measurement_basis(tech_tr_t1530, observed).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1550, 0.42).
narrative_ontology:measurement_basis(tech_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement_basis(tech_be_t1450, observed).
narrative_ontology:measurement(tech_be_t1475, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1475, 0.28).
narrative_ontology:measurement_basis(tech_be_t1475, observed).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1490, 0.42).
narrative_ontology:measurement_basis(tech_be_t1490, observed).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1510, 0.58).
narrative_ontology:measurement_basis(tech_be_t1510, observed).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1530, 0.66).
narrative_ontology:measurement_basis(tech_be_t1530, observed).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1550, 0.68).
narrative_ontology:measurement_basis(tech_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1450, 0.12).
narrative_ontology:measurement_basis(tech_su_t1450, observed).
narrative_ontology:measurement(tech_su_t1475, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1475, 0.31).
narrative_ontology:measurement_basis(tech_su_t1475, observed).
narrative_ontology:measurement(tech_su_t1490, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1490, 0.48).
narrative_ontology:measurement_basis(tech_su_t1490, observed).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1510, 0.64).
narrative_ontology:measurement_basis(tech_su_t1510, observed).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1530, 0.69).
narrative_ontology:measurement_basis(tech_su_t1530, observed).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1550, 0.71).
narrative_ontology:measurement_basis(tech_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.14).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'technology_reformation_causality.' All three readings (beneficiary_agency, technological_determinism, co_constitution) share the historical kernel (printing's invention and the Reformation's unfolding) but instantiate different constraints because they locate causal credit differently. Beneficiary_agency emphasizes reformer-printer strategic coordination extracting from Church authority (tangled_rope, ε=0.68). Technological_determinism emphasizes printing's enabling role (rope or scaffold, lower ε). Co_constitution emphasizes mutual shaping (likely tangled_rope but distributed credit). The three readings are linked via network.affects_constraints as a constraint family because each reading's credibility affects the others' interpretive burden and because institutional historians recognize these as competing framings of the same historical nexus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
