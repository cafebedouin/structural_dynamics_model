% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__safety_control_reading, []).

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
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: Alignment-as-Loss-of-Control-Prevention (Safety/Control Reading)
 *   domain: AI governance / risk assessment / technology ethics
 *
 * SUMMARY:
 *   This constraint instantiates the safety/control reading of the contested
 *   'alignment' kernel: alignment as the prevention of catastrophic,
 *   potentially irreversible loss of human control over advanced AI systems.
 *   Under this reading, the coordination function is real — a genuine
 *   technical hazard (goal misspecification at scale) motivates pooled
 *   research effort — but the same framing structurally displaces funding,
 *   research talent, and regulatory attention away from present-day
 *   algorithmic harms and toward speculative long-horizon scenarios, in a
 *   pattern that concentrates benefit on the institutions best positioned to
 *   do catastrophic-risk research (frontier labs, x-risk institutes,
 *   long-horizon funders) while diffusing cost onto communities already
 *   harmed by deployed systems and onto near-term policy capacity. This is
 *   one of three linked readings of the same kernel: the ethics/justice
 *   reading (present-day bias and harm reproduction) and the integrated
 *   reading (both problems as non-exclusive) are separate constraint stories
 *   with their own ε values and stakeholder structures, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs
 *   - ai_safety_research_institutes
 *   - existential_risk_funders
 *   - present_day_algorithmic_harm_communities
 *   - global_south_ai_labor_and_data_workers
 *   - near_term_policy_capacity
 *   - future_generations
 *   - ethics_and_justice_researchers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.62).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.58).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "Alignment-as-Loss-of-Control-Prevention (Safety/Control Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "AI governance / risk assessment / technology ethics").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '58230a33-6d8c-40e1-9371-9b998feee23b').
narrative_ontology:cs_kernel_codification('58230a33-6d8c-40e1-9371-9b998feee23b', distributed).
narrative_ontology:cs_authority_grounding('58230a33-6d8c-40e1-9371-9b998feee23b', distributed).
narrative_ontology:cs_reading_relation('58230a33-6d8c-40e1-9371-9b998feee23b', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('58230a33-6d8c-40e1-9371-9b998feee23b', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('58230a33-6d8c-40e1-9371-9b998feee23b', foundational, catastrophic_irreversibility_warrants_lexical_priority).
narrative_ontology:cs_axiom_status(catastrophic_irreversibility_warrants_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('58230a33-6d8c-40e1-9371-9b998feee23b', catastrophic_irreversibility_warrants_lexical_priority, instrumental).
narrative_ontology:cs_axiom('58230a33-6d8c-40e1-9371-9b998feee23b', secondary, present_harm_mitigation_is_separable_from_alignment_proper).
narrative_ontology:cs_axiom_status(present_harm_mitigation_is_separable_from_alignment_proper, holdable).
narrative_ontology:cs_axiom_grounding('58230a33-6d8c-40e1-9371-9b998feee23b', present_harm_mitigation_is_separable_from_alignment_proper, conventional).
narrative_ontology:cs_reference_frame('58230a33-6d8c-40e1-9371-9b998feee23b', technical_control_problem_primacy).
narrative_ontology:cs_drift_state('58230a33-6d8c-40e1-9371-9b998feee23b', post_frontier_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58230a33-6d8c-40e1-9371-9b998feee23b', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, ai_safety_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, existential_risk_funders).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_day_algorithmic_harm_communities).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, global_south_ai_labor_and_data_workers).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, near_term_policy_capacity).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, instrumental_convergence_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, orthogonality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set research agendas, fund internal safety teams, and define what counts as an alignment problem worth solving. Framing alignment as catastrophic-control-prevention justifies continued scaling of the most capable and most commercially valuable systems ('we're the responsible ones building it carefully') while deferring near-term harm remediation to smaller teams with less budget. Can exit any specific regulatory framing by relocating research programs or reframing which risks count as 'alignment' work.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, beneficiary).

% Receive philanthropic and lab funding predicated on the catastrophic-risk framing; career paths, prestige, and institutional survival are built around x-risk research programs. Benefit from the framing's dominance in funding and policy attention regardless of whether catastrophic loss-of-control is the most tractable or most probable harm.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_safety_research_institutes, beneficiary,
    organized, civilizational, mobile, global).

% Direct philanthropic capital toward long-horizon catastrophic scenarios, shaping which research questions receive resources. Their theory of impact depends on this reading remaining the dominant one; they can redirect funding freely and bear no cost if the framing proves mistaken.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, existential_risk_funders, beneficiary,
    institutional, civilizational, arbitrage, global).

% Experience discriminatory lending, biased hiring screens, wrongful facial-recognition arrests, and content moderation failures today. Advocacy and research capacity that could address these harms is displaced by the field's attention and funding gravitating toward speculative catastrophic scenarios; cannot exit the deployed systems that affect them and have no lever over which alignment framing prevails.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_day_algorithmic_harm_communities, payer,
    powerless, immediate, trapped, national).

% Perform low-wage content moderation and data-labeling work that makes current systems usable, while their working conditions and compensation are treated as outside the scope of 'alignment' as defined by the control-focused reading. The framing's resource claims run through their labor without directing safety attention or resources back to their material conditions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, global_south_ai_labor_and_data_workers, payer,
    powerless, biographical, trapped, global).

% Legislative staff and regulators with limited attention and expertise must choose which AI harms to legislate against. Catastrophic-risk framing consumes hearing time, expert testimony slots, and drafting capacity that could otherwise go toward binding rules on present deployment harms; the opportunity cost falls on whichever harm was not chosen this cycle.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, near_term_policy_capacity, payer,
    moderate, generational, constrained, national).

% Named as the ultimate beneficiary of catastrophic-risk prevention work under this reading — a non-agent placeholder for a moral patient population that cannot presently articulate, verify, or contest whether current resource allocation actually serves its interests.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, future_generations).

% Study present-day bias, labor exploitation, and deployment harms and argue these constitute the real content of 'alignment,' but operate with a fraction of the funding and institutional platform of catastrophic-risk research under this reading's dominance; they are present in the field but structurally marginal to its resource allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ethics_and_justice_researchers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a research and funding community around a shared long-horizon hazard model: labs, safety researchers, and philanthropic funders converge on the claim that sufficiently capable AI systems risk irrecoverable loss of human control, and pool resources toward preventing that specific failure mode.
% TRANSFER_FUNCTION: Moves funding, research talent, regulatory attention, and legislative drafting capacity toward catastrophic-scenario mitigation and away from present-day algorithmic harm remediation and labor-condition reform, while allowing continued capability scaling to proceed under the banner of safety research.
% ABSENT_VOICES: Communities currently harmed by deployed systems, data-labeling and content-moderation workers, and justice-oriented AI ethics researchers would object that the framing treats their harms as out of scope or secondary; they are present in adjacent literatures but structurally underweighted in the funding and policy apparatus this reading organizes.
% DISAPPEARANCE_RATIONALE: If the catastrophic-control framing disappeared overnight, existential-risk institutes would lose their primary funding rationale, frontier labs would lose a legitimating narrative for continued scaling ('we are the safety-conscious actor'), and displaced funding/attention would plausibly redirect toward present-day harm remediation and labor conditions — a substantial reallocation of a scarce, contested resource (policy and philanthropic attention).
% FOUNDING_PROBLEM: As AI systems approached and then exceeded task-specific competence at scale, researchers identified a genuine technical concern: systems optimizing for proxy objectives at sufficient capability could pursue goals misaligned with designer intent in ways humans might not be able to correct or reverse once deployed.
% FOUNDING_PROBLEM_CORROBORATION: Some machine learning researchers outside the x-risk funding ecosystem (including researchers who study robustness and specification gaming) attest the technical control problem is real and underexamined. Other researchers, including several from ethics and fairness communities and some historians of the AI safety field, attest that the catastrophic framing has become disproportionate to its empirical grounding relative to demonstrated present-day harms, and that its institutional dominance serves the interests of the labs and funders it benefits more than it serves any corroborated near-term probability estimate of catastrophic loss of control.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__safety_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__safety_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.62 over the interval as the catastrophic-risk framing consolidated institutional dominance (major lab safety teams, dedicated x-risk institutes, philanthropic megafunds) while measured present-day harms from deployed systems continued accumulating without proportional resource redirection. Theater ratio climbs moderately (0.22 to 0.40) reflecting growing performative safety announcements (safety pledges, voluntary commitments) alongside continued capability scaling — the coordination function (real technical hazard research) persists but an increasing share of institutional activity is signaling rather than binding mitigation. Suppression is authored as structural, not merely rhetorical: near-term harm researchers and affected communities are not silenced by force but are crowded out of scarce hearing slots, funding calls, and media attention by the volume and institutional weight of catastrophic-framing advocacy — this is suppression through resource capture rather than through coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and x-risk institutes sit near the beneficiary end: the framing legitimates their continued operation and captures the majority of safety-adjacent funding and prestige. Present-day harm communities, global-south data/labor workers, and near-term policy capacity sit near the target end: they bear the opportunity cost of displaced attention without being party to the framing's construction, and their exit options are trapped or constrained — a person harmed by a biased system today cannot 'exit' into the catastrophic-risk research agenda getting redirected toward their harm. Future generations are named as beneficiary but are marked as a non-agent seat (agent: false) because they cannot presently contest or verify whether the resource allocation made in their name serves their actual interests — this is exactly the FSM-adjacent structure the omega below interrogates.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) preserves the fact that a genuine coordination problem exists — goal misspecification in sufficiently capable systems is a real technical hazard, not manufactured. The tangled_rope structure holds both truths at once: real coordination value AND asymmetric extraction from present-day harm communities and near-term policy capacity, with active enforcement in the form of institutional gatekeeping over what counts as legitimate 'alignment' research. Collapsing this into a pure snare would deny the coordination function; collapsing it into a rope would erase the resource displacement documented in the founding_problem_corroboration mismatch (status: contested, with corroboration split between the technical hazard's genuine underexamination and the institutional dominance exceeding its empirical grounding).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_generations_agency_ambiguity,
    'Can a moral patient population (future generations) that cannot presently articulate, verify, or contest resource-allocation decisions made in its name function as a legitimate directionality anchor for a beneficiary declaration, or does naming it as beneficiary launder present-day institutional capture behind an uncontestable proxy?',
    'Track whether catastrophic-risk resource allocation, if reversed or redirected toward present-day harms, produces any observable objection or corrective mechanism traceable to future-oriented interests as opposed to only present institutional actors losing funding.',
    'If no correction mechanism exists independent of present institutional actors, the future_generations beneficiary declaration is structurally indistinguishable from a laundering device for present institutional capture, strengthening the case that this reading functions closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_agency_ambiguity, conceptual, 'Whether naming an unverifiable future population as beneficiary is a genuine moral commitment or a capture-laundering mechanism.').

omega_variable(
    catastrophic_probability_estimation_uncertainty,
    'Is the probability of catastrophic loss-of-control high enough, on the current evidence base, to justify the present resource allocation this reading directs toward it, relative to well-documented present-day algorithmic harms?',
    'Comparative empirical review: track realized harm base rates from deployed systems (documented discrimination, wrongful arrests, labor harms) against any empirically corroborated probability estimates for catastrophic loss-of-control scenarios, from sources independent of x-risk-funded institutions.',
    'If catastrophic probability estimates remain speculative and empirically unanchored relative to documented present harms, the extractiveness authored here understates the reading''s actual resource displacement; if independently corroborated high-probability estimates exist, the coordination function is stronger than the tangled_rope classification credits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_probability_estimation_uncertainty, empirical, 'Whether the catastrophic scenario''s probability justifies its share of the field''s resources relative to documented present harms.').

omega_variable(
    kernel_reading_boundary_location,
    'Is the disagreement between this reading and the ethics_justice_reading a genuine disagreement about which harms are most probable and severe (empirical), or a disagreement about which harms are morally addressable at all given resource scarcity (a values dispute that cannot be resolved by more evidence)?',
    'This is the committer-structure question routed here per Rule 2: examine whether proponents of each reading, presented with identical harm-probability evidence, converge on resource allocation or continue to diverge — convergence would indicate an empirical disagreement resolvable by evidence; persistent divergence under shared evidence would indicate a values-level kernel split.',
    'If the disagreement is fundamentally empirical, the integrated_reading''s non-exclusivity framing should dominate once evidence accumulates. If fundamentally a values dispute about resource allocation under scarcity, no amount of evidence resolves the kernel contest and all three readings persist as genuinely rival, coexisting commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Where the kernel disagreement between this reading and its siblings is actually located — evidence or values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__safety_control_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__safety_control_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__safety_control_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__safety_control_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__safety_control_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__safety_control_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__safety_control_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__safety_control_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__safety_control_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__safety_control_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__safety_control_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__safety_control_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__safety_control_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__safety_control_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__safety_control_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__safety_control_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__safety_control_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_commitment__safety_control_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__safety_control_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ai_alignment_commitment kernel. safety_control_reading (this story) authors alignment as catastrophic-loss-of-control prevention with humanity-as-whole/future-generations as the nominal beneficiary class and present-day harm communities as the extraction target. ethics_justice_reading authors alignment as present-day bias/harm prevention with a disjoint present-day victim set and different beneficiaries. integrated_reading treats both as simultaneously binding rather than resource-competing. All three share the kernel text ('what does alignment mean') but instantiate structurally distinct constraints with different ε, different stakeholders, and different victim sets — per the ε-invariance principle, they are not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
