% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: AI Safety as Existential Risk Prevention (Alignment/Extinction Reading)
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'AI safety' kernel:
 *   the claim that AI safety fundamentally means preventing extinction-level
 *   outcomes from misaligned superintelligent systems. This reading has a
 *   distinctive structural signature — its victim set (all future humans,
 *   potentially infinite) is unlike any other constraint's victim set because
 *   it cannot be corroborated or contested by the parties it claims to
 *   protect, and its beneficiary set includes 'humanity conditional on
 *   alignment success,' a beneficiary that only cashes out in an unverifiable
 *   counterfactual. The reading concentrates high ε on speculative,
 *   resource-intensive technical interventions (large-scale alignment
 *   research programs, interpretability agendas, governance proposals for
 *   pause or slowdown) while implicitly treating present algorithmic
 *   accountability work as lower priority — this is precisely the transfer
 *   function the sibling near_term_harms_reading exists to contest. This
 *   story does NOT attempt to average across sibling readings or hedge its ε;
 *   it is written as the clean existential-risk claim, full stop. The sibling
 *   readings (near_term_harms_reading, dual_priority_reading) are separate
 *   constraint files with their own ε, own stakeholders, own classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.61).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.42).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety as Existential Risk Prevention (Alignment/Extinction Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '6ceb596c-bfb3-4a72-800e-37969936827f').
narrative_ontology:cs_kernel_codification('6ceb596c-bfb3-4a72-800e-37969936827f', distributed).
narrative_ontology:cs_authority_grounding('6ceb596c-bfb3-4a72-800e-37969936827f', distributed).
narrative_ontology:cs_reading_relation('6ceb596c-bfb3-4a72-800e-37969936827f', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ceb596c-bfb3-4a72-800e-37969936827f', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('6ceb596c-bfb3-4a72-800e-37969936827f', foundational, extinction_scale_harm_dominates_expected_value).
narrative_ontology:cs_axiom_status(extinction_scale_harm_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('6ceb596c-bfb3-4a72-800e-37969936827f', extinction_scale_harm_dominates_expected_value, instrumental).
narrative_ontology:cs_axiom('6ceb596c-bfb3-4a72-800e-37969936827f', secondary, capability_threshold_crossing_is_irreversible).
narrative_ontology:cs_axiom_status(capability_threshold_crossing_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('6ceb596c-bfb3-4a72-800e-37969936827f', capability_threshold_crossing_is_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('6ceb596c-bfb3-4a72-800e-37969936827f', pre_agi_risk_neutral_baseline).
narrative_ontology:cs_drift_state('6ceb596c-bfb3-4a72-800e-37969936827f', post_frontier_model_scaling_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6ceb596c-bfb3-4a72-800e-37969936827f', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment_success).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, existential_risk_research_institutes).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_day_algorithmic_harm_victims).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, future_humans_under_uncertainty).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, ai_governance_reform_advocates_for_near_term_harms).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, instrumental_convergence_thesis).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, orthogonality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and staff alignment research, publish safety frameworks (RLHF, interpretability, scaling-law risk assessments), and simultaneously build the frontier systems the risk narrative is about. They set which technical interventions count as 'safety work,' secure regulatory carve-outs by presenting themselves as the responsible actors best positioned to solve the problem they are creating, and can exit any given commitment by citing competitive pressure from labs that did not adopt it.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, beneficiary).

% Receive funding, prestige, and policy access premised on extinction-level risk being the central framing of AI safety. Careers, philanthropic grants, and institutional legitimacy are built on this reading remaining dominant; their exit from the framing would mean dissolution of the field as currently constituted.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, constrained, global).

% The abstract collective whose survival is the stated stake. Cannot organize, negotiate, or exit; benefits only if alignment work succeeds and only in a counterfactual that can never be directly verified. Has no seat at any table where the resource allocation between this reading and rival readings is decided.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment_success, beneficiary,
    powerless, civilizational, trapped, universal).

% People experiencing documented algorithmic discrimination, wrongful denial of benefits, labor displacement, and misinformation harms today. Research funding, regulatory attention, and public discourse bandwidth spent on speculative extinction scenarios is bandwidth and funding not spent on their documented, present, remediable harms. They bear an opportunity cost imposed by the dominance of this reading in policy and philanthropic circles.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_day_algorithmic_harm_victims, payer,
    powerless, immediate, trapped, national).

% The potentially infinite set of people who would exist absent extinction, and who bear the cost if the extinction-risk framing is wrong about mechanism, timeline, or tractability while consuming the safety community's attention and resources that could have gone to near-term harm reduction or to a different risk model entirely. Structurally unable to corroborate or contest any claim made on their behalf.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_humans_under_uncertainty, payer,
    powerless, civilizational, trapped, universal).

% Labor organizers, civil rights groups, and algorithmic accountability researchers who argue the existential framing displaces attention and funding from documented present harms. They are frequently characterized by existential-risk proponents as addressing a 'lesser priority' and find themselves competing for the same finite regulatory and philanthropic attention that the extinction framing has captured.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_governance_reform_advocates_for_near_term_harms, excluded,
    moderate, biographical, constrained, national).

% Evaluate whether resources allocated under the existential-risk framing produce verifiable risk reduction, and whether the framing's dominance in legislative testimony and funding decisions is proportionate to its evidentiary basis relative to competing risk models.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research effort, funding, and governance attention around the possibility that a sufficiently capable AI system could cause irreversible catastrophic or extinction-level harm, and channels technical work (alignment research, interpretability, capability evaluations, pause/slowdown advocacy) toward reducing that specific tail risk.
% TRANSFER_FUNCTION: Moves philanthropic funding, government regulatory attention, top technical talent, and public discourse salience toward speculative long-horizon technical interventions and away from near-term algorithmic accountability work; also moves reputational and regulatory legitimacy toward the frontier labs who position themselves as the responsible stewards of the risk they are simultaneously creating.
% ABSENT_VOICES: Present-day victims of algorithmic bias, labor displacement, and misinformation harms are rarely in the room when existential-risk funding and policy priorities are set; when they are present, their concerns are frequently reframed as secondary to 'the real risk.' Future humans, by construction, cannot corroborate or contest any claim made in their name.
% DISAPPEARANCE_RATIONALE: Existential-risk proponents argue the world would rearrange catastrophically if this framing and its associated research infrastructure vanished — alignment research would stall, governance attention would evaporate, and genuine extinction-level risk would go unaddressed. Near-term harms advocates argue the world would barely change for the people currently being harmed by deployed systems, or might even improve as attention and funding redirected to documented, remediable problems. Both positions are held by parties with direct stakes in the answer.
% FOUNDING_PROBLEM: The founding problem, as stated by the reading's own proponents: increasingly capable AI systems could develop goals misaligned with human survival, and once a system crosses a capability threshold, correction may become impossible — making pre-emptive technical and governance solutions to alignment a matter of civilizational survival rather than ordinary risk management.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is thin outside the community with direct career and funding stakes in the framing. Some independent AI researchers and philosophers of risk (outside frontier labs and dedicated x-risk institutes) affirm the mechanism (instrumental convergence, orthogonality) is plausible in principle; other independent researchers and historians of technology argue the timeline and tractability claims are unfalsifiable within any policy-relevant horizon and that the framing has been shaped, in part, by the commercial interests of the labs building the systems in question. No corroboration exists from the future humans the framing is built to protect, by construction of the problem itself.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) sits at tangled-rope levels because there IS a genuine coordination function — pooling technical talent and funding around a real (if contested) tail risk — but the coordination rides alongside a substantial transfer: attention, funding, and regulatory legitimacy captured by frontier labs and x-risk institutes at the expense of present-day harm remediation. Suppression (0.42) is moderate: there is no direct coercive suppression of the near-term-harms framing, but there is a structural crowding-out effect operating through funding allocation, media salience, and legislative testimony access. Theater ratio (0.47) reflects that a substantial and growing share of 'safety' activity in frontier labs functions as reputational insulation and regulatory pre-emption rather than verified risk reduction — safety statements, voluntary commitments, and interpretability publications that do not demonstrably reduce deployment risk. Accessibility collapse (0.38) is moderate-low: alternative framings (near-term harms, dual priority) remain visibly and actively contested in the discourse, not foreclosed. Resistance (0.58) is substantial because near-term-harms advocates actively and vocally contest the resource allocation this reading produces.
 *
 * PERSPECTIVAL GAP:
 *   From the frontier lab seat, this reading is functioning coordination: pooling resources against a genuine catastrophic tail risk that only well-resourced technical actors can meaningfully address. From the near-term-harms-advocate seat, the same structure is an attention and funding extraction mechanism that uses an unfalsifiable, civilizationally-scaled harm to out-compete documented, remediable, present harms for scarce policy bandwidth. The engine should compute these as structurally different experiences of the identical arrangement — the payer seats (present_day_algorithmic_harm_victims, future_humans_under_uncertainty) and the agenda-setter seat (frontier_ai_labs) diverge sharply because their power, exit options, and time horizons differ radically, not because either is wrong about what they observe.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs sit near the beneficiary end: they set the technical agenda for what counts as safety work, capture regulatory goodwill from appearing responsible, and can exit specific commitments by citing competitive dynamics — arbitrage-grade exit. Existential-risk research institutes are structural beneficiaries whose institutional survival depends on this reading's continued dominance, but their exit options are more constrained than the labs' since their legitimacy is tied to the framing itself. 'Humanity conditional on alignment success' is technically a beneficiary but is powerless and trapped — it cannot act on its own behalf, so its d approaches the target end despite the beneficiary label; this divergence between role and derived directionality is intentional and is exactly the kind of case an override could correct, though none is applied here because the derivation from powerless+trapped already pushes appropriately high. Present-day harm victims and future humans are targets: they bear the opportunity cost of resources and attention this reading commands, with no meaningful exit — the future humans cannot even be consulted about whether the tradeoff being made in their name is one they would endorse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (potential misalignment catastrophe from superintelligent systems) has NOT been resolved — it remains live and contested rather than dead, which is what keeps this from being classified as a pure zombie mandate. But the founding-problem-status/disappearance-verdict pairing (contested/contested) signals genuine ambiguity rather than either capture or clean coordination: unlike a zombie mandate (dead problem, persisting arrangement) this is a live-but-unverifiable problem whose persisting arrangement commands resources disproportionate to any available corroboration. Tangled Rope classification (rather than snare) is warranted because the coordination function is real and substantial — genuine alignment research has genuine technical content — even as the enforcement of resource priority (via funding gatekeeping, discourse salience, and regulatory access) imposes real costs on a specific, nameable set of present-day victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_nearterm_resource_competition,
    'Do existential-risk and near-term-harms safety work actually compete for the same finite pool of funding, talent, and regulatory attention, or is the pool elastic enough that both can be fully funded without tradeoff?',
    'Track whether philanthropic AI-safety funding and legislative attention have grown fast enough to fund both agendas at the levels each community says it needs, versus whether growth has been captured disproportionately by extinction-risk framed proposals.',
    'If the pool is genuinely elastic, this reading''s classification should move toward rope (pure coordination with no real victim); if the pool is fixed or the extinction framing systematically outcompetes for a fixed pool, tangled_rope or even snare is the more accurate classification for the transfer component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_vs_nearterm_resource_competition, empirical, 'Whether existential and near-term safety work are genuinely in resource competition.').

omega_variable(
    tractability_and_timeline_uncertainty,
    'Is the core mechanism (instrumental convergence leading to misaligned superintelligence causing extinction-level harm) a well-founded near-to-medium-term risk, or is it a speculative claim whose policy-relevant probability is currently unknowable?',
    'Track calibrated forecasting records of AI capability researchers versus x-risk theorists over the interval; track whether any capability threshold identified in advance as dangerous is subsequently crossed with observable warning signs matching predictions.',
    'If tractability is high and timelines are policy-relevant, the beneficiary declaration for ''humanity conditional on alignment success'' and the high ε on speculative interventions are well-grounded. If tractability and timeline are currently unknowable, the resource allocation this reading commands is disproportionate to its evidentiary basis and the classification shifts toward extraction dressed as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tractability_and_timeline_uncertainty, empirical, 'Whether the founding mechanism is a well-founded near-term risk or an unfalsifiable speculative claim.').

omega_variable(
    frontier_lab_capture_of_safety_framing,
    'Is the existential-risk framing''s current dominance in policy and funding circles substantially a product of frontier AI labs'' commercial and reputational interest in being seen as the responsible stewards of a risk only they can address — i.e., is this reading partly a manufactured moat?',
    'Compare the safety framings and funding priorities advocated by researchers and institutions with no commercial stake in frontier AI development against those advocated by frontier-lab-affiliated researchers and institutions.',
    'If the framing tracks commercial interest closely, frontier_ai_labs'' directionality should be overridden further toward the beneficiary extreme and the tangled_rope classification''s extraction component is understated; if the framing is independent of commercial interest, the current directionality derivation is adequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontier_lab_capture_of_safety_framing, conceptual, 'Whether existential-risk framing dominance reflects genuine risk assessment or commercial capture by frontier labs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__existential_risk_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__existential_risk_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__existential_risk_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__existential_risk_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__existential_risk_reading, theater_ratio, 20, 0.47).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__existential_risk_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__existential_risk_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__existential_risk_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__existential_risk_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__existential_risk_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__existential_risk_reading, base_extractiveness, 20, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__existential_risk_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__existential_risk_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__existential_risk_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__existential_risk_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__existential_risk_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__existential_risk_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the natural-language label 'AI safety' along with near_term_harms_reading and dual_priority_reading, per the ε-invariance principle. Each reading has a distinct ε: this reading carries substantially higher ε (0.61) than would be measured for pure algorithmic-accountability interventions because it concentrates resources on speculative, hard-to-verify technical interventions with a diffuse, unfalsifiable victim class. near_term_harms_reading is expected to carry lower ε on its own core interventions (bias audits, labor protections) with a concentrated, verifiable, present-day victim class. dual_priority_reading is expected to sit structurally between the two, or to resolve as a rope if its coordination claim (that the two priorities are genuinely non-competing) holds up against the resource-competition omega above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
