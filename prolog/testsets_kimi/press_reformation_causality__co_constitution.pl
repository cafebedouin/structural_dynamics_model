% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Press-Reformation Co-Constitution Feedback Economy
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   The constraint models the historical arrangement in which printing
 *   technology and Reformation religious controversy entered a mutually
 *   amplifying feedback loop during the sixteenth century. Printers profited
 *   from controversial pamphlets; reformers gained unprecedented geographic
 *   reach; secular magnates extracted political autonomy from a weakened
 *   Church; and the Catholic hierarchy lost its interpretive monopoly. The
 *   co-constitution reading treats technology as scaffold infrastructure and
 *   the social-technical feedback as a distributed tangled rope with no
 *   single capturer of gains. This is one reading of the press-reformation
 *   causality kernel; it coexists with technological determinism and
 *   influences the strategic deployment reading by adding structural feedback
 *   constraints to intentional actor models.
 *
 * KEY AGENTS:
 *   - vernacular_printers: Primary agenda-setter (moderate/constrained) â controls print output and profits from controversy
 *   - reformist_clergy: Primary beneficiary (organized/constrained) â gains dissemination but locked into print economy
 *   - catholic_hierarchy: Primary payer/victim (institutional/constrained) â loses interpretive monopoly and diverts resources to counter-pamphleteering
 *   - secular_magnates: Secondary agenda-setter (powerful/mobile) â licenses printing to extract political autonomy
 *   - urban_lay_readers: Secondary beneficiary (moderate/constrained) â gains access but absorbs propaganda costs
 *   - rural_peasantry: Excluded payer (powerless/trapped) â bears social and military costs without access to the print sphere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.62).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.48).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Press-Reformation Co-Constitution Feedback Economy").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history/technology/religion").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, 'dde79f92-9c02-4fba-9357-cf33d6385c34').
narrative_ontology:cs_kernel_codification('dde79f92-9c02-4fba-9357-cf33d6385c34', distributed).
narrative_ontology:cs_authority_grounding('dde79f92-9c02-4fba-9357-cf33d6385c34', distributed).
narrative_ontology:cs_reading_relation('dde79f92-9c02-4fba-9357-cf33d6385c34', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('dde79f92-9c02-4fba-9357-cf33d6385c34', press_reformation_causality__strategic_deployment, influences).
narrative_ontology:cs_axiom('dde79f92-9c02-4fba-9357-cf33d6385c34', foundational, agency_technology_mutual_constitution).
narrative_ontology:cs_axiom_status(agency_technology_mutual_constitution, holdable).
narrative_ontology:cs_axiom_grounding('dde79f92-9c02-4fba-9357-cf33d6385c34', agency_technology_mutual_constitution, empirically_contingent).
narrative_ontology:cs_axiom('dde79f92-9c02-4fba-9357-cf33d6385c34', secondary, print_religion_feedback_loop).
narrative_ontology:cs_axiom_status(print_religion_feedback_loop, holdable).
narrative_ontology:cs_axiom_grounding('dde79f92-9c02-4fba-9357-cf33d6385c34', print_religion_feedback_loop, empirically_contingent).
narrative_ontology:cs_reference_frame('dde79f92-9c02-4fba-9357-cf33d6385c34', reciprocal_media_agency).
narrative_ontology:cs_drift_state('dde79f92-9c02-4fba-9357-cf33d6385c34', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dde79f92-9c02-4fba-9357-cf33d6385c34', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformist_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, secular_magnates).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, urban_lay_readers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, rural_peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owned presses and type, organized in guilds. Chose which pamphlets and bibles to print based on market demand and political safety. Profited from the surge in controversial religious literature but faced legal risk and confessional competition. Could not easily exit because capital was sunk in equipment and skills.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_printers, agenda_setter,
    moderate, biographical, constrained, regional).

% Authored theological pamphlets and vernacular scripture translations that depended on the print channel for reach. Gained massive audiences but became locked into the pamphlet economy â their authority and fundraising became tied to continuous printed output. Could not return to manuscript dissemination without losing their movement.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformist_clergy, beneficiary,
    organized, generational, constrained, continental).

% Lost the monopoly on sacred interpretation and the material control of text production. Was forced to engage in costly counter-pamphleteering and censorship infrastructure. Could not exit the print arena without ceding the propaganda war, but entering it required massive resource diversion and legitimized the vernacular public sphere.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% Princes and city councils regulated or protected printing within their territories. Used the print-reformation feedback to consolidate political autonomy from Rome and the Empire. Could choose confessional alignment and licensing policy, but once chosen, were constrained by the polemical economy they had licensed.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, secular_magnates, agenda_setter,
    powerful, generational, mobile, regional).

% Gained unprecedented access to vernacular scripture and political argument. Formed the demand side of the print economy, purchasing pamphlets and participating in communal reading. Their interpretive autonomy expanded but they were also subjected to information overload, propaganda, and the social costs of confessional polarization.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, urban_lay_readers, beneficiary,
    moderate, biographical, constrained, regional).

% Mostly illiterate, excluded from the print public sphere, yet directly affected by the social unrest and warfare that the print-reformation feedback loop incited. Their grievances were appropriated by printed manifestos they could not read, and they paid the military and fiscal costs of confessional conflict.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, rural_peasantry, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the collective-action problem of disseminating dissenting theological views and organizing geographically dispersed opposition to a centralized ecclesiastical interpretive monopoly across linguistic and political boundaries.
% TRANSFER_FUNCTION: Moved wealth from urban buyers and institutional patrons to printers and reformist authors; moved epistemic authority from the Catholic hierarchy to distributed vernacular readership and print markets; moved political legitimacy from transnational Church institutions to territorial magnates.
% ABSENT_VOICES: The illiterate rural peasantry, women in enclosed religious orders, and dissenters within the reform movements who lacked access to presses or patronage. They were affected by the theological and social conflicts but excluded from the print public sphere and its feedback economy.
% DISAPPEARANCE_RATIONALE: If the co-constitution feedback loop between print economy and religious controversy had not formed, the Reformation would have remained a localized academic dispute or manuscript-only movement. The European confessional map, the authority of territorial princes, the economy of sacred interpretation, and the infrastructure of public discourse would not have taken their historical forms.
% FOUNDING_PROBLEM: The late medieval Church maintained a monopoly on sacred interpretation and the manuscript economy created severe bottlenecks in the production and geographic dissemination of dissenting theological views, limiting the scale of religious reform movements.
% FOUNDING_PROBLEM_CORROBORATION: Modern Reformation historians in the STS and Annales traditions, and media historians outside the theological beneficiary camps, corroborate that manuscript production was a genuine bottleneck. Catholic revisionist historians acknowledge the bottleneck but dispute whether the print-reformation co-constitution was the necessary or proportionate solution; they attest the founding problem was solvable through conciliar reform rather than disruptive print economics.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the feedback loop created durable extraction of authority from the Church and of wealth from urban readers, distributed across printers and political actors. Suppression (0.48) is moderate: the loop persisted through a mix of censorship, guild controls, and market incentives rather than pure coercion. Theater ratio (0.48) reflects that by mid-century, confessional polemics had become stylized and performative even as they retained genuine coordination function. Accessibility collapse (0.40) is moderate because manuscript alternatives persisted for decades and oral culture remained strong. Resistance (0.72) is high because the Church and imperial authorities mounted sustained counter-measures. The measurement series show extraction and theater rising as the feedback loop matured, then stabilizing during confessionalization.
 *
 * PERSPECTIVAL GAP:
 *   The printer and princely seats compute as beneficiaries with low directionality because they controlled the means of production and licensing; the Catholic hierarchy and rural peasantry compute as targets with high directionality because they bore costs without controlling the loop. The reformist clergy sits ambiguously â they appear as beneficiaries in the base_properties but their constrained exit and dependence on continuous print output means their effective extraction is higher than a pure beneficiary reading would suggest. The engine should compute seat divergence between the agenda-setting printers and the locked-in reformers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to agents who gained from the loop's operation: printers (profits), reformers (reach), magnates (autonomy), readers (access). Victim declarations map to agents who bore uncompensated costs: the Church (lost monopoly), the peasantry (unrest and war). The reformist clergy's situation is the key ambiguous seat: they are declared beneficiaries because their movement required print, but their exit_options=constrained means the engine will compute a directionality closer to symmetric or target than a pure beneficiary. No override is needed because this ambiguity is structurally captured.
 *
 * MANDATROPHY ANALYSIS:
 *   The co-constitution reading prevents mislabeling the arrangement as pure extraction (snare) by insisting on the genuine coordination function print served for reform movements and readers. It prevents mislabeling as pure coordination (rope) by naming the Church and peasantry as victims of the same loop. The founding problem (manuscript bottleneck) is dead, and the constraint persists beyond its solving into confessional polemics and printer profit â mandatrophy resolved would be true if we treated it as a scaffold, but the tangled rope dynamics layered onto the scaffold mean the overall system drifted from its founding function. However, because multiple parties continue to benefit (printers, princes, confessional states), it is not a piton â there is no inertial maintenance without beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_constitution_asymmetry,
    'Does the causal arrow between printing technology and Reformation agency run symmetrically, or did one side of the feedback loop dominate in specific phases?',
    'Phase-resolved bibliometric analysis of print output against reformist political milestones, weighted by economic data on printer profitability versus reformist funding sources.',
    'If the press economics consistently led reformist political milestones, the co-constitution reading collapses toward technological determinism in its stronger moments; if reformist political events consistently led print output spikes, it collapses toward strategic deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_asymmetry, empirical, 'Directional asymmetry in the press-reformation feedback loop').

omega_variable(
    distributed_extraction_site,
    'In a system with no single beneficiary, where does the extracted surplus of the co-constitution loop actually accumulate â economic profit, political autonomy, or epistemic authority?',
    'Multi-ledger analysis tracking printer profit margins, princely tax revenues from confiscated Church lands, and the symbolic capital of interpretive authority across the interval.',
    'If extraction concentrates in printer profits, the loop is a market snare; if it concentrates in princely political capital, it is a political scaffold that atrophied into extraction; if genuinely diffuse, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_extraction_site, conceptual, 'Identification of extraction accumulation site in distributed system').

omega_variable(
    suppression_mechanism_nature,
    'Was the persistence of the co-constitution loop driven by structural suppression (censorship, guild monopoly) or by internalized confessional identity fusion that made exit unthinkable?',
    'Comparative analysis of print output in jurisdictions with different censorship regimes versus jurisdictions with strong confessional identity formation.',
    'If suppression was primarily structural, the constraint''s extractiveness was externally enforced; if internalized, the constraint operated as identity_coordination with higher effective extraction than structural measures indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, empirical, 'Structural versus internalized suppression in confessional print economy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prc_cc_tr_t0, press_reformation_causality__co_constitution, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prc_cc_tr_t25, press_reformation_causality__co_constitution, theater_ratio, 25, 0.45).
narrative_ontology:measurement(prc_cc_tr_t50, press_reformation_causality__co_constitution, theater_ratio, 50, 0.55).
narrative_ontology:measurement(prc_cc_tr_t75, press_reformation_causality__co_constitution, theater_ratio, 75, 0.5).
narrative_ontology:measurement(prc_cc_tr_t100, press_reformation_causality__co_constitution, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(prc_cc_be_t0, press_reformation_causality__co_constitution, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prc_cc_be_t25, press_reformation_causality__co_constitution, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(prc_cc_be_t50, press_reformation_causality__co_constitution, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(prc_cc_be_t75, press_reformation_causality__co_constitution, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(prc_cc_be_t100, press_reformation_causality__co_constitution, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prc_cc_su_t0, press_reformation_causality__co_constitution, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prc_cc_su_t25, press_reformation_causality__co_constitution, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(prc_cc_su_t50, press_reformation_causality__co_constitution, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(prc_cc_su_t75, press_reformation_causality__co_constitution, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(prc_cc_su_t100, press_reformation_causality__co_constitution, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
