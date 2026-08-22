% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Settlement on Development-Model Choice
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   The pragmatic openness settlement frames software development-model
 *   choice as an instrumental engineering question: open source is preferred
 *   because peer review and collaboration produce better software, while
 *   proprietary development remains a fully legitimate alternative. The
 *   arrangement coordinates a global contributor community, shelters
 *   commercial closed-source producers from ethical condemnation, and routes
 *   donated labor toward a commons whose largest material beneficiaries are
 *   enterprise platform consumers. Its history shows slow drift: extraction
 *   crept upward and performative openness (open-washing) grew as corporate
 *   participation deepened, though both remain low in absolute terms. This
 *   story instantiates ONE reading of the software_control_legitimacy kernel;
 *   the freedom_imperative, property_rights, and commons readings are
 *   separate constraints with their own epsilon values and victim structures,
 *   linked through the network block. Epsilon's referent is the settlement
 *   itself — the standing normative arrangement that frames model choice as
 *   methodology — assessed by this reading's own lights; it is not the
 *   arrangements the sibling readings would endorse or condemn. KEY AGENTS
 *   (by structural relationship): - open_source_contributors: primary
 *   beneficiary cohort (moderate/mobile) — supplies the peer review and
 *   collaboration the quality argument runs on - downstream_software_users:
 *   beneficiary with friction (moderate/constrained) — receives quality at
 *   zero license cost, pays switching costs - proprietary_software_firms:
 *   shielded beneficiary (institutional/arbitrage) — legitimacy protected,
 *   mild reputational pressure - enterprise_platform_consumers: dominant
 *   value capturer (powerful/arbitrage) — largest material share of donated
 *   labor - open_source_stewardship_foundations: agenda setter
 *   (organized/mobile) — polices the open/non-open boundary -
 *   uncompensated_critical_maintainers: excluded seat (powerless/trapped) —
 *   sustainability objection has no place in the methodology frame -
 *   software_engineering_researchers: analytical observer — supplies and
 *   audits the evidence base
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.22).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Settlement on Development-Model Choice").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "economic/technological/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'b2707de7-7709-47ed-887b-d8ba13625593').
narrative_ontology:cs_kernel_codification('b2707de7-7709-47ed-887b-d8ba13625593', distributed).
narrative_ontology:cs_authority_grounding('b2707de7-7709-47ed-887b-d8ba13625593', expertise).
narrative_ontology:cs_interpretation_layer_present('b2707de7-7709-47ed-887b-d8ba13625593').
narrative_ontology:cs_reading_relation('b2707de7-7709-47ed-887b-d8ba13625593', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2707de7-7709-47ed-887b-d8ba13625593', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2707de7-7709-47ed-887b-d8ba13625593', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('b2707de7-7709-47ed-887b-d8ba13625593', foundational, development_model_is_instrumental_choice).
narrative_ontology:cs_axiom_status(development_model_is_instrumental_choice, holdable).
narrative_ontology:cs_axiom_grounding('b2707de7-7709-47ed-887b-d8ba13625593', development_model_is_instrumental_choice, instrumental).
narrative_ontology:cs_axiom('b2707de7-7709-47ed-887b-d8ba13625593', foundational, open_source_quality_advantage_empirical).
narrative_ontology:cs_axiom_status(open_source_quality_advantage_empirical, holdable).
narrative_ontology:cs_axiom_grounding('b2707de7-7709-47ed-887b-d8ba13625593', open_source_quality_advantage_empirical, empirically_contingent).
narrative_ontology:cs_reference_frame('b2707de7-7709-47ed-887b-d8ba13625593', efficacy_based_model_pluralism).
narrative_ontology:cs_drift_state('b2707de7-7709-47ed-887b-d8ba13625593', post_cloud_relicensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b2707de7-7709-47ed-887b-d8ba13625593', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, downstream_software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_firms).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, enterprise_platform_consumers).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, peer_review_quality_hypothesis).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, methodology_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer and professionally-assigned developers who publish code under open licenses and review each other's work. They gain reputation, portable skill, and collaboration networks; they can stop contributing or move between projects and employers with little friction, and many convert community standing into employment.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and organizations that run open-source software in production. They receive peer-reviewed code at zero license cost and may inspect or fork it, but switching stacks carries real migration and retraining costs, so their practical choice set is bounded by ecosystem gravity even where licenses impose nothing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, downstream_software_users, beneficiary,
    moderate, biographical, constrained, global).

% Companies that develop and sell closed-source products. The settlement protects their standing: choosing a proprietary model is framed as a legitimate methodology decision rather than an ethical failing. They bear mild reputational pressure to open selected components and can relicense, adopt open-core structures, or acquire open projects whenever the balance shifts.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Hyperscale cloud and platform companies that build paid services on donated open-source infrastructure. They contribute selectively, capture the largest material share of the commons' value, and can fork, fund, or abandon upstream projects at will; their take-without-reciprocity pattern is the visible strain on the settlement.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, enterprise_platform_consumers, beneficiary,
    powerful, generational, arbitrage, global).

% License-definition bodies and project foundations that maintain the open-source definition, approve licenses, hold trademarks, and referee which projects count as open. They administer the norm's boundary but command no enforcement arm beyond trademark control and community standing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_stewardship_foundations, agenda_setter,
    organized, generational, mobile, global).

% Individual maintainers of widely depended-upon packages who carry security and compatibility work without funding. The methodology debate adjudicates which model builds better software and never reaches their question — who pays for upkeep — so their sustainability objection has no seat; personal responsibility and bus-factor guilt keep them in place.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, uncompensated_critical_maintainers, excluded,
    powerless, biographical, trapped, global).

% Academic and industrial research groups studying defect rates, productivity, and community health across development models. They supply the evidence base the quality argument rests on and document reciprocity gaps, holding no stake in either model's commercial fortunes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, enterprise_platform_consumers).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels development-model decisions onto a shared evaluative surface: models compete on evidenced quality outcomes, letting contributors, users, and firms coordinate expectations about which practices earn trust and contribution, without mandating any single model.
% TRANSFER_FUNCTION: Moves reputational standing and collaborative labor toward open-source participation, and moves legitimacy cover toward proprietary choices; value produced in the peer-review commons flows to all adopters, with the largest material share accruing to enterprises that build on donated code.
% ABSENT_VOICES: Uncompensated critical-infrastructure maintainers sit outside the conversation: the debate adjudicates which model produces better software, not who pays for sustaining it, so their sustainability objection has no seat. Non-technical end users affected by dependency and telemetry decisions likewise rarely enter the frame.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, model choice would revert to a raw contest between the freedom imperative and property-rights framings; hiring signals, procurement criteria, and contribution flows would scramble as firms lost the legitimacy shield and contributors lost the efficacy-based rationale for donating labor; the stewardship bodies' boundary role would lose its referent.
% FOUNDING_PROBLEM: In the late 1990s, free software's moral framing limited business adoption: firms would not accept the freedom imperative's premises, and shared development lacked a rationale compatible with commercial models. The pragmatic openness settlement was built to solve that adoption problem by recasting shared development as an engineering-efficacy question.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary histories of the 1998 open-source reframing corroborate the adoption-problem genealogy from outside any single benefiting party, and engineering-management and procurement records attest that firms engaged once the framing shifted from ethics to efficacy. The freedom-imperative camp attests the same genealogy with inverted valence — the problem was 'solved' by diluting the ethical claim — which corroborates the founding problem while disputing the settlement's worth.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (epsilon 0.22 at interval end) because the settlement coerces no one: contribution is voluntary, the proprietary path remains fully legitimate, and the norm's costs are reputational rather than material. Suppression is correspondingly low (0.12) — the constraint operates through community preference and hiring/procurement signaling, not enforcement machinery; what suppression exists is partly internalized (contribution expectations absorbed as professional obligation) rather than structural. Accessibility collapse is low (0.25): understanding the norm does not close alternatives — choosing closed development carries no sanction under this reading, the profile opposite of a mountain or snare. Resistance is moderate (0.40): the settlement is flanked by freedom-imperative critics who read it as ethical capitulation and property-rights defenders who read its quality preference as stigma against commercial models. Theater (0.30 at end, up from 0.06) tracks open-washing: branding activity that performs openness without reciprocal practice grew with corporate participation. Both temporal series run on one shared eight-point grid spanning the settlement's life (t0 approximates the 1998 open-source reframing; t28 approximates the current AI-era release debates), and the end-state values match the base_properties scalars. Claim and metrics are independently authored: rope is claimed from structure — broad net benefit, no victim set, unsuppressed alternatives — and the metrics independently describe low-extraction operation; neither was tuned to the other.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the contributor position the settlement is opportunity-rich coordination: reputation, skill, and employment flow from participation, and exit is cheap. From the proprietary-firm position the same arrangement is a legitimacy shield that ends an era of ethical condemnation, purchased with mild reputational pressure. From the downstream-user position it is quality without license fees but inside ecosystem gravity that limits practical exit. From the excluded maintainer position the entire conversation misses the labor question — the frame debates model superiority while upkeep goes unfunded. The engine computes per-seat classifications from power, exit, and role data; these divergences are outputs of the structural data, not authored claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party is declared a beneficiary, so derived directionality sits near the beneficiary end for all of them and no full-target seat exists — there is no victim set to amplify effective extraction. The mild extraction the measurements register flows through voluntary-contribution asymmetry rather than through any seat's structural position: enterprise_platform_consumers capture the largest material share of donated labor (recorded on the receipt surface as gain_flow), yet under this reading's own lights they are beneficiaries of the commons, not its targets. Downstream users carry switching-cost friction (exit: constrained), which nudges their derived directionality slightly above the pure-beneficiary end relative to mobile contributors. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bridging ideological and commercial software production so both could engage — remains live: each entrant wave (enterprise, cloud, AI labs) re-raises it, so the R5 mismatch consumer finds status=live alongside verdict=world_rearranges, a coherent pair with no zombie flag. Mandatrophy discipline cuts both ways here. It blocks the freedom-imperative temptation to read the settlement as a snare laundering proprietary extraction: most contributor flows are voluntary and repaid in reputation, skill, and employment, and no seat is coerced. It equally blocks the temptation to naturalize the settlement as a mountain: emerges_naturally is false, and the arrangement is a maintained human settlement whose boundary the stewardship bodies actively police. The reciprocity-decay omega tracks the one pathway by which this rope could degrade toward tangled_rope — if the take-without-give-back asymmetry proves intrinsic to the structure rather than imported by participant conduct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the software_control_legitimacy kernel (pragmatic_openness_reading); which reading governs a given dispute, and how does classification change across the sibling readings?',
    'Identify the governing frame from the disputants'' actual commitments: freedom_imperative_reading instantiates a constraint whose target set includes proprietary software firms and their customers; property_rights_reading instantiates one where copyleft obligations and license enforcement are the contested extraction; commons_reading adds governance victims among competing users of shared infrastructure. Each sibling is a separate constraint file; classification follows the file whose reading the dispute invokes.',
    'Under freedom_imperative, epsilon over the proprietary arrangement is high and a victim set appears; under property_rights, open-source license enforcement becomes the extractive surface; under this reading both models are legitimate, no victims exist, and epsilon stays low — identical behavioral facts classify differently depending on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: this story is one of four readings; siblings are distinct constraints with different victim sets and epsilon values.').

omega_variable(
    quality_advantage_evidence_stability,
    'Does the peer-review quality advantage that anchors the settlement''s preference leg survive current and future evidence?',
    'Matched-cohort meta-analyses comparing defect density, security response time, and productivity across open and closed development of comparable software.',
    'Weakening evidence collapses the preference leg toward neutral pluralism and drifts this reading toward convergence with property_rights; strengthening evidence entrenches the rope and raises the reputational transfer''s efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_advantage_evidence_stability, empirical, 'The preference half of the settlement rests on a falsifiable empirical claim; its stability governs the constraint''s future shape.').

omega_variable(
    reciprocity_decay_attribution,
    'Is the measured rise in extractiveness intrinsic to the settlement, or imported by platform free-riding that the settlement itself does not mandate?',
    'Contribution-flow accounting by actor class across the interval, with counterfactual comparison of projects that have funded versus unfunded maintenance.',
    'If intrinsic, the rope degrades toward tangled_rope with enterprise_platform_consumers as the seat on the paying side of the same structure; if imported, the norm itself remains rope and remedies target participant conduct rather than the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_decay_attribution, empirical, 'Attribution question for the rising extraction trajectory: norm failure versus participant conduct.').

omega_variable(
    agenda_setting_location,
    'Is the settlement administered by the stewardship foundations that nominally police its boundary, or de facto by employer platforms whose contribution policies shape practice?',
    'Trace where boundary disputes are actually decided: license approvals, project expulsions, and trademark actions versus platform-level contribution-policy changes that alter practice without any foundation ruling.',
    'If platforms are the operative agenda setters, the foundations'' agenda_setter seat is nominal, the mild capture reading strengthens, and effective extraction sits higher than the declared structure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_setting_location, conceptual, 'Framing under-determination in the CS layer: which institution actually adjudicates the kernel''s boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t4, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement_basis(soft_tr_t4, observed).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(soft_tr_t8, observed).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(soft_tr_t12, observed).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(soft_tr_t16, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(soft_tr_t24, observed).
narrative_ontology:measurement(soft_tr_t28, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement_basis(soft_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t4, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement_basis(soft_be_t4, observed).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement_basis(soft_be_t8, observed).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement_basis(soft_be_t12, observed).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(soft_be_t16, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement_basis(soft_be_t24, observed).
narrative_ontology:measurement(soft_be_t28, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 28, 0.22).
narrative_ontology:measurement_basis(soft_be_t28, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, identity_coordination).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, commons_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'software control legitimacy' decomposes, per the epsilon-invariance principle, into four structurally distinct readings that cannot share one story — they assign different victim sets and different epsilon values to the same behavioral facts. This file is the pragmatic_openness member (low epsilon, no victims, both models legitimate). freedom_imperative_reading and property_rights_reading instantiate constraints with opposed target sets; commons_reading governs the infrastructure this settlement accumulated. The upstream/downstream structure runs from this reading to commons_reading: the pragmatic settlement's success built the shared infrastructure whose governance the commons reading addresses, which is why the reading_relations edge to commons_reading is 'influences' while the edges to the two ideological siblings are 'coexists_with'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
