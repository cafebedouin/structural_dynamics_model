% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: Safety-Control Reading of the AI Alignment Commitment
 *   domain: technological/governance/ethical
 *
 * SUMMARY:
 *   Across roughly two decades a definitional regime consolidated around the
 *   claim that 'alignment' in AI means preventing catastrophic loss of
 *   control over increasingly capable systems. The regime is administered
 *   from frontier labs' internal safety organizations, dedicated research
 *   institutes, and the funding and media networks surrounding them: it sets
 *   what counts as alignment work, which hazards register as
 *   alignment-relevant, and whose concerns fall outside the term's
 *   jurisdiction. Its operation has a genuine coordination core — a shared
 *   hazard definition makes safety claims comparable across labs, enables
 *   pooled evaluation infrastructure, and gives a constituency-less tail risk
 *   an institutional home. Its operation also carries a persistent asymmetry:
 *   resources that would otherwise mitigate present-day deployment harms
 *   (discriminatory screening, surveillance error, misinformation at scale)
 *   are redirected to programs targeting hypothesized future systems; rival
 *   definitions are gated out of flagship venues and funding lines; and the
 *   frame's protective promise toward future generations remains unverifiable
 *   while its legitimation function operates immediately. This story authors
 *   the standing arrangement this reading instantiates and governs: epsilon's
 *   referent is that arrangement itself, and the frame's own accounting
 *   concedes the displaced mitigation as deliberate triage — the metrics
 *   record the magnitude of that displacement, which even sympathetic holders
 *   acknowledge is large. Claim and metrics are authored independently: the
 *   claimed type states my structural judgment; the engine computes per-seat
 *   classifications from the structural data and may diverge from it. KEY
 *   AGENTS (by structural relationship): - frontier_ai_labs: primary agenda
 *   setter and principal recipient of the frame's gains
 *   (institutional/arbitrage) — hosts the definition, absorbs its resources -
 *   ai_safety_control_researchers: concentrated beneficiary
 *   (organized/identity_locked) — careers, funding, and self-concept bound to
 *   the frame - xrisk_advocacy_networks: beneficiary
 *   (organized/identity_locked) — programmatic existence depends on the
 *   hazard staying central - capability_race_capital: indirect beneficiary
 *   (powerful/arbitrage) — the frame shields scaling narratives from
 *   present-harm regulation - present_day_harmed_communities: primary target
 *   (powerless/trapped) — bear deployment harms whose mitigation the frame
 *   displaces - fairness_ethics_researchers: secondary target
 *   (moderate/constrained) — lose the defining label, venues, and funding
 *   stream - future_generations: dual-positioned target and claimed
 *   beneficiary (powerless/trapped, universal scope) — carry the frame's
 *   opportunity cost while receiving its unverifiable protection pledge -
 *   legislative_regulators: adjudicating observer (institutional/analytical)
 *   — can legislate around the definitional dispute -
 *   global_south_civil_society: excluded voice (powerless/constrained) —
 *   bears the frame's opportunity costs with no seat in its forums -
 *   science_technology_studies_observers: analytical observer
 *   (moderate/analytical) — documents the frame's movement without stake
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.72).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.62).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "Safety-Control Reading of the AI Alignment Commitment").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "technological/governance/ethical").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, 'e40422d1-2d0f-4b16-b3bc-e6d938d1317d').
narrative_ontology:cs_kernel_codification('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', formalized).
narrative_ontology:cs_authority_grounding('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', extraction).
narrative_ontology:cs_interpretation_layer_present('e40422d1-2d0f-4b16-b3bc-e6d938d1317d').
narrative_ontology:cs_reading_relation('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', foundational, catastrophic_risk_categorical_priority).
narrative_ontology:cs_axiom_status(catastrophic_risk_categorical_priority, holdable).
narrative_ontology:cs_axiom_grounding('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', catastrophic_risk_categorical_priority, empirically_contingent).
narrative_ontology:cs_axiom('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', secondary, present_harms_subordinated_to_tail_risk).
narrative_ontology:cs_axiom_status(present_harms_subordinated_to_tail_risk, holdable).
narrative_ontology:cs_axiom_grounding('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', present_harms_subordinated_to_tail_risk, instrumental).
narrative_ontology:cs_reference_frame('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', control_centric_alignment_doctrine).
narrative_ontology:cs_drift_state('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', contemporary_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e40422d1-2d0f-4b16-b3bc-e6d938d1317d', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, ai_safety_control_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, xrisk_advocacy_networks).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, capability_race_capital).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_day_harmed_communities).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, fairness_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, future_generations).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, instrumental_convergence_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy the most capable AI systems; host and fund the largest safety-research groups; define through safety frameworks, evaluation publications, and policy teams what counts as credible alignment work. Because the governing definition centers loss of control, their internal safety investments purchase both research progress and public standing. They can shift operations across jurisdictions, revise framing with leadership changes, and lobby legislation directly; stepping out of the definitional game entirely would cede a market where certification of responsibility is a purchasing criterion for governments and enterprise customers.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% Work on interpretability, evaluations, and control methods at labs and dedicated institutes; receive the bulk of philanthropic and corporate safety funding, prestigious publication venues, and media attention that follow the loss-of-control framing. Career paths, citation networks, and hiring pipelines run through the frame's institutions; senior figures describe their work in civilizational terms. Moving to adjacent fields means rebuilding professional standing and, for many, abandoning a self-concept built around averting a specific kind of catastrophe.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_safety_control_researchers, beneficiary,
    organized, biographical, identity_locked, global).

% Run fellowships, grant programs, lobbying campaigns, and public-letter efforts premised on catastrophic loss of control as the governing AI hazard. Their funding, membership, and policy access depend on that hazard remaining the frame's centerpiece; several organizations grew from movements whose members organize their civic identity around the scenario. Refocusing on present-day harms would dissolve much of their programmatic identity.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, xrisk_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).

% Invest in frontier capability companies whose valuations assume continued rapid scaling. A governing definition centered on loss of control lets portfolio companies present scaling-with-safeguards as the responsible posture, blunting regulatory challenges keyed to harms occurring now. Capital moves across sectors and jurisdictions quickly if the framing turns hostile, so exposure to the frame's fortunes is a chosen position, not a trap.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, capability_race_capital, beneficiary,
    powerful, immediate, arbitrage, global).

% Experience the technology's current failures directly: discriminatory lending and hiring screens, wrongful surveillance flags, fabricated outputs feeding decisions about them. Mitigation for these harms competes for funding, talent, and legislative attention against programs aimed at hypothesized future systems. Most cannot opt out of the systems making decisions about them; organizing capacity is thin and invitations into the forums where priorities are set are rare.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_day_harmed_communities, payer,
    powerless, immediate, trapped, global).

% Study bias, documentation, and accountability in deployed systems, and watch the field's defining label migrate to a problem set that excludes theirs. Position papers treating fairness work as peripheral to real alignment problems cite past them out of flagship venues; grant calls specify safety scope; hiring committees follow the dominant agenda. They retain their own venues and can retitle their work, but the field's center of gravity, funding, and press follow the prevailing definition.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, fairness_ethics_researchers, payer,
    moderate, biographical, constrained, global).

% Do not yet exist to hold any seat, yet carry the stakes both camps invoke: the balance struck between present-harm mitigation and control research determines which risks arrive unmitigated. The governing definition promises them protection from loss-of-control outcomes while consuming resources that present-harm mitigation would direct elsewhere; whether they net gain or net lose depends on hazard probabilities no one can currently establish. They cannot exit, consent, or contest any of it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, future_generations, beneficiary).

% Draft and enforce AI legislation; receive testimony weighted toward catastrophic scenarios from lab-affiliated witnesses while affected communities and fairness researchers testify less often. They commission their own analyses, compare jurisdictions, and can legislate present-harm requirements regardless of how alignment is defined. Their seat lets them adjudicate the definitional dispute rather than merely inhabit it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, legislative_regulators, observer,
    institutional, generational, analytical, national).

% Communities and organizations outside North America and Europe bearing data extraction, cultural flattening by large models, and deployment harms with the least recourse. Rarely invited to frontier safety summits or catastrophic-risk convenings where agendas are drafted. Would argue that diverting mitigation resources compounds inequities the technology already encodes. Organizing exists, but access to the rooms where the definition is negotiated does not.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, global_south_civil_society, excluded,
    powerless, generational, constrained, continental).

% Scholars of science and technology studies and policy documenting how the alignment label's meaning moved, which communities gained standing as it did, and which lost it. Hold no stake in either definitional camp; publish analyses both camps read selectively.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, science_technology_studies_observers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed research effort, evaluation infrastructure, and policy attention around a shared hazard definition. Without a fixed meaning for 'alignment', safety claims become incommensurable across labs, evaluations cannot be compared or pooled, and funders cannot aggregate behind any protective program; the definitional fixation solves that aggregation problem.
% TRANSFER_FUNCTION: Moves research funding, talent, media attention, and legislative agenda space away from mitigation of present-day deployment harms toward control-focused programs hosted by frontier labs and allied institutes; moves definitional authority over the term 'alignment' from dispersed academic communities to lab-adjacent safety institutions.
% ABSENT_VOICES: Present-day harmed communities and Global South civil society are largely absent from the summits, funding panels, and hearings where alignment priorities are set; fairness researchers attend marginally. Seated at the table, they would contest the priority ordering that assigns their harms residual status. Their absence means the frame's apparent consensus is partly an artifact of room composition.
% DISAPPEARANCE_RATIONALE: If the definition lost its grip overnight, funding calls would reopen to present-harm mitigation, fairness research would regain flagship-venue standing, catastrophic-risk institutions would reorganize around broader hazard portfolios, and labs would lose the ability to certify themselves responsible while scaling. The research economy would reorder around whichever hazards command independent consent rather than definitional custody.
% FOUNDING_PROBLEM: Early in the capability race, researchers recognized that sufficiently capable systems might pursue objectives detached from operator intent faster than oversight could correct — a hazard class with no market constituency and no natural institution to address it. This reading was founded to make that hazard governable: to give it a name, an agenda, and institutions.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists for the founding hazard itself: senior researchers publishing risk statements after leaving industry employment, state-run evaluation institutes, and international scientific panels all attest loss-of-control risk as a real object of concern, from seats outside the benefiting parties. What no party outside the beneficiary set attests is that THIS definitional arrangement — rather than mandated external evaluation or diversified mitigation — is the right response: regulators and fairness-community statements originating outside the frame treat its priority ordering as precisely the contested element.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.72 because the displacement is large and continuous: safety-scoped funding calls, lab absorption of fairness-adjacent talent, and legislative testimony dominated by catastrophic scenarios steadily move resources away from mitigation of harms materializing now. It is not authored higher because the coordination core is genuine — shared evaluations and a common hazard taxonomy carry real protective value, and some displaced attention purchases durable safety knowledge. Suppression (0.62) is structural in the main: the frame holds by venue editorial policy, grant scope-locking, hiring-pipeline composition, and testimony selection rather than coercive force; roughly seven-tenths structural, three-tenths internalized definitional loyalty that would outlast open gates (see omega suppression_structural_vs_internalized). Theater (0.42) reflects a growing legitimation share — alignment pledges, voluntary-framework announcements, and evaluation reporting whose primary audience is regulators and press — alongside technical work with standalone value. Accessibility collapse (0.55) is partial: rival framings survive in their own venues and an integrative position circulates, but the flagship label, funding streams, and press attention collapse onto the dominant definition once its grip is understood. Resistance (0.58) is sustained — fairness-community statements, present-harm-centered legislation in several jurisdictions, dissent from deployed-harm specialists — but outmatched in resources and definitional authority. The measurement series run on one shared seven-point grid (interval units map to years 2000-2024 at four-year steps); all three tracked metrics rise monotonically, so no cyclical pattern is claimed: post-incident surges in catastrophic framing exist in the record but are smoothed in these period judgments, and the oscillation-as-extraction-mechanism analysis does not apply. The suppression_requirement series is authored deliberately despite the static-picture rule, because the story's narrative specifically tracks enforcement-capacity buildup: the enforcement machinery intensified as rival framings gained traction late in the interval, an enforcement ratchet rather than stable suppression.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergent types from the same structure. From the agenda-setter seat the arrangement is a necessary triage it runs and staffs: stakes asymmetry justifies displacement, and enforcement reads as quality control. From the payer seats the same arrangement is a definitional monopoly that renders their concerns out-of-scope by fiat: harmed communities experience it as their mitigation losing every budget cycle; fairness researchers experience it as their field's defining label migrating away. Beneficiaries diverge among themselves: identity-locked beneficiaries (researchers, advocacy networks) experience deep mission coherence; mobile beneficiaries (capital) experience the frame as cheap insurance on scaling narratives. Inter-institutional dynamics: labs and regulators hold similar formal power but opposite exit profiles — labs arbitrage jurisdictions while regulators adjudicate from fixed seats — so one experiences the frame as an asset to manage and the other as a claim to audit. Same-level lateral dynamics: fairness and control researchers hold comparable academic standing, yet differ sharply in constraint-specific access; definitional ownership, not rank, differentiates their experience of the same field.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. frontier_ai_laps... frontier_ai_labs (beneficiary, arbitrage-grade exit) sits nearest the beneficiary pole: the frame subsidizes its social license and regulatory position, and exit is cheap. ai_safety_control_researchers and xrisk_advocacy_networks are beneficiaries whose identity_locked exit does not raise their own extraction but stabilizes the frame's enforcement at low cost — locked defenders maintain it unpaid, which is why suppression is structural rather than costly-coercive. capability_race_capital (beneficiary, arbitrage) likewise sits near the subsidized pole. present_day_harmed_communities (payer, trapped) and fairness_ethics_researchers (payer, constrained) sit near the full-target pole: the frame taxes their mitigation stream and their standing respectively, and neither can arbitrage away. future_generations are authored dual-positioned — payer with secondary beneficiary standing — receiving the frame's protection pledge (subsidy-side pull) while bearing its opportunity cost (target-side push); their trapped, universal-scope condition places them near the target pole with the widest uncertainty of any seat, handled in omega future_generations_net_position. Coalition prospects for the powerless payers are poor by construction: harmed communities' grievances are materialized harms rather than definitional disputes, and the resource asymmetry that would motivate coalition formation is the same one that blocks its funding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving a constituency-less tail hazard institutional form — remains live: capabilities are still growing and control methods remain immature, so nothing here has outlived its function and mandatrophy is not declared. The tangled-rope judgment is what prevents mislabeling in both directions. Reading the frame as pure extraction erases the coordination value that made it adopted: comparable evaluations, a common hazard taxonomy, and cross-lab incident learning are goods no rival definition currently provides at scale, and abolishing the frame without replacement strands them. Reading it as pure coordination erases the displacement its operation imposes on non-consenting constituencies — the triage is real, continuous, and decided in rooms the displaced do not occupy. Keeping both halves visible also disciplines the persistence question: if capability growth plateaus or control methods mature, the coordination half decays first and what remains is maintenance performance — the degraded transition the theater_ratio series is positioned to detect. The founding_problem_status x disappearance_verdict pair (live x world_rearranges) carries no mismatch flag: the arrangement persists because its problem persists, not because a corpse is being performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_routing,
    'This constraint instantiates the safety_control_reading of the ai_alignment_commitment kernel; what structural features would the sibling readings (ethics_justice_reading, integrated_reading) assign differently to the same standing arrangement?',
    'Comparative classification across the three reading-level stories: divergent epsilon values, victim sets, and computed types locate the kernel contest in the extension of ''alignment'' — which failure modes generate obligation.',
    'If the ethics_justice story computes higher epsilon over the same resource pool, the kernel contest is materially about resource allocation rather than semantics; if the integrated story computes intermediate epsilon with merged victim sets, the readings are partial views of one underlying arrangement rather than rivals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_routing, conceptual, 'Committer routing: this is one of three readings of the alignment kernel').

omega_variable(
    tail_risk_empirical_status,
    'Does the catastrophic loss-of-control hazard carry the probability-and-controllability profile the frame''s urgency assumes, or is the urgency''s evidentiary basis thinner than its rhetorical force?',
    'Longitudinal dangerous-capability evaluation results, structured elicitation of expert hazard distributions, and incident-base-rate analysis as deployments accumulate.',
    'A weaker-than-assumed hazard profile shifts weight toward the frame''s extraction components and recomputes payer-heavy seats toward snare-leaning classifications; a stronger profile validates the coordination function and supports rope-leaning computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tail_risk_empirical_status, empirical, 'Empirical status of the tail-risk premise beneath the definitional frame').

omega_variable(
    lab_sincerity_vs_cover,
    'Do frontier labs pursue the control agenda as a substantive engineering goal, or consume alignment commitment primarily as legitimation for continued scaling?',
    'Revealed-preference audit: compute allocation between capability and safety programs under external verification, plus staff attrition patterns from safety teams following capability milestones.',
    'Cover-dominance concentrates the frame''s gains further on lab legitimation and hardens payer-seat classifications; demonstrated sincerity strengthens the coordination half and softens effective extraction for lab-facing seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lab_sincerity_vs_cover, empirical, 'Sincerity of lab alignment commitment versus racing cover').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the frame''s suppression of rival framings structural (venue, funding, and hiring gatekeeping) or internalized (definitional loyalty that persists when the gates open)?',
    'Post-exit trajectory study: researchers who leave safety institutions for fairness-adjacent work — do their framing allegiances persist, and do venues admit rival framings once editorial boards diversify?',
    'An internalized share raises effective suppression above the structural measure and predicts slower frame decay even under reform; purely structural suppression would unwind quickly if funding gates opened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism behind measured suppression').

omega_variable(
    future_generations_net_position,
    'Are future generations net protected or net burdened by a definitional frame that promises them loss-of-control protection while displacing present-harm mitigation?',
    'Counterfactual portfolio analysis comparing frame-directed versus diversified mitigation spending against both hazard classes under uncertainty-weighted outcomes.',
    'A net-burden verdict confirms their placement among payers; a net-protection verdict moves them to beneficiary-side directionality and shrinks the victim set to present-day constituencies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_net_position, conceptual, 'Net position of future generations under the frame').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the commitment system best framed as a semantic contest over the term ''alignment'', or as a legitimacy contest over who may speak for alignment?',
    'Trace adjudication events — editorial decisions, funding awards, regulatory witness selection: disputes settled by definitional argument indicate the semantic frame; disputes settled by institutional position indicate the authority frame.',
    'The authority-frame reading shifts authority grounding from extraction toward practice, changes which seats count as agenda setters, and alters the drift-state interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__safety_control_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__safety_control_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__safety_control_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__safety_control_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__safety_control_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__safety_control_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__safety_control_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__safety_control_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__safety_control_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__safety_control_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__safety_control_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__safety_control_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__safety_control_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__safety_control_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__safety_control_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__safety_control_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__safety_control_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_commitment__safety_control_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, information_standard).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment' covers structurally distinct claims sharing one kernel (the alignment commitment) and diverging on which failure modes constitute misalignment. This story instantiates the safety-control reading: alignment as preventing catastrophic loss of control, with present-harm constituencies and contingently future generations among its targets and high displacement of present-day harm-mitigation resources. The sibling ethics-justice reading instantiates alignment as preventing reproduction of social bias and present-day harm, with its own epsilon, victim set, and enforcement surface; the integrated reading instantiates simultaneous non-exclusive treatment. Per the epsilon-invariance principle the readings are separate files linked through this edge set. The safety-control reading's ascendancy structurally conditions both siblings' operating environments — recorded in reading_relations as influences toward the integrated reading and coexistence with the ethics-justice reading, whose lineage predates and parallels this frame's consolidation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
