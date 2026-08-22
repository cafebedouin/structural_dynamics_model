% ============================================================================
% CONSTRAINT STORY: dignity_kernel__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__imago_dei_reading, []).

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
 *   constraint_id: dignity_kernel__imago_dei_reading
 *   human_readable: Capability-Indexed Worth Regime (as assessed by the Imago Dei Reading)
 *   domain: theological ethics / technology governance / philosophical anthropology
 *
 * SUMMARY:
 *   This story instantiates the imago_dei reading of the dignity_kernel:
 *   dignity is the inviolable image of the Triune God, equal in all persons
 *   prior to any capability. Per the epsilon-referent rule for kernel
 *   readings, every metric's referent is the standing arrangement under
 *   contest — the operative governance and cultural regime in which human
 *   worth is progressively indexed to capability, enhancement proceeds as a
 *   consumer and strategic market, and AI development faces no
 *   subordination-to-person limit — assessed by this reading's own lights,
 *   which see the regime as stripping inviolable status from persons. The
 *   referent is NOT the created-order arrangement this reading would
 *   institute. Sibling readings are separate constraint files sharing this
 *   referent with divergent reading-indexed values:
 *   dignity_kernel__autonomy_rights_reading (worth grounded in autonomy and
 *   rationality — a narrower protected class that leaves the severely
 *   cognitively disabled exposed) and dignity_kernel__posthumanist_reading
 *   (no fixed human limit — which would assess the same arrangement as benign
 *   or beneficial). The family decomposition follows the epsilon-invariance
 *   principle: the colloquial label 'human dignity' covers three structurally
 *   distinct constraints with different victim sets, linked via
 *   network.affects_constraints. The claim/metrics split is deliberate:
 *   claimed_type states the structure this reading believes true of the
 *   arrangement (real coordination functions carrying asymmetric extraction
 *   through the same machinery); the metric values state what the reading
 *   observes descriptively; the engine computes per-seat classifications from
 *   the structural data independently of both.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: primary beneficiary (institutional/arbitrage) — develops toward superintelligence under no subordination limit; captures capital, talent, and regulatory attention
 *   - enhancement_biotech_industry: secondary beneficiary (institutional/arbitrage) — sells enhancement as consumer health; shapes trial endpoints and approval pathways
 *   - capability_elites: tertiary beneficiary (powerful/mobile) — early adopters whose augmented capacities compound advantage and recognized standing
 *   - transhumanist_institutions: ideological beneficiary (organized/identity_locked) — the regime's operation vindicates the continuity thesis their identity rides on
 *   - technocratically_reduced_persons: primary target (powerless/trapped) — worth priced by capability metrics; cannot exit being measured
 *   - enhancement_pressured_persons: secondary target (moderate/constrained) — nominal choice, practically compelled; opting out compounds disadvantage
 *   - unconsenting_future_generations: tertiary target (powerless/trapped, civilizational horizon) — inherit heritable modification and an AI-saturated world without consent
 *   - bioethics_governance_bodies: agenda setter (institutional/constrained) — writes the operative rules under an autonomy-plus-risk frame
 *   - religious_dignity_traditions: excluded voice (organized/identity_locked) — holds the imago-dei doctrine at advisory margins; would contest the framing itself
 *   - disability_rights_advocates: analytical observer (organized) — documents capability-indexed devaluation; collects nothing, bears advocacy costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, 0.78).
domain_priors:suppression_score(dignity_kernel__imago_dei_reading, 0.62).
domain_priors:theater_ratio(dignity_kernel__imago_dei_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__imago_dei_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__imago_dei_reading, "Capability-Indexed Worth Regime (as assessed by the Imago Dei Reading)").
narrative_ontology:topic_domain(dignity_kernel__imago_dei_reading, "theological ethics / technology governance / philosophical anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__imago_dei_reading, '088fe727-a8b5-49f3-a348-9584bca76177').
narrative_ontology:cs_kernel_codification('088fe727-a8b5-49f3-a348-9584bca76177', distributed).
narrative_ontology:cs_authority_grounding('088fe727-a8b5-49f3-a348-9584bca76177', distributed).
narrative_ontology:cs_reading_relation('088fe727-a8b5-49f3-a348-9584bca76177', dignity_kernel__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('088fe727-a8b5-49f3-a348-9584bca76177', dignity_kernel__posthumanist_reading, forecloses).
narrative_ontology:cs_axiom('088fe727-a8b5-49f3-a348-9584bca76177', foundational, dignity_equal_prior_to_capability).
narrative_ontology:cs_axiom_status(dignity_equal_prior_to_capability, holdable).
narrative_ontology:cs_axiom_grounding('088fe727-a8b5-49f3-a348-9584bca76177', dignity_equal_prior_to_capability, theological).
narrative_ontology:cs_axiom('088fe727-a8b5-49f3-a348-9584bca76177', secondary, technology_subordinate_to_human_person).
narrative_ontology:cs_axiom_status(technology_subordinate_to_human_person, holdable).
narrative_ontology:cs_axiom_grounding('088fe727-a8b5-49f3-a348-9584bca76177', technology_subordinate_to_human_person, theological).
narrative_ontology:cs_reference_frame('088fe727-a8b5-49f3-a348-9584bca76177', created_order_imago_equality).
narrative_ontology:cs_drift_state('088fe727-a8b5-49f3-a348-9584bca76177', contemporary_technocratic_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('088fe727-a8b5-49f3-a348-9584bca76177', '').
narrative_ontology:cs_kernel_id(dignity_kernel__imago_dei_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, enhancement_biotech_industry).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, capability_elites).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, transhumanist_institutions).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, technocratically_reduced_persons).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, enhancement_pressured_persons).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, unconsenting_future_generations).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, capability_progressivism).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, enhancement_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop large-scale AI systems toward ever-higher capability, including openly pursued superintelligence, under governance that imposes no requirement that systems remain subordinate to the human person. Gains flow to them as capital, talent, and regulatory attention concentrate on their roadmap. If any jurisdiction tightened subordination limits, they could shift research operations to permissive ones.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, frontier_ai_labs, beneficiary,
    institutional, generational, arbitrage, global).

% Sells cognitive, physical, and reproductive enhancement as a consumer health market. Benefits from a governance frame that treats enhancement as product development rather than as an anthropological boundary question, and shapes trial endpoints and approval pathways accordingly. Trials and product launches can move to permissive jurisdictions.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, enhancement_biotech_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Early adopters of enhancement whose augmented capacities compound advantages in education, labor, and social selection. Under a regime that indexes worth to capability, their recognized standing rises with each purchased increment. They can buy insulation from the regime's downsides through private medicine and jurisdictional mobility.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, capability_elites, beneficiary,
    powerful, biographical, mobile, global).

% Think tanks, movements, and communities whose purpose and self-understanding are fused with the enhancement project. The regime's operational success lends their continuity thesis practical vindication and confers legitimacy on their policy agenda. Abandoning the project would dissolve their institutional and ideological identity.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, transhumanist_institutions, beneficiary,
    organized, generational, identity_locked, global).

% Elderly, severely disabled, and cognitively dependent persons whose care allocation, insurance pricing, and social recognition increasingly track measured capability. They cannot stop being measured, cannot re-enter the capability economy, and their dependence deepens their exposure. What they lose is recognition of worth that does not depend on output.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, technocratically_reduced_persons, payer,
    powerless, biographical, trapped, global).

% Students, workers, and parents inside competitive systems where declining available enhancement means falling behind peers who accept it. Choice is nominally free but practically compelled: opting out carries compounding costs in admission, employment, and standing. They pay in money, risk, and in the conversion of their own capacities into performance assets.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, enhancement_pressured_persons, payer,
    moderate, biographical, constrained, global).

% Persons who will inherit heritable modifications and an AI-saturated environment decided before they existed. They have no exit from an inherited bodily and cognitive condition and no seat in the decisions that set it. Their stake is asserted on their behalf and contested on their behalf.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, unconsenting_future_generations, payer,
    powerless, civilizational, trapped, universal).

% National bioethics commissions, research-ethics regimes, and AI safety institutes that write the operative rules. Their working frame is autonomy-plus-risk-management: protect consent, manage harm, permit capability development. Adopting created-order limits would break their working coalition with industry and adjacent disciplines, so the frame persists.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, bioethics_governance_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Churches and theological traditions holding that every person bears the image of the Triune God with equal and inviolable worth prior to any capability. They are seated at advisory margins of bioethics and AI governance, consulted but not deciding. They would contest the regime's basic framing rather than its parameters, and cannot abandon the doctrine without dissolving their own identity.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, religious_dignity_traditions, excluded,
    organized, civilizational, identity_locked, global).

% Organizations and scholars documenting how capability-indexed evaluation devalues dependent persons, and pressing for worth-floors that do not track function. They track the regime closely and publish its failures; they collect nothing from its operation and bear its costs only as advocates. They ally with the imago-dei reading on the equal-worth floor while differing on its ground.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, disability_rights_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__imago_dei_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(dignity_kernel__imago_dei_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates biomedical and AI innovation at scale: common research-governance rules, funding pipelines, capability metrics for clinical triage and insurance, deployment-safety frameworks for AI, and a shared progress narrative that aligns capital, talent, and regulation across jurisdictions.
% TRANSFER_FUNCTION: Moves worth-recognition, resources, and decision authority along capability gradients: enhanced and high-capacity persons capture prestige, funding, and priority; allocation weight shifts away from the capability-poor; authority over the human body and mind shifts from persons, families, and traditions to technical systems, markets, and expert bodies. Net flow runs from technocratically_reduced_persons, enhancement_pressured_persons, and unconsenting_future_generations toward capability_elites, frontier_ai_labs, and enhancement_biotech_industry.
% ABSENT_VOICES: Religious dignity traditions hold the created-order limit but sit at advisory margins of bioethics and AI governance; the capability-poor appear through proxies and advocacy documentation rather than decision seats; future generations have no seat at all. Each would contest the framing that capability is a legitimate axis of worth, not merely its calibration.
% DISAPPEARANCE_RATIONALE: If the capability-indexed regime vanished overnight, health-resource allocation would lose its triage and pricing metrics, the enhancement market would lose its legitimacy frame, AI laboratories would face sudden subordination demands they have not planned around, and the prestige economy rewarding augmentation would collapse into renegotiation. The innovation economy built on the regime would reorganize around whatever worth-floor replaced it.
% FOUNDING_PROBLEM: Postwar biomedical governance was assembled to permit therapeutic progress without repeating eugenic atrocity: to govern transformative power over human bodies and minds after capability-ranked extermination had discredited capability-ranked worth. Later layers applied the same autonomy-and-risk toolkit to AI acceleration and germline intervention.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: disability-rights scholarship documents capability-indexed devaluation of dependent persons; eugenics historiography traces the continuity between capability-ranked worth and the founding atrocity; the UN Convention on the Rights of Persons with Disabilities anchors protection in inherent rather than functional worth. Industry and transhumanist parties dispute that the regime reproduces the founding threat, attesting instead that governance has contained it.
narrative_ontology:disappearance_verdict(dignity_kernel__imago_dei_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__imago_dei_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__imago_dei_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignity_kernel__imago_dei_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__imago_dei_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__imago_dei_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because, by this reading's lights, the regime decouples recognized worth from the inviolable: persons are priced by output, enhancement converts capacities into purchasable standing, and heritable modification transfers bodily decisions to those who precede the persons affected. Suppression (0.62) is a raw structural property, unscaled by power or scope: the regime holds its shape through funding gatekeeping, prestige economies that penalize refusal, jurisdictional competition that punishes unilateral restraint, and a technocratic formation that makes capability-indexing feel like neutral description. Theater (0.38) is rising because dignity language saturates AI-ethics charters and bioethics reports while substantive protection of the capability-poor recedes — proxy language substituting for the function it names (Goodhart drift). Accessibility collapse is moderate (0.45): refusal, subordination-limited AI, and theological anthropology remain visible and practiced in communities, but each carries compounding competitive cost, so alternatives narrow without vanishing. Resistance (0.55) is real: religious bodies, disability advocates, germline-editing moratoria, and several national prohibitions actively contest the trajectory. All three tracked series run on one shared time grid (points 0, 6, 12, 18, 24, 30) so every metric is authored at every examined time point; the suppression_requirement series is authored deliberately because the story traces enforcement-capacity change — the governance machinery hardened and extended over the interval rather than staying static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the frontier_ai_labs seat the regime is legitimate coordination it helped build: common safety frameworks, talent pools, and capital alignment solve real problems, and its costs register as ordinary compliance. From the technocratically_reduced_persons seat the same machinery operates as enforced reduction: the metrics that coordinate research are the metrics that price their lives. From the religious_dignity_traditions seat the regime is a civilizational framing error that forecloses the created-order limit before deliberation begins. From the transhumanist_institutions seat the regime is a partial and frustratingly slow realization of flourishing. The engine derives these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directional positions: frontier_ai_labs and enhancement_biotech_industry sit near the beneficiary end (arbitrage-grade exit amplifies this — they can leave any jurisdiction that restrains them), capability_elites near it (mobile, insulated), transhumanist_institutions somewhat above them (they collect legitimacy but bear opportunity costs of slower timelines). Victim declarations drive high directional positions: technocratically_reduced_persons sit nearest the full-target end (trapped — no exit from being measured), unconsenting_future_generations likewise (trapped at civilizational scale, universal scope amplifying verification difficulty), enhancement_pressured_persons slightly below full target (constrained but retaining nominal choice, and capturing some competitive benefit from accepting enhancement). Bioethics_governance_bodies occupy a middle administrative position: they run the machinery and absorb its legitimacy costs without collecting its principal gains. Religious_dignity_traditions and disability_rights_advocates stand outside the gain-and-cost circuit as excluded voice and observer respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — govern transformative power without repeating eugenic atrocity — is not dead, which is why this is not a piton story: the arrangement has not outlived its function by its own lights, and the (contested status x world_rearranges) pair raises no zombie flag. But the reading attests function-substitution inside a live mandate: guardrails written to prevent capability-ranked worth now administer it (triage metrics, insurance pricing, enhancement approval pathways), while dignity rhetoric expands to cover the gap. Classifying the regime as pure extraction would erase its genuine coordination achievements — real safety frameworks, real therapeutic benefit, real collective-action solutions at scale; classifying it as pure coordination would erase the reduction the reading identifies and the victim set it names. The tangled_rope claim preserves both halves: coordination and extraction run through the same machinery, and the machinery requires active enforcement (funding gatekeeping, prestige sanction, jurisdictional competition management) to hold. The receipt surface sharpens the picture: gains land principally at frontier_ai_labs, and fixing is prohibitive for whoever could fix it, because unilateral restraint triggers capital and research flight — a captured-flavored cell that the engine weighs against the authored claim rather than reconciling with it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    created_order_vs_constructed_status,
    'Is the imago-dei limit a feature of created reality binding regardless of belief, or a doctrinal construction maintained by institutions that also hold authority interests in it?',
    'Comparative-anthropological and theological-realist analysis: whether secular orders independently converge on capability-independent worth floors, and whether the doctrine''s enforcement history shows rent-collection alongside protection.',
    'If constructed-with-interests, the constraint''s enforcement carries an institutional-authority component that the reading''s framing obscures, and part of the measured conflict is inter-institutional competition rather than pure anthropological dispute; if created-order-real, the standing regime''s reduction of persons is objectively graver than a preference clash.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(created_order_vs_constructed_status, conceptual, 'Natural-law versus constructed status of the dignity limit; routes the false-summit question for this reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of the created-order alternative structural (funding gatekeeping, prestige economies, jurisdictional competition) or internalized (technocratic formation making capability-indexing feel like neutral description)?',
    'Post-exit suppression trajectory: examine communities and institutions that exited the technocratic frame — if capability-indexing of worth persists in their internal allocations after external barriers are removed, a substantial share is internalized.',
    'If largely internalized, the regime''s effective suppression exceeds the structural measure — targets carry the frame with them into supposedly free choices — and formal governance remedies would under-treat the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized share of the regime''s suppressive force.').

omega_variable(
    sibling_reading_victim_set_delta,
    'This constraint is one reading of dignity_kernel; what structurally changes under the sibling readings — specifically, who counts as a victim at all?',
    'Cross-reading comparison of victim-set boundaries against hard cases: severe cognitive disability (does autonomy-grounding exclude persons lacking rational capacity from protection?), radical enhancement (does no-fixed-limit dissolve the category of harm-by-transformation?).',
    'If the autonomy_rights_reading''s victim set excludes the severely cognitively disabled, that reading materially understates the regime''s harm over the shared referent; the imago reading''s high assessment depends on the image-grounding premise, and the posthumanist reading would assess the same arrangement as benign — the divergence is located entirely in the ground of worth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_victim_set_delta, conceptual, 'Committer-frame omega: how sibling readings of dignity_kernel restructure the victim set over the shared referent.').

omega_variable(
    heritable_modification_consent,
    'Can heritable enhancement ever be legitimated toward persons who did not and could not consent, or does the consent asymmetry make the unconsenting_future_generations seat a permanent victim class under any governance frame?',
    'Longitudinal study of edited-lineage cohorts where they exist, plus structured deliberation across cultures on intergenerational consent instruments.',
    'If legitimation instruments prove possible, the third victim seat contracts to a contested-preference dispute; if not, the regime''s extraction includes an irreducible consent violation that no procedural reform removes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(heritable_modification_consent, empirical, 'Whether the consent asymmetry in heritable modification is resolvable or structurally permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__imago_dei_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignity_imago_tr_t0, dignity_kernel__imago_dei_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(dignity_imago_tr_t0, observed).
narrative_ontology:measurement(dignity_imago_tr_t6, dignity_kernel__imago_dei_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(dignity_imago_tr_t6, observed).
narrative_ontology:measurement(dignity_imago_tr_t12, dignity_kernel__imago_dei_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(dignity_imago_tr_t12, observed).
narrative_ontology:measurement(dignity_imago_tr_t18, dignity_kernel__imago_dei_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(dignity_imago_tr_t18, observed).
narrative_ontology:measurement(dignity_imago_tr_t24, dignity_kernel__imago_dei_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(dignity_imago_tr_t24, observed).
narrative_ontology:measurement(dignity_imago_tr_t30, dignity_kernel__imago_dei_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(dignity_imago_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(dignity_imago_be_t0, dignity_kernel__imago_dei_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(dignity_imago_be_t0, observed).
narrative_ontology:measurement(dignity_imago_be_t6, dignity_kernel__imago_dei_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(dignity_imago_be_t6, observed).
narrative_ontology:measurement(dignity_imago_be_t12, dignity_kernel__imago_dei_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(dignity_imago_be_t12, observed).
narrative_ontology:measurement(dignity_imago_be_t18, dignity_kernel__imago_dei_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement_basis(dignity_imago_be_t18, observed).
narrative_ontology:measurement(dignity_imago_be_t24, dignity_kernel__imago_dei_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement_basis(dignity_imago_be_t24, observed).
narrative_ontology:measurement(dignity_imago_be_t30, dignity_kernel__imago_dei_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(dignity_imago_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(dignity_imago_su_t0, dignity_kernel__imago_dei_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(dignity_imago_su_t0, observed).
narrative_ontology:measurement(dignity_imago_su_t6, dignity_kernel__imago_dei_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(dignity_imago_su_t6, observed).
narrative_ontology:measurement(dignity_imago_su_t12, dignity_kernel__imago_dei_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(dignity_imago_su_t12, observed).
narrative_ontology:measurement(dignity_imago_su_t18, dignity_kernel__imago_dei_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement_basis(dignity_imago_su_t18, observed).
narrative_ontology:measurement(dignity_imago_su_t24, dignity_kernel__imago_dei_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(dignity_imago_su_t24, observed).
narrative_ontology:measurement(dignity_imago_su_t30, dignity_kernel__imago_dei_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(dignity_imago_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__imago_dei_reading, resource_allocation).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, dignity_kernel__autonomy_rights_reading).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'human dignity' decomposes into three structurally distinct constraints sharing one referent (the standing capability-indexed regime) with reading-indexed assessments. This file (imago_dei_reading) assesses the regime at high extraction because worth-tracking-capability strips inviolable status; dignity_kernel__autonomy_rights_reading assesses the same regime through an autonomy-grounded worth floor (different victim set — functionally gated protection); dignity_kernel__posthumanist_reading assesses it as benign or beneficial (no fixed limit to violate). Upstream/downstream: the autonomy reading currently supplies the regime's operative legitimacy language, so it structurally influences this reading's operating environment; the imago reading contests that language at the level of ground. All three files link one another via network.affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
