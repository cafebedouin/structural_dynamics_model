% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite: External Cost + Conceptual Redefinition)
 *   domain: social/legal/historical
 *
 * SUMMARY:
 *   The decline of dueling as a legitimate status mechanism in Europe
 *   (approximately 1600–1850) resulted from two analytically distinct
 *   mechanisms operating simultaneously: (1) external costs (drop mechanism)
 *   — legal penalties, social ostracism, and military discipline made dueling
 *   practically costly; (2) conceptual redefinition (contraction mechanism) —
 *   honor itself was redefined to exclude violence, rendering dueling
 *   structurally unthinkable even as practitioners retained the capacity to
 *   fight. This composite reading treats both mechanisms as essential to
 *   persistence. The drop mechanism alone would have driven dueling
 *   underground or into ritual forms; the contraction mechanism ensured that
 *   even survivors of the external pressure regime lost the cognitive
 *   framework that made violence-based honor intelligible. The key structural
 *   innovation is the insight that the two mechanisms target different victim
 *   sets and operate at different constraint levels: the drop works through
 *   immediate material cost (individual and organizational level), while the
 *   contraction works through identity redefinition (class and structural
 *   level). The composite reading is distinct from the drop-only reading
 *   (which underestimates the cognitive work required to delegitimize honor
 *   violence) and the contraction-only reading (which underestimates the
 *   material enforcement required to prevent practitioners from abandoning
 *   the new framework when it conflicts with their identity).
 *
 * KEY AGENTS:
 *   - Honor class practitioners (military aristocrats, gentlemen): primary targets of both mechanisms; identity-locked to honor-violence nexus; faced death, legal jeopardy (drop) and delegitimation (contraction)
 *   - State consolidation apparatus (crown, judiciary, military hierarchy): agenda-setter; enforced the drop through legal sanctions and enforced the contraction through cultural redefinition; benefited from monopolized violence and legitimacy
 *   - Male honor dependents (merchants, professionals, lower-rank soldiers): secondary payers; experienced drop costs without aristocratic alternatives; faced contraction without institutional backup for status
 *   - Alternative status frameworks (merit, wealth, bureaucratic rank): beneficiary positions (not agents); gained legitimacy through the redefinition; framed as modern, rational, civilized alternatives
 *   - Legal and moral reformers: observers with institutional power; articulated the redefinition of honor; claimed scientific and moral grounding for the contraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.71).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite: External Cost + Conceptual Redefinition)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "social/legal/historical").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '8ec896c3-d466-493d-ba0e-cbfd4c9d6239').
narrative_ontology:cs_kernel_codification('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', formalized).
narrative_ontology:cs_authority_grounding('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', extraction).
narrative_ontology:cs_interpretation_layer_present('8ec896c3-d466-493d-ba0e-cbfd4c9d6239').
narrative_ontology:cs_reading_relation('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', foundational, dual_mechanism_necessity).
narrative_ontology:cs_axiom_status(dual_mechanism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', dual_mechanism_necessity, empirically_contingent).
narrative_ontology:cs_axiom('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', foundational, contraction_plus_enforcement_sufficiency).
narrative_ontology:cs_axiom_status(contraction_plus_enforcement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', contraction_plus_enforcement_sufficiency, deontological).
narrative_ontology:cs_reference_frame('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', honor_violence_legitimate_status_mechanism).
narrative_ontology:cs_drift_state('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', post_reform_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ec896c3-d466-493d-ba0e-cbfd4c9d6239', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, state_consolidation_apparatus).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, alternative_status_frameworks).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, honor_class_practitioners).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, male_honor_dependents).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, state_monopoly_on_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, rationalist_honor_reconstruction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military aristocrats and gentlemen whose identity and status were constitutively tied to the capacity and willingness to defend honor through violence. They bore the external costs of dueling (death, injury, legal jeopardy) and the cognitive cost of having their legitimacy framework redefined as backward. Exit from honor-based status claims meant abandonment of professional and social identity.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_class_practitioners, payer,
    moderate, biographical, identity_locked, national).

% The centralizing state (crown, judiciary, military command structure) enforced bans on dueling and redefined honor to exclude violence. It collected legitimacy and enforcement authority by monopolizing violence and rewriting what counts as honorable conduct. It also eliminated a rival status-legitimacy system that competed with state-assigned rank.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_consolidation_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Non-aristocratic men who depended on honor-based status networks for social position, employment, and alliance formation (merchants, professionals, soldiers of lower rank). They faced the external pressure of legal risk from dueling without the institutional alternatives wealthy aristocrats developed; their identity anchors contracted without replacement options.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, male_honor_dependents, payer,
    powerless, biographical, constrained, national).

% Merit-based, wealth-based, and bureaucratic status systems that gained legitimacy as violence-based honor contracted. These frameworks benefited from the redefinition because they could now claim to be the modern, rational, civilized alternatives to honor violence. They were not actors but beneficiary institutional positions.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, alternative_status_frameworks, beneficiary,
    institutional, generational, analytical, national).

% Families of men killed in duels had no voice in the legitimacy redefinition. Their material loss was real but not counted as part of the honor-cost calculus by either the practitioners or the state. They were structurally outside the honor framework itself.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_victims_families, excluded,
    powerless, immediate, trapped, local).

% Philosophers, jurists, and moral authorities who articulated the redefinition of honor to exclude violence. They claimed scientific, rational, and moral grounds for the new framework. Their role was making the contraction conceptually coherent and inevitable-seeming rather than coercive.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, legal_and_moral_reformers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, state_consolidation_apparatus).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Status determination: the constraint coordinated how individuals established and maintained social rank within a hierarchical society. Without a shared, stable legitimacy frame, status claims fragment and hierarchy becomes contested. Honor violence provided one such frame (martial prowess = legitimacy); the constraint persists by ensuring that alternative frames (merit, wealth, bureaucratic rank) become the accepted mechanisms.
% TRANSFER_FUNCTION: Transfers status authority from violence-based honor systems to state-approved and rational/merit-based frameworks. It redistributes legitimacy and authority: away from martial aristocrats who depended on capacity to fight, toward state officials who control punishment/reward, toward wealth-holders and merit-bearers whose status depends on institutional (not martial) validation. It extracts compliance and identity-reconstruction from honor practitioners.
% ABSENT_VOICES: Dueling victims and their families are absent from the legitimacy framework entirely — they never had standing to claim the honor-based status that dueling protected. Lower-status dependents on honor networks have limited voice in the redefinition process — they experience the contraction primarily as enforcement by elites rather than as moral discovery. Practitioners themselves are included in the final framework but only after their identity has been restructured.
% DISAPPEARANCE_RATIONALE: Different seats dispute what drove the disappearance. If only the drop mechanism (external legal/military enforcement) were in effect and the contraction mechanism (cognitive redefinition) were removed, dueling might re-emerge in underground or ritualized forms within the honor community — the material threat alone cannot prevent practitioners from maintaining the cognitive frame. If only the contraction mechanism were in effect and enforcement became lax, dueling would remain suppressed because practitioners would no longer believe honor requires violence. The fact that dueling remained suppressed even as enforcement capacity sometimes weakened suggests the contraction was persistent. Yet modern honor discourse occasionally resurfaces (military codes, nationalist narratives), suggesting the redefinition is not absolutely irreversible. The verdict is contested because the two mechanisms are analytically distinguishable but empirically entangled.
% FOUNDING_PROBLEM: Dueling was generating observable mortality among the governing class and undermining state monopoly on violence. An alternative status-legitimacy system (honor-based) competed with state-assigned rank and produced wasteful violence. The state faced a coordination problem: how to suppress a rival status framework that generated real costs (deaths) and real authority competition, while maintaining a functional status hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: State officials, military leaders, and legal reformers of the period attest the problem was real: dueling deaths were documented, state authority was undermined, honor culture was widespread. Modern historians document the same facts independently. Honor practitioners after the constraint took hold acknowledged the deaths but disputed that the solution required complete redefinition rather than merely stricter enforcement. The independent historical record (external to the benefiting parties) confirms dueling was a material problem; the dispute centers on whether the contraction mechanism was necessary or merely convenient.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness series shows a characteristic pattern for a composite mechanism: initial rise (t=0 to t=25) driven primarily by the drop mechanism (external costs accumulating, enforcement capacity expanding, legal penalties becoming predictable), then plateau (t=25 to t=40) once the contraction mechanism takes hold and the cognitive framework has been sufficiently delegitimized that the material threat alone ceases to be the binding constraint. Theater_ratio rises in parallel, tracking the growth of performative honor redefinition — legal rhetoric framing the constraint as civilizing progress, institutional ceremonials replacing martial honor with bureaucratic rank. The suppression_requirement follows a similar rise-then-plateau pattern: as the contraction mechanism solidifies, less active suppression is needed because practitioners have internalized the new legitimacy framework. The claim/metric gap is intentional: the constraint is CLAIMED as tangled_rope (genuine coordination function in status determination + asymmetric extraction) while the metrics describe substantially extractive, actively enforced operation with rising theater — the engine detects that this is not pure coordination but hybrid coordination/extraction with identity-lock on the targets.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence is severe and operates across three dimensions. (1) Temporal: at t=0, practitioners see honor violence as legitimate and necessary; the state sees it as undermining authority; by t=40, both have ostensibly adopted the redefined honor framework, but practitioners experience it as imposed, while the state experiences it as natural progress. (2) Structural: the state can frame the constraint as civilizing and inevitable; practitioners experience it as coercive identity destruction. (3) Mechanistic: the drop mechanism is visible to all parties (the dueling deaths, the legal penalties); the contraction mechanism is presented as the inevitable evolution of moral understanding, but practitioners experience it as having been authored by their antagonists. The engine should compute: state/institutional seat at low directionality (d ≈ 0.1–0.2, high beneficiary status), honor practitioners at high directionality (d ≈ 0.8–0.9, high target status), dependent/lower-status seats at moderate-high (d ≈ 0.65–0.75, caught between material drop costs and identity contraction without institutional alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: state_consolidation_apparatus (collects legitimacy, violence monopoly, removes rival status system; exit options are arbitrage — it can shift enforcement mechanisms without losing institutional identity); alternative_status_frameworks (positioned to gain legitimacy through the redefinition; institutional exit is analytical). Victims/Payers: honor_class_practitioners (bear both drop costs and contraction costs; identity-locked makes exit unthinkable — to abandon honor is to cease being the type of person they understand themselves to be); male_honor_dependents (bear drop costs without aristocratic resources to absorb them; constrained exit because their status depends on the networks that are being redefined). The contraction mechanism is what makes the drop mechanism extractive rather than merely regulatory: without the contraction, practitioners could cling to honor violence as illegitimate but personally meaningful; the contraction forces them to abandon the frame itself, not just the practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (status determination via violence-based honor) has definitively outlived its function as the primary coordination mechanism for status. By t=40, the state has successfully monopolized violence, alternative status systems are functioning, and honor violence is rare and prosecuted. Yet the constraint persists in the form of residual enforcement (laws against dueling remain; dishonor language persists in military codes) and theatrical maintenance (honor terminology recycled into nationalist and military discourse). The theatrical maintenance is the surest sign of mandatrophy: the substantive honor-dueling framework is dead; the language of honor persists as performance. The founder problem (dueling deaths, state monopoly) is accomplished; the persistence is inertial. However, the contraction mechanism complicates the mandatrophy diagnosis: while the original function is gone, the cognitive delegitimation persists as a structural achievement that actively prevents regurgitation of honor violence even as external enforcement might relax. The theater is not just maintaining the constraint but defending the delegitimation itself — so long as honor is redefined to exclude violence, even a relaxed enforcement regime would not restore dueling. The mandatrophy is real at the original function level; the persistence is real at the cognitive level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_sufficiency_boundary,
    'Would the drop mechanism alone (legal enforcement, military discipline, external costs) have been sufficient to suppress dueling without the contraction mechanism (redefinition of honor to exclude violence)?',
    'Historical natural experiment: comparative analysis of jurisdictions where legal enforcement was strict but honor redefinition was weak (did dueling persist in underground or ritualized forms?) versus jurisdictions where redefinition was strong but enforcement was intermittent (did dueling remain cognitively legitimate despite legal risk?).',
    'If drop mechanism alone was sufficient, the constraint is primarily Snare with extractive enforcement plus material suppression; if both mechanisms were necessary, the constraint is genuinely Tangled Rope with coordination (status determination) + asymmetric extraction + cognitive delegitimation as the persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_sufficiency_boundary, empirical, 'Whether external costs or cognitive redefinition was the binding constraint on dueling persistence.').

omega_variable(
    contraction_authenticity,
    'Was the redefinition of honor to exclude violence a genuine evolution of moral understanding, or was it a post-hoc rationalization authored by the state and reformers to justify enforcement?',
    'Textual and genealogical analysis of honor discourse before and after the enforcement period; attribution of honor redefinition ideas to pre-existing intellectual currents versus to state-sponsored reform institutions; testimony from practitioners about whether the new honor framework felt like discovery or imposition.',
    'If authentic evolution, the constraint captures a real change in what counts as honorable (the contraction is structural); if post-hoc rationalization, the constraint is primarily Snare with a cover story of moral progress. This affects the classification directionality computation: authentic contraction makes it Tangled Rope with real but asymmetric coordination; rationalized contraction makes it pure Snare with theatrical legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_authenticity, conceptual, 'Whether the redefinition of honor represents genuine moral evolution or coercive cognitive capture.').

omega_variable(
    identity_lock_internalization,
    'For honor class practitioners, how much of the observed compliance with the new framework represents genuine internalization of the redefined honor (acceptance that honor now excludes violence), versus strategic conformity while maintaining private dissent?',
    'Post-exit trajectory analysis: if practitioners who abandoned honor-violence compliance at the edge of legal enforcement (e.g., by emigrating to less-enforced jurisdictions) maintained the belief in violence-based honor, that suggests the internalization was partial/coerced; if they adopted the redefined framework even outside enforcement reach, that suggests genuine internalization.',
    'High internalization suggests the contraction mechanism was effective and persistent; low internalization suggests the drop mechanism remains binding and the constraint relies on ongoing enforcement. This affects long-term stability and the theater ratio interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Degree of genuine internalization versus strategic conformity in practitioners'' adoption of redefined honor.').

omega_variable(
    suppression_mechanism_type,
    'For male honor dependents and lower-status practitioners, was the suppression of honor violence primarily structural (they lacked the institutional power to sustain dueling culture) or internalized (they accepted the new honor framework as legitimate)?',
    'Comparative class analysis: did lower-status populations that were excluded from honor-based status networks (servants, peasants, laborers) show different patterns of violence legitimacy than dependent males (merchants, professionals, lower-rank soldiers) who had participated in honor networks? If structural, they should show little change; if internalized, they should show evidence of having accepted the new framework.',
    'If structural suppression, the lower-status victims experienced the constraint primarily as coercion without cognitive capture; if internalized, the constraint was more effective at delegitimization across classes. Affects the theater ratio interpretation and the overall assessment of contraction mechanism effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Whether suppression of lower-status honor violence was structural exclusion or internalized acceptance of new framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t5, honor_violence_legitimacy__composite_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(hono_tr_t5, observed).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__composite_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t15, honor_violence_legitimacy__composite_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t25, honor_violence_legitimacy__composite_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(hono_tr_t25, observed).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(hono_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t5, honor_violence_legitimacy__composite_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(hono_be_t5, observed).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__composite_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t15, honor_violence_legitimacy__composite_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t25, honor_violence_legitimacy__composite_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(hono_be_t25, observed).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hono_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.41).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t5, honor_violence_legitimacy__composite_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(hono_su_t5, observed).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__composite_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t15, honor_violence_legitimacy__composite_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t25, honor_violence_legitimacy__composite_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hono_su_t25, observed).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(hono_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(hono_grid_01, honor_violence_legitimacy__composite_reading, accessibility_collapse(class), 0, 0.42).
narrative_ontology:measurement(hono_grid_02, honor_violence_legitimacy__composite_reading, accessibility_collapse(class), 40, 0.72).
narrative_ontology:measurement(hono_grid_03, honor_violence_legitimacy__composite_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(hono_grid_04, honor_violence_legitimacy__composite_reading, accessibility_collapse(individual), 40, 0.78).
narrative_ontology:measurement(hono_grid_05, honor_violence_legitimacy__composite_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(hono_grid_06, honor_violence_legitimacy__composite_reading, accessibility_collapse(organizational), 40, 0.81).
narrative_ontology:measurement(hono_grid_07, honor_violence_legitimacy__composite_reading, accessibility_collapse(structural), 0, 0.38).
narrative_ontology:measurement(hono_grid_08, honor_violence_legitimacy__composite_reading, accessibility_collapse(structural), 40, 0.69).
narrative_ontology:measurement(hono_grid_09, honor_violence_legitimacy__composite_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(hono_grid_10, honor_violence_legitimacy__composite_reading, resistance(class), 40, 0.35).
narrative_ontology:measurement(hono_grid_11, honor_violence_legitimacy__composite_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(hono_grid_12, honor_violence_legitimacy__composite_reading, resistance(individual), 40, 0.41).
narrative_ontology:measurement(hono_grid_13, honor_violence_legitimacy__composite_reading, resistance(organizational), 0, 0.75).
narrative_ontology:measurement(hono_grid_14, honor_violence_legitimacy__composite_reading, resistance(organizational), 40, 0.38).
narrative_ontology:measurement(hono_grid_15, honor_violence_legitimacy__composite_reading, resistance(structural), 0, 0.61).
narrative_ontology:measurement(hono_grid_16, honor_violence_legitimacy__composite_reading, resistance(structural), 40, 0.28).
narrative_ontology:measurement(hono_grid_17, honor_violence_legitimacy__composite_reading, stakes_inflation(class), 0, 0.51).
narrative_ontology:measurement(hono_grid_18, honor_violence_legitimacy__composite_reading, stakes_inflation(class), 40, 0.74).
narrative_ontology:measurement(hono_grid_19, honor_violence_legitimacy__composite_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(hono_grid_20, honor_violence_legitimacy__composite_reading, stakes_inflation(individual), 40, 0.82).
narrative_ontology:measurement(hono_grid_21, honor_violence_legitimacy__composite_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(hono_grid_22, honor_violence_legitimacy__composite_reading, stakes_inflation(organizational), 40, 0.79).
narrative_ontology:measurement(hono_grid_23, honor_violence_legitimacy__composite_reading, stakes_inflation(structural), 0, 0.41).
narrative_ontology:measurement(hono_grid_24, honor_violence_legitimacy__composite_reading, stakes_inflation(structural), 40, 0.68).
narrative_ontology:measurement(hono_grid_25, honor_violence_legitimacy__composite_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(hono_grid_26, honor_violence_legitimacy__composite_reading, suppression(class), 40, 0.68).
narrative_ontology:measurement(hono_grid_27, honor_violence_legitimacy__composite_reading, suppression(individual), 0, 0.35).
narrative_ontology:measurement(hono_grid_28, honor_violence_legitimacy__composite_reading, suppression(individual), 40, 0.72).
narrative_ontology:measurement(hono_grid_29, honor_violence_legitimacy__composite_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(hono_grid_30, honor_violence_legitimacy__composite_reading, suppression(organizational), 40, 0.75).
narrative_ontology:measurement(hono_grid_31, honor_violence_legitimacy__composite_reading, suppression(structural), 0, 0.39).
narrative_ontology:measurement(hono_grid_32, honor_violence_legitimacy__composite_reading, suppression(structural), 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_violence_legitimacy kernel has three constraint stories corresponding to three readings of the same contested claim. The composite_reading asserts that both the drop mechanism (external costs reducing dueling practice) and the contraction mechanism (redefinition of honor to exclude violence) operated simultaneously and were both necessary to persistence. The drop_reading emphasizes external costs as the primary driver and treats contraction as post-hoc rationalization. The contraction_reading emphasizes the cognitive delegitimation as primary and treats external costs as secondary enforcement. The three readings are linked via network edges: composite_reading influences both drop_reading and contraction_reading (the composite analysis subsumes the mechanisms both readings foreground); drop_reading coexists_with contraction_reading (they compete as explanations but neither logically forecloses the other). All three stories share the same referent (dueling legitimacy and its decline) and assess it by the same lights (a materialist/institutional analysis); they diverge in which mechanisms they treat as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
