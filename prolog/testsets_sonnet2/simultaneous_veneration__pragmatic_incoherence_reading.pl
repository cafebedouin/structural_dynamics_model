% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Shinbutsu-Shugo as Suppressed Incoherence (Pragmatic Incoherence Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the pragmatic incoherence reading of the
 *   shinbutsu-shugo (kami-buddha combinatory) kernel that structured Japanese
 *   religious life from roughly the Nara period through 1868. On this
 *   reading, honji-suijaku theory and combined shrine-temple worship were
 *   never a coherent synthesis of two ontologies — they were an unexamined
 *   accommodation that persisted because no institutional actor had both the
 *   incentive and the power to force practitioners to say what they actually
 *   believed kami and buddhas were relative to each other. The arrangement
 *   was sustained by lack of enforcement pressure, not by successful
 *   resolution, and the abruptness and violence of Meiji shinbutsu-bunri (the
 *   1868 forced separation edict, followed by haibutsu kishaku destruction of
 *   Buddhist property) is read here as the revelation of latent incoherence
 *   rather than an externally imposed rupture of something that had been
 *   working. This is one of three readings of the same kernel: the
 *   domain_partition_reading holds that kami and buddhas governed genuinely
 *   separate functional domains (a coherent division of ritual labor), and
 *   the ontological_fusion_reading holds that honji-suijaku captured a real
 *   metaphysical identity beneath cultural surface difference. Both siblings
 *   treat the arrangement as, in some sense, actually working. This reading
 *   treats it as never having worked at all, with the appearance of stability
 *   an artifact of low enforcement pressure across nearly a millennium.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: institutional beneficiary collecting dual revenue from unresolved ambiguity
 *   - syncretic_ritual_specialists: professional beneficiaries whose expertise depends on the contradiction remaining unexamined
 *   - lay_practitioners_seeking_doctrinal_clarity: powerless payers absorbing lifelong cognitive dissonance as custom
 *   - sectarian_reformist_clergy: constrained payers silenced for centuries for naming the incoherence
 *   - meiji_state: excluded external actor whose arrival supplies the enforcement pressure that had been absent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.68).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.58).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, piton).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Shinbutsu-Shugo as Suppressed Incoherence (Pragmatic Incoherence Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '55a51637-80f8-42d6-83d9-f55cbf543f28').
narrative_ontology:cs_kernel_codification('55a51637-80f8-42d6-83d9-f55cbf543f28', distributed).
narrative_ontology:cs_authority_grounding('55a51637-80f8-42d6-83d9-f55cbf543f28', practice).
narrative_ontology:cs_interpretation_layer_present('55a51637-80f8-42d6-83d9-f55cbf543f28').
narrative_ontology:cs_reading_relation('55a51637-80f8-42d6-83d9-f55cbf543f28', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('55a51637-80f8-42d6-83d9-f55cbf543f28', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_axiom('55a51637-80f8-42d6-83d9-f55cbf543f28', foundational, propositional_consistency_was_never_achieved).
narrative_ontology:cs_axiom_status(propositional_consistency_was_never_achieved, holdable).
narrative_ontology:cs_axiom_grounding('55a51637-80f8-42d6-83d9-f55cbf543f28', propositional_consistency_was_never_achieved, empirically_contingent).
narrative_ontology:cs_axiom('55a51637-80f8-42d6-83d9-f55cbf543f28', foundational, stability_reflects_absent_enforcement_not_resolved_doctrine).
narrative_ontology:cs_axiom_status(stability_reflects_absent_enforcement_not_resolved_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('55a51637-80f8-42d6-83d9-f55cbf543f28', stability_reflects_absent_enforcement_not_resolved_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('55a51637-80f8-42d6-83d9-f55cbf543f28', premodern_combinatory_accommodation).
narrative_ontology:cs_drift_state('55a51637-80f8-42d6-83d9-f55cbf543f28', meiji_shinbutsu_bunri_1868, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('55a51637-80f8-42d6-83d9-f55cbf543f28', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_ritual_specialists).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, sectarian_reformist_clergy).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, doctrinal_coherence_is_not_required_for_institutional_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jingu-ji (shrine-temple complexes) administer combined kami-buddha worship sites, collecting patronage, land grants, and ritual fees from both Shinto and Buddhist revenue streams simultaneously. They have no incentive to resolve the underlying contradiction because the ambiguity itself doubles their base of legitimation and lets them absorb whichever framing a given patron finds persuasive.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes, agenda_setter).

% Shugenja and combinatory ritualists perform rites that invoke both kami and buddhas interchangeably, deriving professional standing from mastery of a synthesis that cannot survive doctrinal interrogation. Their livelihood depends on the contradiction remaining unexamined; pressed to explain the ontological status of honji-suijaku correspondences, they retreat to ritual efficacy rather than propositional consistency.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_ritual_specialists, beneficiary,
    moderate, generational, constrained, regional).

% Ordinary worshippers inherit a devotional framework that asks them to hold that a kami both is and is not identical to a buddha, without being given tools to resolve this, and without an alternative venue for coherent instruction available in most villages. They pay in the currency of unexamined cognitive dissonance across a lifetime of practice, absorbing the incoherence as unquestioned custom rather than resolving it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Pure Shinto revivalists (kokugaku scholars, later Meiji-era shinbutsu-bunri architects) and reformist Buddhist clergy who wanted doctrinal purity bore the reputational and institutional cost of pointing out the contradiction for centuries before anyone with state power was willing to act on it. Prior to Meiji, raising the incoherence got a cleric labeled a troublemaker rather than a reformer; the path to correction was blocked until an external political actor made the incoherence actionable.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, sectarian_reformist_clergy, payer,
    moderate, generational, constrained, national).

% The Meiji government that later enacted shinbutsu-bunri (1868) is excluded from the pre-Meiji constraint story proper — it is the external actor whose arrival ends the equilibrium, not a party sustained by it, but its absence from the earlier centuries is precisely why the incoherence persisted unchallenged: there was no enforcement body with both the will and the power to compel resolution before it existed as a modernizing, anti-Buddhist nation-building project.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state, excluded,
    institutional, generational, analytical, national).

% Historians and religious studies scholars examine temple records, doctrinal treatises, and the abruptness of Meiji separation to assess whether pre-Meiji practice reflected a coherent synthesis or an unexamined contradiction tolerated by administrative convenience. Their assessment is what this reading itself represents.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuinely present at the doctrinal level — practitioners were not coordinating around a shared resolved belief. What looks like coordination is administrative convenience: combined shrine-temple complexes pooled ritual labor, land, and patronage under one roof, and the surface-level appearance of synthesis let this pooling continue without anyone having to adjudicate the underlying metaphysical conflict.
% TRANSFER_FUNCTION: Moves clarity and doctrinal accountability away from lay practitioners and reform-minded clergy and toward the administrative complexes and ritual specialists who profit from the arrangement remaining unexamined. The specialists and institutions extract ongoing legitimacy and revenue from a framework whose contradictions are never priced into anyone's decision to participate.
% ABSENT_VOICES: Reform clergy who wanted resolution one way or the other were present but structurally silenced — not literally absent from Japan, but excluded from any forum with power to adjudicate until the Meiji state supplied one. Lay practitioners had essentially no voice at all in the doctrinal question; their participation was assumed rather than solicited.
% DISAPPEARANCE_RATIONALE: When the ambiguity was forcibly resolved by shinbutsu-bunri in 1868, the world did rearrange substantially: shrines and temples were physically and administratively separated, thousands of jingu-ji were dismantled or converted, ritual specialists lost their syncretic professional niche, and a wave of haibutsu kishaku (anti-Buddhist violence) followed. This is direct evidence that something load-bearing — institutional arrangements, property, professional identity — depended on the unresolved ambiguity being left alone, even though the ambiguity itself was never doctrinally coherent.
% FOUNDING_PROBLEM: The problem was never theological; it was the practical need to integrate an imported Buddhist tradition into an existing kami-based ritual landscape without triggering the kind of doctrinal war that could destabilize patronage networks, court politics, and local ritual authority. Honji-suijaku theory and combinatory practice were adopted as a working accommodation, not because anyone had solved the metaphysical question of what kami and buddhas actually are relative to each other.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era shinbutsu-bunri architects and kokugaku scholars (an audience with every institutional incentive to declare the old arrangement finished) attested the incoherence explicitly and acted on it. More significantly, modern historians of Japanese religion working outside any tradition invested in either syncretism or separation (e.g. scholarship analyzing jingu-ji records and honji-suijaku textual inconsistencies across centuries) independently corroborate that no stable resolving doctrine existed at any point pre-Meiji — the syncretic institutions themselves never produced a settled account, which is itself the strongest outside-the-beneficiaries evidence that the founding accommodation had no doctrinal core to begin with.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as substantial (0.68 by the interval's end) and rising over the story's near-millennium span, because the cost of the unresolved contradiction compounds: as combinatory institutions accumulate land, patronage networks, and professional guilds built on top of the ambiguity, the eventual cost of resolution (borne catastrophically at Meiji) grows in proportion to how entrenched the unexamined accommodation becomes. Theater ratio is authored as high and rising (0.40 to 0.71) because honji-suijaku doctrine increasingly functioned as ritual and administrative performance — invoked to justify continued dual patronage and combined worship — rather than as a doctrine anyone was prepared to defend under interrogation; the theatrical function grew as the practical convenience calcified into unquestioned custom. Suppression (0.58) is authored as moderate rather than extreme: this was not a violently enforced orthodoxy but a suppression of INQUIRY — social and institutional pressure against pressing the contradiction, not physical coercion, which is consistent with reform clergy being marginalized rather than persecuted prior to Meiji. Accessibility collapse is authored moderately (0.42) because alternative frameworks (Pure Shinto revival, doctrinally strict Buddhism) did exist throughout the period at the margins — they were disfavored, not eliminated — and resistance is authored as present but historically weak (0.35) until state power made resistance suddenly effective.
 *
 * DIRECTIONALITY LOGIC:
 *   The shrine-temple administrative complexes and syncretic ritual specialists sit at the beneficiary end: they derive ongoing legitimacy, revenue, and professional standing from the ambiguity's persistence, and their exit options (arbitrage, constrained-but-adaptive) reflect that they could shift their framing opportunistically as patrons required. Lay practitioners sit at the target end: trapped, powerless, and bearing the cost of unexamined contradiction across a lifetime without ever being offered a resolved account. Reformist clergy are also targets, but structurally distinct from lay practitioners — they possessed the doctrinal sophistication to see and name the incoherence, which is precisely what made them dangerous to the beneficiary institutions and got them marginalized rather than ignored.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as mere coordination-that-degraded (which would suggest an original working synthesis that later atrophied) versus what this reading claims: that there was no original coherent function to degrade from. The founding_problem was administrative/political integration, not theological resolution, and its status is dead, not merely obsolete — the practical problem of integrating Buddhism into the ritual landscape was solved centuries before Meiji, at which point the arrangement's function shifted entirely to shielding beneficiary institutions from a reckoning. The claimed_type is piton rather than snare precisely because no single concentrated beneficiary was extracting deliberately extractive rents through active enforcement — the shrine-temple complexes and ritual specialists benefited passively from inertia and low scrutiny, not through coercive maintenance of victims. This is why requires_active_enforcement is false: the arrangement needed no defender, only an absence of anyone with power to challenge it, which is the diagnostic signature of degraded/inertial persistence rather than pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_genuine_synthesis,
    'Did premodern Japanese practitioners actually hold contradictory propositional beliefs about kami-buddha identity, or does this reading impose a modern demand for propositional consistency onto a religious system that operated on entirely different (ritual/pragmatic rather than doctrinal) terms, in which case the ''incoherence'' is an artifact of applying Western/post-Meiji analytical categories retroactively?',
    'Close textual analysis of pre-Meiji honji-suijaku treatises and temple records to determine whether practitioners themselves treated the kami-buddha relationship as a live doctrinal question requiring resolution, versus whether doctrinal consistency was simply not a criterion by which the tradition judged itself — cross-referenced against comparative cases of syncretism elsewhere that did NOT collapse under external pressure.',
    'If practitioners never operated under a propositional-consistency framework, then ''incoherence'' is a category error imported by this reading, and the domain_partition or ontological_fusion readings become better descriptions of the tradition''s own self-understanding — this reading''s high ε would then reflect the reading''s imposed framework rather than a real suppressed cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_genuine_synthesis, conceptual, 'Whether propositional incoherence is a real feature of premodern practice or an anachronistic analytical imposition.').

omega_variable(
    meiji_rupture_vs_revelation,
    'Was Meiji shinbutsu-bunri (1868) a revelation of pre-existing latent incoherence, as this reading claims, or was it itself a constructed rupture driven by Meiji nation-building and anti-Buddhist political motives that manufactured a ''contradiction'' where none had previously been operative, then retroactively declared the prior arrangement to have been incoherent all along?',
    'Examine the political-motive record for Meiji state actors (kokugaku ideology, need to establish State Shinto as distinct from Buddhism for legitimation purposes) versus records of internal doctrinal dispute predating any state involvement, to assess whether the incoherence was locally recognized by practitioners themselves before 1868 or introduced by the reforming state''s framing.',
    'If the incoherence claim was substantially manufactured by Meiji political actors rather than a latent pre-existing condition, this reading''s core causal story (suppressed contradiction revealed) collapses into a different story (state-imposed rupture for unrelated political ends), which would make this reading closer to a post-hoc rationalization than an accurate historical account of the pre-Meiji equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_vs_revelation, empirical, 'Whether Meiji separation reveals prior incoherence or manufactures a contradiction retroactively for state-building purposes.').

omega_variable(
    beneficiary_class_precision,
    'Is the beneficiary class (shrine-temple administrative complexes, ritual specialists) accurately characterized as passive beneficiaries of inertia, or did some subset of these institutions actively resist doctrinal clarification efforts in ways that would push this constraint toward tangled_rope or snare rather than piton?',
    'Search for documented instances of shrine-temple complexes actively suppressing reformist clergy (as opposed to simply outcompeting them institutionally) — legal action, patronage withdrawal used punitively, or exclusion from ritual office as retaliation for doctrinal challenge.',
    'Evidence of active suppression would require requires_active_enforcement: true and reclassification consideration toward tangled_rope; the current piton classification depends on the suppression being diffuse and structural (absence of an enforcement body) rather than a deliberate strategy pursued by beneficiary institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_precision, empirical, 'Whether beneficiary institutions passively profited from inertia or actively suppressed doctrinal challenge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 400, 0.55).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 600, 0.6).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 800, 0.65).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1000, 0.69).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1200, 0.71).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 800, 0.63).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1000, 0.66).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1200, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__pragmatic_incoherence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__pragmatic_incoherence_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_shinbutsu_bunri_separation_edict).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the simultaneous_veneration kernel (shinbutsu-shugo). The ontological_fusion_reading authors low ε (honji-suijaku as genuine metaphysical insight, minimal suppressed cost); the domain_partition_reading authors low-to-moderate ε (functional specialization, coherent division of ritual labor); this pragmatic_incoherence_reading authors substantial and rising ε, treating the same historical arrangement as suppressed contradiction rather than functioning coordination. All three should link to a downstream meiji_shinbutsu_bunri_separation_edict constraint documenting the state-driven rupture itself, since each reading generates a different causal account of why that rupture happened and what it revealed or imposed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
