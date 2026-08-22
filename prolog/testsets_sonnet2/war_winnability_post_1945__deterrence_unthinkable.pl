% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence Unthinkability Doctrine (MAD Reading of Great-Power War)
 *   domain: strategic_studies/nuclear_deterrence_theory/international_relations
 *
 * SUMMARY:
 *   This story instantiates the deterrence_unthinkable reading of the
 *   war_winnability_post_1945 kernel: the claim that thermonuclear weapons
 *   and assured mutual destruction moved great-power total war entirely out
 *   of the space of coherent strategic objectives, such that 'planning to
 *   win' a direct nuclear war between great powers became a category error
 *   rather than merely a harder problem. This is distinct from the
 *   countervailing_thinkable reading (which holds limited victory remains
 *   achievable via counterforce) and the rhetorical_contraction reading
 *   (which holds the unwinnability claim is a discursive taboo layered atop
 *   unchanged operational planning). Under THIS reading's own lights, the
 *   standing arrangement is the arms-control-era deterrence doctrine as
 *   actually practiced — declaratory unwinnability paired with real
 *   institutional costs to military establishments whose occupational mission
 *   it hollows out. The tangled_rope classification reflects that this
 *   reading sees a genuine coordination function (crisis stability, avoidance
 *   of civilizational catastrophe) riding alongside a real asymmetric cost
 *   (military establishments and planning officers bear mission incoherence)
 *   enforced through arms-control institutions, doctrine review, and civilian
 *   oversight of nuclear posture.
 *
 * KEY AGENTS:
 *   - civilian_populations: primary beneficiary (powerless/trapped) — protected by the doctrine holding, has no independent lever
 *   - arms_control_epistemic_community: co-beneficiary and agenda_setter (organized/mobile) — professional and institutional stake in the unwinnability premise
 *   - military_establishments: primary payer (institutional/constrained) — mission incoherence, budget retained but victory referent removed
 *   - war_planning_officer_corps: individual payer (moderate/identity_locked) — career identity built on a deliverable the doctrine renders incoherent
 *   - national_command_authorities: agenda_setter (institutional/trapped) — must enforce the doctrine while remaining bound by its own logic
 *   - counterforce_theorists: excluded voice (organized/mobile) — the sibling reading's constituency, treated as inconsistency here
 *   - historians_of_nuclear_strategy: analytical observer — arbitrates via archival record which reading fits the evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.42).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.58).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.42).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence Unthinkability Doctrine (MAD Reading of Great-Power War)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence_theory/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'db7028a8-b4f4-41f1-b1c1-908930554977').
narrative_ontology:cs_kernel_codification('db7028a8-b4f4-41f1-b1c1-908930554977', distributed).
narrative_ontology:cs_authority_grounding('db7028a8-b4f4-41f1-b1c1-908930554977', expertise).
narrative_ontology:cs_interpretation_layer_present('db7028a8-b4f4-41f1-b1c1-908930554977').
narrative_ontology:cs_reading_relation('db7028a8-b4f4-41f1-b1c1-908930554977', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('db7028a8-b4f4-41f1-b1c1-908930554977', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('db7028a8-b4f4-41f1-b1c1-908930554977', foundational, victory_requires_survivable_outcome).
narrative_ontology:cs_axiom_status(victory_requires_survivable_outcome, holdable).
narrative_ontology:cs_axiom_grounding('db7028a8-b4f4-41f1-b1c1-908930554977', victory_requires_survivable_outcome, empirically_contingent).
narrative_ontology:cs_axiom('db7028a8-b4f4-41f1-b1c1-908930554977', foundational, mutual_destruction_forecloses_all_victory_categories).
narrative_ontology:cs_axiom_status(mutual_destruction_forecloses_all_victory_categories, holdable).
narrative_ontology:cs_axiom_grounding('db7028a8-b4f4-41f1-b1c1-908930554977', mutual_destruction_forecloses_all_victory_categories, empirically_contingent).
narrative_ontology:cs_reference_frame('db7028a8-b4f4-41f1-b1c1-908930554977', pre_nuclear_clausewitzian_war_aims).
narrative_ontology:cs_drift_state('db7028a8-b4f4-41f1-b1c1-908930554977', post_mutual_assured_destruction_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('db7028a8-b4f4-41f1-b1c1-908930554977', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, arms_control_epistemic_community).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, war_planning_officer_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear zero formal decision power over nuclear posture but are the constituency whose survival the unthinkability doctrine is meant to secure. They cannot exit the risk of great-power war except through the doctrine holding; they have no independent enforcement capacity and depend entirely on the belief structure being maintained by others.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Strategists, treaty negotiators, and academic deterrence theorists whose professional standing, funding, and institutional relevance depend on the premise that nuclear war is categorically unwinnable and must be prevented rather than fought. They author doctrine, staff arms-control institutions, and shape war-college curricula around the unthinkability premise; their expertise is the interpretive layer that keeps the doctrine coherent.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_epistemic_community, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, arms_control_epistemic_community, agenda_setter).

% Services and general staffs whose core professional mission — planning to fight and win wars — becomes structurally incoherent once total war against another nuclear power is declared unwinnable in principle. They still receive budgets and maintain forces, but the doctrine strips their central occupational logic of a victory referent, forcing continuous internal justification of readiness for a war their own doctrine says cannot be won.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, constrained, national).

% Individual planning officers trained to produce war-winning options for national command authorities. Their career identity is built on operational planning competence; the unthinkability doctrine renders their core deliverable — a plan for victory — a contradiction in terms, forcing them to either quietly plan for outcomes officially denied to exist or reframe their careers around damage limitation and escalation management instead of victory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, war_planning_officer_corps, payer,
    moderate, biographical, identity_locked, national).

% Heads of state and their nuclear command structures who must simultaneously maintain the unthinkability doctrine publicly (to sustain deterrence credibility and crisis stability) and retain some operational capacity for decision under crisis. They administer the doctrine's enforcement through declaratory policy, arms control agreements, and command-and-control architecture, but are themselves bound by the doctrine's logic — they cannot credibly threaten a war they have declared unwinnable without undermining the very deterrent the doctrine protects.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, national_command_authorities, agenda_setter,
    institutional, immediate, trapped, global).

% Strategists who argue limited nuclear war and damage-limiting counterforce strategies remain operationally coherent and that 'unwinnable' overstates the actual structural situation. Their position is marginalized in mainstream deterrence discourse and treaty-era institutions, though it persists in war-college planning documents and targeting doctrine that the unthinkability reading treats as inconsistent residue rather than a live alternative.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_theorists, excluded,
    organized, generational, mobile, national).

% Study declassified war plans, targeting doctrine, and declaratory statements to assess whether the unwinnability claim describes actual strategic planning or only public rhetoric. Their access to archival material (SIOP revisions, NATO flexible response documents) makes them the primary source of evidence bearing on whether this reading or its rivals better fits the historical record.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, historians_of_nuclear_strategy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, diffuse).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great-power behavior around avoiding any war between nuclear-armed states, by making mutual destruction so certain and so total that victory ceases to be a coherent planning objective for any party — this removes an entire class of catastrophic outcomes from the reachable strategic space.
% TRANSFER_FUNCTION: Moves strategic legitimacy and institutional mission away from military establishments organized around war-winning and toward the arms-control and deterrence-theory community organized around war-prevention; moves existential risk reduction to civilian populations at the cost of occupational coherence for war planners.
% ABSENT_VOICES: Counterforce theorists and elements of the operational planning community who maintain that limited nuclear war remains winnable are structurally excluded from the doctrine's own self-description — their continued production of targeting plans is treated by this reading as doctrinal inconsistency rather than a live counter-claim.
% DISAPPEARANCE_RATIONALE: If the unwinnability premise were abandoned and great-power planners genuinely believed victory in total nuclear war was achievable, crisis stability logic collapses: first-strike incentives strengthen, arms racing intensifies, and civilian populations lose the structural protection the doctrine currently provides. Military establishments would regain a coherent war-winning mission, but at the cost of removing the central mechanism credited with preventing great-power war since 1945.
% FOUNDING_PROBLEM: The advent of thermonuclear weapons and assured second-strike capability meant that, for the first time, a full military victory by either great power in a direct war would not preserve the victor's society — the founding problem was reconciling the existence of war-fighting military establishments with an outcome space that no longer contained a survivable victory.
% FOUNDING_PROBLEM_CORROBORATION: Arms control theorists and civilian policy communities (independent of military budgets) attest the founding problem remains live — no technological or doctrinal development has restored a survivable victory outcome for great-power nuclear war. Military planning communities and counterforce theorists, from outside the arms-control beneficiary group, contest this, citing continued targeting doctrine, damage-limitation planning, and declared willingness to fight limited nuclear wars as evidence the founding problem was reframed rather than solved.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).
:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the primary cost imposed on military establishments and planning officers is mission incoherence and identity strain, not material expropriation — a real but bounded cost. Suppression (0.58) is substantial: maintaining the unwinnability doctrine requires active enforcement against alternative framings (declaratory policy discipline, arms-control treaty compliance, war-college curriculum control) that actively work to keep 'winnable nuclear war' out of legitimate planning discourse. Theater ratio rose from 0.10 in 1945 to 0.38 by 2025, tracking the growing gap between declared unwinnability and the persistence of targeting doctrine and continuity-of-government planning that the historians' record documents — some of the doctrine's maintenance work is now performative rather than operationally binding. Accessibility collapse is high (0.72): once the mutual-destruction logic is understood, alternative framings of nuclear war as a winnable enterprise become very difficult to hold credibly, though not fully closed off (hence not near-mountain levels).
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations sit at the strong-beneficiary end (d near 0): they bear the catastrophic tail risk the doctrine suppresses but have no exit and no enforcement role, and the derivation correctly reads them as subsidized by the arrangement holding. The arms-control epistemic community also sits near the beneficiary end but with agency the civilians lack, reflected in organized power and mobile exit. Military establishments and the officer corps sit near the target end: their institutional and career logics are directly disrupted by the doctrine's premise, and their exit options (constrained institutionally, identity_locked individually) prevent them from simply opting out of the incoherence. National command authorities occupy an unusual dual position — administering the doctrine's enforcement while being bound by its logic themselves — captured here as agenda_setter with trapped exit, since they cannot exit the doctrine without undermining the deterrent they rely on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem status is authored as contested rather than resolved, precisely to avoid mislabeling this doctrine as either pure obsolete mandatrophy (military establishments simply clinging to a dead mission) or pure live coordination (as the arms-control community would have it). The R5 corroboration draws on sources outside both benefiting groups: historians with archival access to actual targeting doctrine provide the evidence base that could resolve whether the founding problem persists in fact or has been quietly reframed into damage-limitation planning that the unwinnability doctrine's public face does not acknowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_declaratory_unwinnability,
    'Does the unwinnability premise actually govern classified war planning (SIOP-successor documents, targeting doctrine, continuity-of-government plans), or does it govern only declaratory policy and public discourse while operational planning proceeds on different, undisclosed assumptions?',
    'Comparative analysis of declassified targeting doctrine and continuity-of-government planning across the interval against declaratory statements from the same administrations; convergence supports this reading, divergence supports the rhetorical_contraction sibling reading.',
    'If operational planning has consistently diverged from declared unwinnability, this reading''s claim that winnability truly exited the reachable space is undermined, and the constraint''s true structural home is closer to rhetorical_contraction — a discursive taboo layered over unchanged operational planning, which would substantially raise the effective theater_ratio and lower confidence in the tangled_rope classification''s coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_declaratory_unwinnability, empirical, 'Whether the unwinnability claim describes actual planning practice or only public doctrine — the central fact distinguishing this reading from rhetorical_contraction.').

omega_variable(
    counterforce_coherence_ambiguity,
    'Is limited nuclear war through counterforce targeting a genuinely coherent strategic option that this reading wrongly treats as incoherent, or is ''limited nuclear war'' itself an unstable category that collapses toward total war under escalation dynamics?',
    'Escalation-dynamics modeling and historical crisis analysis (Cuban Missile Crisis, 1983 Able Archer, India-Pakistan crises) assessing whether limited nuclear exchanges have historically stayed limited or exhibited escalation pressure toward totality.',
    'If limited exchanges are shown to reliably escalate, this reading''s categorical unwinnability claim is strengthened relative to the countervailing_thinkable sibling; if limited exchanges are shown to be stable and containable, the sibling reading gains support and this reading''s premise weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterforce_coherence_ambiguity, conceptual, 'Whether the boundary between ''limited'' and ''total'' nuclear war is stable enough to sustain the countervailing sibling''s coherent-victory claim, which this reading denies.').

omega_variable(
    military_establishment_adaptation,
    'Have military establishments genuinely internalized mission incoherence (bearing a real structural cost as this reading claims), or have they successfully reframed their mission around escalation management and damage limitation in a way that restores occupational coherence without requiring the unwinnability premise to be false?',
    'Institutional analysis of war-college curricula, promotion criteria, and doctrine publications over the interval to assess whether officer identity and career structures reorganized around new coherent objectives (deterrence maintenance, crisis management) rather than persisting in unresolved incoherence.',
    'If reframing succeeded, the victim designation for military establishments should be weakened or narrowed to a transitional cost already substantially absorbed, lowering the authored extractiveness; if reframing failed or remains incomplete, the victim designation and current extractiveness level are supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_establishment_adaptation, empirical, 'Whether the mission-incoherence cost to military establishments is an enduring structural feature or a transitional cost largely resolved through institutional adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.18).
narrative_ontology:measurement(war__tr_t1975, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1975, 0.27).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1991, 0.32).
narrative_ontology:measurement(war__tr_t2008, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.22).
narrative_ontology:measurement(war__be_t1975, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1975, 0.31).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1991, 0.36).
narrative_ontology:measurement(war__be_t2008, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2008, 0.39).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1962, 0.48).
narrative_ontology:measurement(war__su_t1975, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(war__su_t2008, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the war_winnability_post_1945 kernel, decomposed per the ε-invariance principle because the natural-language claim 'nuclear weapons made great-power war unwinnable' covers structurally distinct claims with different ε values: (1) deterrence_unthinkable (this story) claims war-winnability exited the reachable strategic space entirely — an operational contraction, moderate ε reflecting real institutional costs to military establishments; (2) countervailing_thinkable claims limited victory remains achievable via counterforce, which would carry a different beneficiary/victim structure (favoring war-planning establishments) and likely lower ε for those establishments; (3) rhetorical_contraction claims the contraction is discursive only, with operational planning unchanged underneath — this would show a HIGH theater_ratio and a different extraction profile (the taboo itself becomes the extractive mechanism, protecting continued war-planning behind changed language). All three share the same kernel text (nuclear-era strategic doctrine) but diverge sharply in what they claim actually happened, so each requires its own ε, its own claimed_type, and its own stakeholder structure rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
