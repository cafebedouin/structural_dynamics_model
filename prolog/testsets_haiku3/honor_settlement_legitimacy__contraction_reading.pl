% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy — Contraction Reading (Cognitive Framework Exit)
 *   domain: social/cultural/legal history
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION READING of the
 *   honor-settlement-legitimacy kernel: dueling declined not through
 *   intensifying legal prohibition alone, but through cognitive framework
 *   transformation in which the very concept of dueling as a legitimate
 *   response to dishonor exited the normative possibility space. Under this
 *   reading, the constraint is the cultural-cognitive framework that makes
 *   dueling unthinkable — not a law against it, but a background shift in
 *   what counts as honorable conduct. Honor ceased to require combat and
 *   began to require legal, contractual, or institutional vindication. The
 *   constraint operates as a Mountain from the analytical seat: it is a
 *   genuine structural fact about how legitimacy shifted, not a constructed
 *   extraction mechanism. However, the transition involved massive asymmetric
 *   costs absorbed by those whose identity was locked to honor culture (the
 *   genteel classes who held dueling as constitutive of gentlemanly
 *   identity); they paid the identity-lock cost while bourgeois and legal
 *   institutions gained authority. The contraction reading emphasizes
 *   cognitive replacement, not merely prohibition with resistance ongoing
 *   elsewhere (drop reading) or overdetermined decline with multiple
 *   mechanisms (composite reading). From the honor-culture adherent seat, the
 *   constraint appears as extractive (identity-lock cost) and suppressive
 *   (loss of a crucial legitimacy mechanism). From the benefiting
 *   institutional seat, it appears as a genuine natural development (reason
 *   superseding tradition, law superseding private violence) — a Mountain.
 *   The engine computes this divergence from the structural data.
 *
 * KEY AGENTS:
 *   - honor_culture_adherents: Genteel classes (aristocracy, officer corps, professional gentlemen) whose identity was constituted through honor and whose primary conflict-resolution mechanism was dueling — bore massive identity-lock costs as the cognitive framework shifted.
 *   - bourgeois_institutional_order: Beneficiaries of the framework shift; consolidated authority as legal institutions became the monopoly dispute arbiters.
 *   - legal_rational_authority: Agenda-setters and validators of the new framework through legislation, legal theory, and institutional practice.
 *   - working_and_merchant_classes: Beneficiaries in that legal alternatives to dueling (always unavailable to them) were never required; they only gained from the expansion of legal remedies.
 *   - women_and_non_combatant_dependents: Excluded from the discourse but massive beneficiaries; they bore the structural harm of honor-based conflict (loss of male relatives in duels) and gained from cognitive replacement of the framework.
 *   - legal_scholars_and_reformers: Agenda-setters and propagators of the framework shift through writing and advocacy.
 *   - analytical_observer: External seat from which the transformation is visible as a wholesale replacement of one legitimacy framework with another.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.08).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy — Contraction Reading (Cognitive Framework Exit)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "social/cultural/legal history").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, 'e0666ab1-4991-4ec6-bac9-cf503053a2ac').
narrative_ontology:cs_kernel_codification('e0666ab1-4991-4ec6-bac9-cf503053a2ac', distributed).
narrative_ontology:cs_authority_grounding('e0666ab1-4991-4ec6-bac9-cf503053a2ac', extraction).
narrative_ontology:cs_interpretation_layer_present('e0666ab1-4991-4ec6-bac9-cf503053a2ac').
narrative_ontology:cs_reading_relation('e0666ab1-4991-4ec6-bac9-cf503053a2ac', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('e0666ab1-4991-4ec6-bac9-cf503053a2ac', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('e0666ab1-4991-4ec6-bac9-cf503053a2ac', foundational, honor_culture_cognitive_exit).
narrative_ontology:cs_axiom_status(honor_culture_cognitive_exit, holdable).
narrative_ontology:cs_axiom_grounding('e0666ab1-4991-4ec6-bac9-cf503053a2ac', honor_culture_cognitive_exit, deontological).
narrative_ontology:cs_axiom('e0666ab1-4991-4ec6-bac9-cf503053a2ac', foundational, legal_rationality_supersedes_private_settlement).
narrative_ontology:cs_axiom_status(legal_rationality_supersedes_private_settlement, holdable).
narrative_ontology:cs_axiom_grounding('e0666ab1-4991-4ec6-bac9-cf503053a2ac', legal_rationality_supersedes_private_settlement, instrumental).
narrative_ontology:cs_reference_frame('e0666ab1-4991-4ec6-bac9-cf503053a2ac', honor_culture_legitimacy_framework).
narrative_ontology:cs_drift_state('e0666ab1-4991-4ec6-bac9-cf503053a2ac', legal_rational_institutional_hegemony, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('e0666ab1-4991-4ec6-bac9-cf503053a2ac', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bourgeois_institutional_order).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, legal_rational_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, working_and_merchant_classes).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, women_and_non_combatant_dependents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held dueling as a legitimate (indeed, obligatory) means of settling disputes involving personal honor — particularly slights, accusations of cowardice, or questions of reputation. Their entire self-concept as gentlemen/officers was tied to the availability of the duel as a response to dishonor. When dueling became unthinkable (not merely illegal but conceptually incoherent within the new cultural framework), they faced a situation where their identity-constituted exit was no longer available: accepting the legal prohibition meant accepting a permanent reduction in status-maintenance options and absorbing insults without recourse. The cost was internalized: shame at being unable to respond traditionally, social erosion of the gentlemanly identity itself.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents, payer,
    moderate, biographical, identity_locked, national).

% Beneficiaries of the framework shift: as dueling lost legitimacy, the alternative settlement mechanisms (law courts, contractual arbitration, civil liability) became the default, channeling disputes into the legal system and away from private violence. This consolidated institutional authority and made the legal system the monopoly adjudicator of disputes, which transferred prestige and resources to the bourgeois-led institutional order. No extraction in the classic sense — the constraint benefits them because it reinforces the legitimacy of legal institutions over honor-based alternatives.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_institutional_order, beneficiary,
    institutional, generational, arbitrage, national).

% The institutional authority (courts, legislatures, legal profession) that propagates and enforces the new cognitive framework in which dueling is not merely prohibited but incomprehensible as a legitimate action. They set the terms of honorable conduct within civil society — no longer through private combat but through law and contract. They do not extract in the extractive-constraint sense; they validate and maintain the framework itself, which has become the background condition of legitimacy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_rational_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Beneficiaries of the cognitive shift insofar as it closes off a conflict-resolution mechanism that was never fully available to them (dueling was an aristocratic privilege; common people were not expected or permitted to settle disputes by duel). The expansion of legal remedies for defamation, slander, and breach of contract provided them recourse mechanisms that dueling never did. Their situation improves not through suppression but through expansion of legitimate alternatives.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, working_and_merchant_classes, beneficiary,
    organized, generational, constrained, national).

% Had no formal voice in honor settlement disputes (duels were male-to-male, honor violations to male honor) yet bore massive costs: loss of husbands, fathers, and sons in fatal duels over issues they were not party to. The framework shift that made dueling unthinkable directly removed this structural harm. They gain from the shift but were excluded from the cognitive process that generated it and held no power to enforce it; their relief came as a side effect of the elite cultural transformation.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, women_and_non_combatant_dependents, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, women_and_non_combatant_dependents, excluded).

% Active propagators of the framework shift: through writing, teaching, advocacy, and legislative reform, they delegitimized dueling as incompatible with rational legal order and modern citizenship. Their role straddles agenda-setting (they shaped the new framework) and observation (they documented the cognitive transition and theorized why it occurred). They justified the shift through Enlightenment-era arguments about reason, contract, and the rule of law.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_scholars_and_reformers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, legal_scholars_and_reformers, observer).

% External analytical vantage point from which the transformation appears as a wholesale replacement of one legitimacy framework (honor culture, private settlement through combat) with another (legal rationality, public settlement through law). Neither imposed by force alone nor chosen individually by agents, but absorbed through cultural transmission: a generation grew up where dueling was already beyond the cognitive possibility set.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, bourgeois_institutional_order).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced an honor-based conflict settlement mechanism (dueling) with legal-rational ones (courts, arbitration, written contract). The coordination problem: how do gentlemen resolve disputes over honor and reputation without private violence? Answer under honor culture: combat. Answer under the new framework: litigation, legal remedy, formal apology with damages.
% TRANSFER_FUNCTION: The new framework transfers legitimacy and dispute-resolution authority from private combatants to legal institutions. Honor-satisfaction moves from violence to victory-in-court or damages awarded. Prestige moves from martial prowess to legal acuity and social standing based on contract-keeping rather than combat-proven courage.
% ABSENT_VOICES: Residual honor-culture practitioners would object that the framework eliminates a vital mechanism for maintaining personal dignity and enforcing reputation — that legal remedies are insufficient because they do not restore honor in the way combat does (victory in court is external validation, not the personal reaffirmation of courage that a won duel provides). They are largely absent from the discourse that transforms the framework because they are already on the losing side of the cultural shift; their objections appear as antiquated, aristocratic attachment rather than as live alternatives to be debated.
% DISAPPEARANCE_RATIONALE: If the cognitive framework shift had never occurred and dueling remained thinkable as a legitimate response to dishonor, the entire institutional structure of legal remedy for defamation, slander, and breach of reputation would likely not have developed in the same form; courts would not have become the monopoly arbiters of honor disputes; the legal profession would not have consolidated the authority it did. The disappearance would require a reinvention of alternative settlement mechanisms — a return to honor culture or the invention of something structurally similar. The world under dueling-as-legitimate would have developed differently from the start.
% FOUNDING_PROBLEM: Early modern (16th–18th century) honor culture provided no institutional mechanism for resolving disputes over reputation and personal standing except through combat or formal apology by the offender. When apology was refused or insufficient, combat was the residual option. The founding problem: how to channel these disputes into a system that does not require or permit private violence?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists outside the benefiting institutions (bourgeois/legal) attest that the founding problem is substantially solved: legal remedies for defamation and libel now provide institutional channels for reputation disputes; the problem of private violence as the residual settlement mechanism has been replaced. Honor-culture practitioners might contest this, arguing the problem is merely relocated (no combat, but social death remains; the mechanism changed but not the stakes). However, the overwhelming testimonial consensus from legal institutions, legislatures, popular discourse, and emerging legal scholarship across multiple European and colonial jurisdictions all attest the problem is dead.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series traces the declining extractiveness and suppression requirement over 200 years (roughly 1780–1980, the transition period from honor culture to legal rationality as the default framework). At t=0 (early transition), the constraint exhibits high extractiveness (0.65) from the honor-culture-adherent seat because they experience the loss of a core legitimacy mechanism; suppression is substantial (0.42) because the new framework requires active policing (laws against dueling, social stigma, institutional pressure to use legal remedies instead). Theater is high (0.38) because a great deal of energy goes into establishing the legitimacy of the new framework through rhetoric and reform. By t=200 (contemporary), the constraint exhibits low extractiveness (0.15), suppression (0.08), and theater (0.12) because the framework is no longer contested: dueling is genuinely unthinkable, not even present as a temptation requiring suppression. The residual extractiveness reflects the identity-lock cost borne by any historical actor still socialized into honor culture and now living in a world where that identity has no institutional support — but this is residual, not the active constraint. Accessibility collapse is very high (0.92): once you accept the new framework (law, contract, reason), the old option (dueling) is not merely forbidden but conceptually unavailable. Resistance is very low (0.05) in contemporary times because the framework is now the default; residual honor-culture practitioners are marginal and their resistance appears as anachronistic, not as live contestation. The claim is mountain; the metrics describe a genuine natural-law appearance after the transition completes, but the transition itself involved massive asymmetric costs to one identity group.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between payer and beneficiary seats should be significant: from the honor-culture-adherent seat (high power, identity_locked exit), the constraint appears as extractive suppression — the loss of a core legitimacy mechanism for which there is no adequate substitute when your entire identity depends on it. Legal remedies do not restore honor the way a won duel does; they are external validation, not internal reaffirmation. From the beneficiary seats (bourgeois institutional order, legal rational authority), the constraint appears as a genuine natural development: reason replacing tradition, law replacing private violence, institutional order replacing aristocratic chaos. The divergence is fundamental because it reflects whether the agent's identity is anchored in the old framework (adherents) or the new one (institutional order). The engine should compute this from: (1) the identity-lock vs. arbitrage exit options differentiating the seats, (2) the beneficiary/victim declarations, (3) the accessibility collapse (very high, so once you accept the new frame, the old is unavailable), and (4) the temporal measurement showing extractiveness declining to near-zero as the framework becomes universal.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture adherents: high power (genteel classes, officer corps) but identity_locked exit (cannot abandon gentlemanly identity without social death). They are targets relative to the constraint (d near 1.0) because the new framework extracts their core legitimacy mechanism. Bourgeois institutional order: institutional power, arbitrage exit (can adapt their framework, design new institutions, control the discourse). They are beneficiaries (d near 0.0) because the constraint consolidates their institutional authority. Legal rational authority: institutional power, arbitrage exit (they control the machinery of the new framework). They are agenda-setters (d near 0.0). Working and merchant classes: organized power, constrained exit (cannot return to honor culture, but also never depended on dueling). They are beneficiaries with low cost (d = 0.1–0.2) because the expansion of legal remedies benefits them but was not imposed against their resistance. Women and non-combatant dependents: powerless, trapped exit (no voice in the original framework, no choice about the transition). They are massive beneficiaries (d near 0.0) because they bore the harm of the old framework and gain from the new one, but their seat is excluded from the discourse that generates the transition. Legal scholars and reformers: institutional power, analytical exit (they control the frame-shifting discourse). They are agenda-setters (d near 0.0). The core asymmetry: identity_locked adherents (high d, high extraction cost) vs. institutional beneficiaries (low d, gain from the shift). The engine should detect this from the stakeholder data alone.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy violation under this reading. The founding problem (how to channel honor disputes into a system that does not require private violence) was genuine, and the constraint (framework shift) genuinely solves it. The problem's status is dead: legal remedies now provide channels for reputation disputes; private violence as a residual settlement mechanism has been replaced. Under the drop_reading, mandatrophy might arise: if dueling persisted as a fringe practice, the problem would be only partially solved and the constraint would be treating the symptom (suppressing the practice) rather than solving the problem (providing alternatives). But under contraction_reading, the problem is solved because the cognitive framework has shifted — there is no longer a residual demand for dueling, no frustrated honor-culture adherents secretly waiting to challenge each other; the mechanism has exited the possibility space entirely. The constraint operates as natural law (once the framework shifts, dueling is as unthinkable as trial-by-ordeal) rather than as maintained extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_framework_vs_legal_prohibition,
    'Was the exit of dueling from the cognitive possibility space a genuine cultural transformation, or was it enforced by legal prohibition and institutional pressure that created the appearance of transformation?',
    'Discourse analysis tracking the conceptual vocabulary of honor, legitimacy, and courage across legal, literary, academic, and popular sources over the transition period. If the new framework was genuinely adopted, non-legal discourse (literature, private correspondence, academic philosophy) should show the shift independently of legal prohibition. If prohibition alone drove the change, legal discourse would shift first and non-legal discourse would lag or show resistance.',
    'If genuine cognitive transformation: the constraint is a Mountain (natural development of thought, reason replacing tradition). If enforced by prohibition: the constraint is a Tangled Rope (real coordination function — replacing private violence with legal settlement — paired with asymmetric extraction of honor-culture identity). The classification changes fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_framework_vs_legal_prohibition, empirical, 'Whether dueling''s exit from legitimacy was autonomous cognitive shift or enforced by prohibition creating appearance of cognitive shift.').

omega_variable(
    contraction_vs_drop_reading_differentiation,
    'Did dueling truly exit the cognitive possibility space for the entire affected population, or did it persist as a live alternative within residual honor-culture communities?',
    'Genealogical and anthropological tracking of honor-culture practices in different geographic and class contexts through the transition period. If contraction is true, dueling should disappear entirely as a live practice and its memory should become purely historical (unthinkable as a present option). If drop_reading is true, isolated communities or subgroups should maintain dueling or its functional equivalents as a live practice even after the cognitive framework shifts in mainstream institutions.',
    'If contraction is true: the constraint is structurally a Mountain; the framework genuinely replaced the old one. If drop_reading is true: the constraint is a Snare (the new framework is imposed on an unsuppressed residual practice; extraction arises from suppressing the alternative). The difference determines whether the constraint operates as natural law or as maintained extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contraction_vs_drop_reading_differentiation, empirical, 'Whether honor culture was wholly replaced (contraction) or partly marginalized (drop reading).').

omega_variable(
    identity_lock_cost_asymmetry,
    'For honor-culture adherents with identity_locked exit, is the measured extractiveness (0.65 at t=0, declining to 0.15 at t=200) a structural cost of the framework shift or a side effect of institutional preference and propaganda?',
    'Biographical and clinical analysis: do agents who maintain honor-culture identity while living under the new framework show persistent psychological or social costs consistent with extraction (loss of mechanism, inability to respond to dishonor, permanent status reduction)? Or do they adapt by reinterpreting honor in legal/institutional terms (integration rather than cost)? A third possibility: some adapt, some don''t, and the extractiveness reflects the proportion who don''t and the intensity of their loss.',
    'If extractiveness is genuine structural cost: the constraint extracts from identity-locked agents even as it appears natural to institutional beneficiaries (classic asymmetric extraction). If extractiveness is primarily identity choice: the constraint is more neutral (agents who maintain the old frame bear their own cost, not extraction by the new frame). This affects whether the constraint is a false summit (claimed mountain with hidden beneficiaries) or a genuine natural law with asymmetric incidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cost_asymmetry, conceptual, 'Whether the identity-lock cost for honor-culture adherents is extractive (system-imposed) or integrative (identity-chosen).').

omega_variable(
    kernel_reading_underdetermination,
    'Is the contraction_reading the best characterization of this constraint''s structure, or is the composite_reading more accurate (decline overdetermined by multiple reinforcing mechanisms, of which contraction is one)?',
    'Structured historical analysis isolating the causal weight of each mechanism: (1) cognitive framework replacement (contraction), (2) legal prohibition and enforcement, (3) democratization eroding aristocratic privilege, (4) bourgeois cultural hegemony, (5) religious reform delegitimizing private violence. If one mechanism explains most of the decline, contraction_reading is appropriate. If multiple mechanisms are necessary and reinforcing, composite_reading is appropriate.',
    'If contraction dominates: the constraint is a Mountain (natural cognitive development). If composite with multiple mechanisms: the constraint is a Tangled Rope (real coordination problem — replacing private violence — paired with multiple mechanisms of extraction by bourgeois order). The classification shifts; the structural reading of the kernel changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether contraction reading or composite reading better captures the actual structure of the decline.').

omega_variable(
    false_summit_candidate_legitimacy,
    'Does the constraint''s appearance as a Mountain (natural development of thought, reason replacing tradition) mask underlying institutional interests? Are bourgeois and legal institutions genuine beneficiaries or merely the carriers of a natural cognitive shift?',
    'Institutional history tracking how legal institutions actively promoted the new framework through legislation, education, and cultural authority. If promotion was incidental to the cognitive shift, the constraint is a genuine Mountain. If legal institutions strategically cultivated the new framework to consolidate their authority, the constraint is a false summit: an institutional capture masquerading as natural law.',
    'If genuine Mountain: the constraint operates as natural law; asymmetric costs to honor-culture adherents reflect only the incidence of natural change, not extraction. If false summit: the constraint is a Tangled Rope (genuine coordination function — replacing private violence — paired with institutional capture and strategic promotion for authority consolidation). The measured beneficiaries become relevant; their declarations activate the FSM gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate_legitimacy, conceptual, 'Whether the mountain claim is genuine or masks institutional capture and strategic promotion by beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_contraction_tr_t0, honor_settlement_legitimacy__contraction_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(honor_contraction_tr_t25, honor_settlement_legitimacy__contraction_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(honor_contraction_tr_t50, honor_settlement_legitimacy__contraction_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(honor_contraction_tr_t100, honor_settlement_legitimacy__contraction_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement(honor_contraction_tr_t150, honor_settlement_legitimacy__contraction_reading, theater_ratio, 150, 0.13).
narrative_ontology:measurement(honor_contraction_tr_t200, honor_settlement_legitimacy__contraction_reading, theater_ratio, 200, 0.12).

% Extraction over time
narrative_ontology:measurement(honor_contraction_be_t0, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(honor_contraction_be_t25, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(honor_contraction_be_t50, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(honor_contraction_be_t100, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(honor_contraction_be_t150, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 150, 0.16).
narrative_ontology:measurement(honor_contraction_be_t200, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(honor_contraction_su_t0, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(honor_contraction_su_t25, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 25, 0.32).
narrative_ontology:measurement(honor_contraction_su_t50, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement(honor_contraction_su_t100, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement(honor_contraction_su_t150, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 150, 0.09).
narrative_ontology:measurement(honor_contraction_su_t200, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the honor_settlement_legitimacy kernel family. The kernel is the cultural-institutional arrangement for settling disputes involving personal honor. Three constraint stories instantiate three different readings: contraction_reading (this story) holds that dueling exited the cognitive possibility space entirely through framework replacement; drop_reading holds that dueling persisted as a fringe practice among residual honor-culture adherents; composite_reading holds that decline was overdetermined by multiple reinforcing mechanisms. These are not the same constraint viewed from different angles; they are three structurally distinct claims with different ε values and different beneficiary/victim structures. The contraction_reading (this story) treats the framework shift as a Mountain (natural cognitive development). The drop_reading treats residual dueling as a Snare (suppressed practice with ongoing resistance). The composite_reading treats the overdetermined decline as a Tangled Rope (genuine coordination problem paired with multiple extraction mechanisms). All three readings share the same referent (the historical decline of dueling) but instantiate different structural claims about it. Network links reflect the dependency: contraction_reading influences both siblings (if the framework genuinely contracted, residual practice is not a live alternative); drop_reading coexists with composite_reading (both accept ongoing residual practice, differing only in whether decline is overdetermined or singular); composite_reading influences both others (if decline is overdetermined, the pure contraction mechanism is one component rather than the whole story).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__contraction_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
