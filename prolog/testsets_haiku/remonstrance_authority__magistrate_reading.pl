% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Authority of French Parlements (Magistrate Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The remonstrance right of the French Parlements was a constitutional
 *   mechanism by which magistrates could formally object to and delay the
 *   registration of royal fiscal edicts. Under the magistrate reading, this
 *   right represents a fundamental check on arbitrary executive power,
 *   protecting ancient liberties and the rule of law. Under the crown reading
 *   (sibling constraint), it represents an illegitimate minoritarian veto
 *   protecting feudal privilege. The two readings share the same constraint
 *   kernel—the institutional power to remonstrate—but differ fundamentally on
 *   whether that power is a constitutional bulwark or a feudal obstruction.
 *   The magistrate reading presented here author-instantiates the constraint
 *   as a genuine coordination mechanism (edicts must be debated and justified
 *   before law) that has become substantially extractive because the
 *   Parlement uses its constitutional authority to defend tax exemptions that
 *   benefit the magistracy while shifting burden to commoners. This reading
 *   does not claim that the coordination function is false; it claims the
 *   constraint simultaneously solves a real coordination problem and enables
 *   asymmetric extraction.
 *
 * KEY AGENTS:
 *   - parlementary_magistracy: The institutional agenda-setter (institutional power) with identity-locked exit — their professional and social identity fuses with the role of constitutional guardians.
 *   - crown_fiscal_authority: The payer (powerful, constrained exit) — must navigate remonstrance before implementing reform; bears the cost of delay and modification.
 *   - commoners_bearing_tax_burden: The primary victims (powerless, trapped) — benefit from constitutional review but lose from magistracy exemptions; have no voice in the Parlement.
 *   - tax_exempt_nobility_of_the_robe: Secondary beneficiary (organized power, constrained exit) — corporate interests defended by remonstrance right protecting their exemptions.
 *   - royal_reform_coalition: Payers within the crown apparatus (powerful, constrained exit) — reform-minded officials whose modernization efforts are structurally obstructed.
 *   - philosophical_opposition: Excluded analytical seat — Enlightenment critics outside the formal remonstrance process who frame the right as feudal obstruction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.72).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Authority of French Parlements (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional/political").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, 'b1685c2e-2a16-48da-9009-34670844a4b8').
narrative_ontology:cs_kernel_codification('b1685c2e-2a16-48da-9009-34670844a4b8', fixed_text).
narrative_ontology:cs_authority_grounding('b1685c2e-2a16-48da-9009-34670844a4b8', lineage).
narrative_ontology:cs_interpretation_layer_present('b1685c2e-2a16-48da-9009-34670844a4b8').
narrative_ontology:cs_reading_relation('b1685c2e-2a16-48da-9009-34670844a4b8', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('b1685c2e-2a16-48da-9009-34670844a4b8', foundational, remonstrance_constitutionally_fundamental).
narrative_ontology:cs_axiom_status(remonstrance_constitutionally_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('b1685c2e-2a16-48da-9009-34670844a4b8', remonstrance_constitutionally_fundamental, deontological).
narrative_ontology:cs_axiom('b1685c2e-2a16-48da-9009-34670844a4b8', foundational, magistracy_guardian_of_ancient_liberties).
narrative_ontology:cs_axiom_status(magistracy_guardian_of_ancient_liberties, holdable).
narrative_ontology:cs_axiom_grounding('b1685c2e-2a16-48da-9009-34670844a4b8', magistracy_guardian_of_ancient_liberties, conventional).
narrative_ontology:cs_reference_frame('b1685c2e-2a16-48da-9009-34670844a4b8', ancient_constitutional_liberty).
narrative_ontology:cs_drift_state('b1685c2e-2a16-48da-9009-34670844a4b8', enlightenment_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1685c2e-2a16-48da-9009-34670844a4b8', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlementary_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_nobility_of_the_robe).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_revenue_authority).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, commoners_bearing_tax_burden).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, commoners_bearing_tax_burden).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, royal_reform_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parlements (particularly the Parlement de Paris) possess the right of remonstrance: the power to formally object to and delay royal fiscal edicts before registration. The magistrates view this as a constitutional check protecting ancient liberties and the rule of law against arbitrary innovation. Their collective identity fuses with the role of guardians of constitutional order. They exercise this right to block tax increases, maintain their own tax exemptions, and challenge centralization of royal authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlementary_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).

% The crown (represented by the king and his financial ministers) seeks to implement fiscal reforms, increase tax revenue, and modernize state finances. The remonstrance right forces the crown to justify every significant fiscal innovation before the Parlements, creating delay and uncertainty. When remonstrations succeed, the crown is either forced to withdraw the edict or override the Parlement by lit de justice, both of which are costly and weaken its authority claim.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_authority, payer,
    powerful, biographical, constrained, national).

% Commoners and non-exempt merchants bear the actual tax burden. They benefit indirectly when remonstrations block unfair tax increases, but they pay the cost when remonstrations maintain exemptions for the magistracy and nobility of the robe, shifting fiscal burden downward. They have no voice in the Parlement and cannot mount effective resistance to either the magistracy or the crown.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, commoners_bearing_tax_burden, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, commoners_bearing_tax_burden, beneficiary).

% The nobility of the robe—magistrates and office-holders—benefit structurally from remonstrance right because their corporate tax exemptions are defended and reaffirmed each time a remonstration blocks a universal tax reform. The constraint protects their privileged fiscal status by making it difficult to impose taxes on the magistracy without their consent.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_exempt_nobility_of_the_robe, beneficiary,
    organized, generational, constrained, national).

% Economic reformers and enlightenment-minded officials within the crown apparatus seek to eliminate feudal exemptions and create uniform taxation. Remonstrance right is their structural constraint: every reform attempt must navigate or overcome Parlementary objection, and the Parlements' corporate interest in maintaining exemptions makes this negotiation asymmetrical and slow.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_reform_coalition, payer,
    powerful, biographical, constrained, national).

% Enlightenment philosophers and legal theorists (outside the Parlement) argue that remonstrance right is an illegitimate corporate veto protecting feudal privilege under the language of constitutional liberty. They advocate for royal absolutism as a modernizing force that can break aristocratic obstruction. Their critique is excluded from the formal remonstrance process but shapes intellectual context.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, philosophical_opposition, excluded,
    analytical, civilizational, analytical, national).

% Observes the constraint from outside: tracks how remonstrance right functions as both coordination mechanism (law must be debated before registration) and extraction mechanism (exemptions are defended by the same right that upholds rule of law).
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, historical_analyst, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, parlementary_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Remonstrance right establishes that significant fiscal edicts must be publicly read, formally objected to if necessary, and debated before registration into law. This creates a deliberative checkpoint: the crown cannot act arbitrarily; it must justify innovations to an educated body of magistrates who understand law and precedent. The constraint solves the problem of unchecked executive fiscal power by embedding constitutional review into the registration process.
% TRANSFER_FUNCTION: Transfers authority to delay and publicly contest fiscal innovation from the crown to the Parlement. Transfers tax burden away from the nobility of the robe and toward commoners by protecting exemptions. Moves prestige and constitutional authority to the magistracy, which frames itself as the guardian of ancient law against arbitrary change.
% ABSENT_VOICES: Commoners and merchants who bear the tax burden have no voice in the Parlement and cannot remonstrate. Enlightenment reformers and crown modernizers are excluded from the remonstrance structure itself; their critique appears in pamphlets and correspondence but has no formal standing in the registration process. Provincial Parlements outside Paris are typically subordinate to the Paris Parlement and have weaker remonstrance capacity.
% DISAPPEARANCE_RATIONALE: If remonstrance right disappeared overnight, fiscal innovation would accelerate: universal taxation could be imposed without Parlementary consent, feudal exemptions would become indefensible, and the Parlement's role as a co-legislator on fiscal matters would collapse. The nobility of the robe would lose its primary structural mechanism for defending tax exemptions. Crown authority to implement reform would expand. The distribution of the tax burden would shift dramatically from commoners toward the formerly exempt.
% FOUNDING_PROBLEM: In the early development of French absolute monarchy, the crown required institutional mechanisms to register and enforce edicts across the realm. The Parlements (particularly Paris) became the registering bodies. Over time, the Parlements claimed the right to examine edicts before registration and to formally remonstrate (object and delay) those seen as violating ancient law or the Parlement's own corporate interests. The founding problem was coordination: how to create a lawful process for implementing edicts without arbitrary imposition and without the Parlement's institutional cooperation in registration.
% FOUNDING_PROBLEM_CORROBORATION: The Parlements attest that the founding problem of preventing arbitrary fiscal innovation remains live and that remonstrance right is the constitutional mechanism that solves it. The crown and its reformist advisors (Turgot, Necker, the Physiocrats) attest that the founding problem has been superseded: the real problem is now the obstruction of necessary fiscal modernization by a self-interested magistracy hiding behind the language of constitutional liberty. Independent historians and legal theorists outside both camps recognize the constraint as solving a real coordination problem while simultaneously protecting feudal privilege; the problem is genuine but the solution is structurally corrupted.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the remonstrance right, while genuinely coordinating fiscal debate, simultaneously enables the magistracy to defend its tax exemptions and shift burden to commoners. The constraint operates as a coordination mechanism (edicts must be justified) and as an extraction mechanism (exemptions are defended by constitutional right), both features operating through the same institutional structure. Suppression is high (0.72) because the crown must actively override Parlementary remonstrations through lit de justice to implement reforms, and the magistracy must actively defend its institutional authority through litigation and collective remonstrance campaigns. Theater is moderate (0.42) because the remonstrance process is genuine constitutional deliberation, but increasingly the magistracy's actual remonstrations defend exemptions rather than abstract principle; by the late 18th century, performance of constitutional role (theatrical invocation of ancient law) is growing while coordination of actual fiscal policy (real function) is degrading. The measurement series shows rising extractiveness from 1700–1780 as the magistracy becomes more aggressive in defending exemptions while maintaining constitutional language; suppression requirement rises as the crown finds the constraint increasingly difficult to overcome; theater ratio rises as remonstrations become more performatively constitutional and less substantively fiscal-policy-coordinating.
 *
 * PERSPECTIVAL GAP:
 *   The magistrate seat and the crown seat compute into radically different types from the same structural data. From the magistrate perspective, remonstrance right is rope—genuine coordination protecting everyone from arbitrary power. From the crown/reform perspective, it is tangled_rope at minimum (genuine coordination plus extraction) and arguably snare (the coordination function is cover for extraction). The engine will compute per-seat divergence from the authored structural data: magistrates with identity-locked exit, collective institutional power, and a framing emphasizing constitutional principle compute differently than commoners with trapped exit, powerlessness, and no voice; and differently still than crown officials with powerful but constrained exit trying to implement reform. The authored directionality values (high extraction, high suppression) should produce institutional-seat and powerful-seat divergence because the same constraint coordinates for the magistracy and extracts from commoners and the reform coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   The parlementary magistracy is the structural beneficiary (d near 0.0): it sets the rules, defends its exemptions through the mechanism, has identity-locked exit (professional identity fused with magistracy), and collects the constraint's gains (exemptions maintained, authority preserved). The crown fiscal authority is a payer (d toward 0.7): must justify every reform, faces delay and obstruction, cannot implement without costly override. The commoners are victims (d near 1.0): powerless, trapped, bear the shifted tax burden, have no voice. The reform coalition within the crown is also a payer (d toward 0.6–0.7): their modernization agenda is structurally obstructed. The magistracy's identity-locked exit means they cannot simply walk away from the role even if the constraint becomes extractive; their social position, property ownership, and professional identity are constituted through magistracy. No override is needed here because the derivation chain (beneficiary + powerful + identity-locked exit + institutional power) produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of remonstrance right was to create a deliberative checkpoint for fiscal edicts—to prevent arbitrary power and ensure laws are justified before registration. This mandate is still technically fulfilled: remonstrations do occur, debate does happen, edicts are sometimes withdrawn or modified. However, by the late 18th century, the actual practice is increasingly captured by the magistracy's defense of its own exemptions; the deliberative function (real coordination) has not vanished but has been layered with extraction (exemption defense). The founding problem—arbitrary fiscal power—is no longer the constraint's primary function; the primary function has become exemption maintenance. The measurements show rising theater ratio (0.25 to 0.42 over the interval) indicating that performance of constitutional authority is rising relative to actual fiscal coordination. The constraint exhibits mild mandatrophy: the original purpose (coordinate fiscal edicts, prevent arbitrary power) is still invoked in justifications, but the operative function is different (defend exemptions, maintain magistrate privilege). This is not severe mandatrophy (the coordination function is not entirely theatrical; remonstrations do block edicts), but it is real drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Are the coordination function (debate and justification before fiscal edicts) and the extraction function (defense of magistracy exemptions) structurally inseparable, or could deliberative fiscal review occur without the Parlement''s power to block edicts that threaten exemptions?',
    'Comparative analysis of other deliberative fiscal bodies (estates general, later parliaments, constitutional conventions) that coordinate fiscal policy without corporate exemption defense; OR historical counterfactual modeling of remonstrance right without exemption protection.',
    'If separable, the extraction is additional and not inherent to coordination—the constraint is cleanly tangled_rope. If inseparable (exemption defense is the only mechanism by which magistrates commit to the deliberative process), the extraction is coordination cost and the classification shifts toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the Parlement''s power to block fiscal edicts and its power to defend exemptions can be separated or are intrinsically linked.').

omega_variable(
    reading_kernel_dispute,
    'Is remonstrance right fundamentally a constitutional check protecting all subjects from arbitrary power (magistrate reading) or fundamentally a feudal veto protecting aristocratic privilege (crown reading)?',
    'This is a framing dispute that cannot be resolved by historical evidence alone. The resolution depends on whether one privileges continuity with medieval constitutional tradition (magistrate reading) or treats feudal privilege as a category error in legitimate governance (crown reading). Different philosophical commitments produce different verdicts on the same historical facts.',
    'Under the magistrate reading (this story), the constraint is tangled_rope: coordination + extraction. Under the crown reading (sibling), the constraint would be classified as snare: the coordination story is secondary to the extraction function. The engine will compute per-seat classification from structural data; the reading contest appears in the omega variables and commentary, not in the core metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_dispute, conceptual, 'The fundamental framing dispute between magistrate and crown readings of the same remonstrance kernel.').

omega_variable(
    theater_ratio_interpretation,
    'Does the rising theater ratio (0.25 to 0.42) indicate that remonstrance practice is becoming more performative and less functionally fiscal-coordinating, or that remonstrations are being increasingly invoked for normative constitutional reasons rather than narrow fiscal ones?',
    'Content analysis of actual remonstrance documents: do they invoke constitutional principle increasingly (higher theater ratio score) because the magistracy is defending principle itself (real function) or because they are performing principle to defend exemptions (extraction cover)?',
    'If remonstrations are increasingly principled defense of constitutional liberty (not fiscal cover), theater ratio does not indicate mandatrophy—it indicates shifting grounds of remonstrance from fiscal to constitutional. If increasingly performative cover, theater ratio indicates genuine functional drift (mandatrophy). The score is the same; the interpretation depends on reading the documents themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Whether rising theater ratio indicates genuine mandatrophy or shifting (but legitimate) grounds of remonstrance.').

omega_variable(
    exemption_defense_as_constitutional_function,
    'Should the Parlement''s defense of corporate tax exemptions be classified as extraction (shifting burden from privileged to unprivileged) or as a legitimate constitutional function (protecting the magistracy''s institutional independence against crown confiscation)?',
    'Normative analysis depends on constitutional theory: does institutional independence require fiscal autonomy, or does equality require universal taxation? No empirical data resolves this; different constitutional frameworks produce different verdicts.',
    'Under a framework that privileges institutional independence (medieval corporatism, federalism), exemption defense is constitutional function and the constraint''s extractiveness is lower. Under a framework that privileges formal equality (enlightenment universalism), exemption defense is unjustifiable extraction and the constraint''s extractiveness is higher. The authored extractiveness (0.68) presumes a partial frame: coordination + extraction both real, but extraction substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_defense_as_constitutional_function, preference, 'Whether magistracy tax exemptions are legitimate institutional protection or unjustifiable feudal privilege.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1700, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1700, remonstrance_authority__magistrate_reading, theater_ratio, 1700, 0.25).
narrative_ontology:measurement_basis(remo_tr_t1700, projected).
narrative_ontology:measurement(remo_tr_t1730, remonstrance_authority__magistrate_reading, theater_ratio, 1730, 0.28).
narrative_ontology:measurement_basis(remo_tr_t1730, observed).
narrative_ontology:measurement(remo_tr_t1760, remonstrance_authority__magistrate_reading, theater_ratio, 1760, 0.35).
narrative_ontology:measurement_basis(remo_tr_t1760, observed).
narrative_ontology:measurement(remo_tr_t1780, remonstrance_authority__magistrate_reading, theater_ratio, 1780, 0.42).
narrative_ontology:measurement_basis(remo_tr_t1780, observed).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.42).
narrative_ontology:measurement_basis(remo_tr_t1789, observed).

% Extraction over time
narrative_ontology:measurement(remo_be_t1700, remonstrance_authority__magistrate_reading, base_extractiveness, 1700, 0.52).
narrative_ontology:measurement_basis(remo_be_t1700, projected).
narrative_ontology:measurement(remo_be_t1730, remonstrance_authority__magistrate_reading, base_extractiveness, 1730, 0.58).
narrative_ontology:measurement_basis(remo_be_t1730, observed).
narrative_ontology:measurement(remo_be_t1760, remonstrance_authority__magistrate_reading, base_extractiveness, 1760, 0.64).
narrative_ontology:measurement_basis(remo_be_t1760, observed).
narrative_ontology:measurement(remo_be_t1780, remonstrance_authority__magistrate_reading, base_extractiveness, 1780, 0.68).
narrative_ontology:measurement_basis(remo_be_t1780, observed).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.68).
narrative_ontology:measurement_basis(remo_be_t1789, observed).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1700, remonstrance_authority__magistrate_reading, suppression_requirement, 1700, 0.48).
narrative_ontology:measurement_basis(remo_su_t1700, projected).
narrative_ontology:measurement(remo_su_t1730, remonstrance_authority__magistrate_reading, suppression_requirement, 1730, 0.55).
narrative_ontology:measurement_basis(remo_su_t1730, observed).
narrative_ontology:measurement(remo_su_t1760, remonstrance_authority__magistrate_reading, suppression_requirement, 1760, 0.64).
narrative_ontology:measurement_basis(remo_su_t1760, observed).
narrative_ontology:measurement(remo_su_t1780, remonstrance_authority__magistrate_reading, suppression_requirement, 1780, 0.72).
narrative_ontology:measurement_basis(remo_su_t1780, observed).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.72).
narrative_ontology:measurement_basis(remo_su_t1789, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__magistrate_reading, 0.12).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel decomposes into two constraint stories distinguished by reading: magistrate_reading (this story, ε=0.68, tangled_rope) and crown_reading (sibling story, higher ε, snare-leaning). The two readings reference the same institutional mechanism (Parlement's right to object and delay fiscal edicts) but differ on its legitimacy and function. The magistrate reading author-holds that remonstrance is genuine constitutional coordination that has become extractive through exemption defense; the crown reading would hold that the coordination framing is primarily cover for extraction. Both ε values are stable and distinct because the readings instantiate different constraint boundaries: magistrate reading includes the exemption-defense function as part of the same constraint; crown reading may decompose exemptions into a separate snare constraint. The two stories are linked via network.affects_constraints because the magistrate reading's legitimacy claim is historically grounded in a version of constitutionalism that the crown reading rejects; changes to one reading's perceived legitimacy affect the other's strategic position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
