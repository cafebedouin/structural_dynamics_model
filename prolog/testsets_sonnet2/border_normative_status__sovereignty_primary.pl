% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereign Border Exclusion Authority (Sovereignty-Primary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primary reading of the contested
 *   border-normative-status kernel: territorial boundaries are legitimate
 *   instruments of collective self-determination, and states possess
 *   foundational authority to exclude non-members without needing to justify
 *   exclusion against the excluded person's competing claim. Under this
 *   reading, border enforcement is a legitimate exercise of a real
 *   coordination function (securing a bounded, self-governing political
 *   community), and the harms borne by excluded migrants are treated within
 *   the reading's own logic as externalities of a domestically legitimate
 *   decision rather than as presumptive rights violations requiring
 *   proportionality review. This is NOT a story about borders in general — it
 *   is one specific reading among three sibling constraints (freedom_primary,
 *   qualified_sovereignty) that read the same underlying kernel (what
 *   normative status does a territorial boundary have?) differently, with
 *   different beneficiary/victim structures and different ε values. The
 *   sibling readings are separate constraint files, not alternative
 *   interpretations folded into this one.
 *
 * KEY AGENTS:
 *   - receiving_state_apparatus: agenda-setter, institutional power, sets and enforces exclusion policy
 *   - citizen_polity: beneficiary, organized power, retains exclusive membership goods
 *   - excluded_migrants: payer, powerless, trapped exit, bears exclusion cost with no voice
 *   - human_rights_monitoring_bodies: observer, institutional power, contests the framework from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.58).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereign Border Exclusion Authority (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '393d83bb-a95e-498f-b324-3c4220d925cf').
narrative_ontology:cs_kernel_codification('393d83bb-a95e-498f-b324-3c4220d925cf', distributed).
narrative_ontology:cs_authority_grounding('393d83bb-a95e-498f-b324-3c4220d925cf', distributed).
narrative_ontology:cs_reading_relation('393d83bb-a95e-498f-b324-3c4220d925cf', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('393d83bb-a95e-498f-b324-3c4220d925cf', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('393d83bb-a95e-498f-b324-3c4220d925cf', foundational, territorial_exclusion_is_foundational_not_derivative).
narrative_ontology:cs_axiom_status(territorial_exclusion_is_foundational_not_derivative, holdable).
narrative_ontology:cs_axiom_grounding('393d83bb-a95e-498f-b324-3c4220d925cf', territorial_exclusion_is_foundational_not_derivative, deontological).
narrative_ontology:cs_axiom('393d83bb-a95e-498f-b324-3c4220d925cf', secondary, excluded_nonmembers_bear_no_correlative_claim_against_the_polity).
narrative_ontology:cs_axiom_status(excluded_nonmembers_bear_no_correlative_claim_against_the_polity, holdable).
narrative_ontology:cs_axiom_grounding('393d83bb-a95e-498f-b324-3c4220d925cf', excluded_nonmembers_bear_no_correlative_claim_against_the_polity, conventional).
narrative_ontology:cs_reference_frame('393d83bb-a95e-498f-b324-3c4220d925cf', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('393d83bb-a95e-498f-b324-3c4220d925cf', post_1990s_globalization_and_human_rights_regime, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('393d83bb-a95e-498f-b324-3c4220d925cf', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_polity).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, receiving_state_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, domestic_labor_incumbents).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers_in_transit).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, displaced_persons_denied_entry).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, state_territorial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces immigration law, border patrol, detention, and removal policy under the doctrine that the polity's self-determination includes the authority to decide who joins it. Administers visa regimes, asylum screening, and physical border infrastructure. Justifies exclusion as an exercise of collective self-governance rather than as a harm requiring special justification.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, receiving_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Retains exclusive claim on public goods, political voice, labor-market protections, and cultural continuity that bordering preserves. Can exit the territory at will (citizenship carries near-arbitrage mobility in many cases) but is not subject to the exclusion the border imposes on others. Votes on and legitimates the border regime through democratic process.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_polity, beneficiary,
    organized, generational, mobile, national).

% Benefits from reduced labor-market competition where entry is restricted, particularly in lower-wage sectors. Has limited independent power to alter border policy but is invoked by the agenda-setter as a beneficiary class justifying restriction.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, domestic_labor_incumbents, beneficiary,
    moderate, biographical, constrained, national).

% Seeks entry for economic opportunity, family reunification, or safety and is turned back, detained, or deported under the doctrine that the receiving state's self-determination authorizes exclusion without need for individualized justification. Has no vote in the polity deciding the exclusion and no forum in which the underlying legitimacy of exclusion itself can be contested — only its procedural application.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Flees persecution or violence and encounters a border regime that treats even protection claims as subject to sovereign discretion over entry. Under this reading, exclusion is a legitimate default; asylum becomes an exception the state grants rather than an obligation constraining sovereignty, so processing delay, pushback, and offshore deterrence measures are read as ordinary exercises of authority rather than presumptive rights violations.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers_in_transit, payer,
    powerless, immediate, trapped, global).

% Bears the compounding costs of statelessness or protracted displacement when denied entry — the harm of exclusion is treated, under this reading, as an externality of a legitimate domestic decision rather than a cost the excluding state must weigh or justify against the excluded person's interest.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, displaced_persons_denied_entry, payer,
    powerless, biographical, trapped, global).

% Bears the fiscal and social consequences of blocked outmigration and remittance loss but has no standing role in the receiving state's self-determination framing — the sovereignty-primary reading treats border policy as an entirely internal matter of the receiving polity.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% Documents pushback deaths, detention conditions, and family separation, and argues from outside this reading's framework that exclusion requires proportionate justification. Under sovereignty-primary logic, their findings are treated as advisory commentary on a domestic prerogative rather than as binding constraints.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, citizen_polity).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bordering coordinates collective self-governance: it lets a bounded political community decide, through its own institutions, who shares in its public goods, labor market, and political voice, and it stabilizes the demos that democratic accountability presupposes.
% TRANSFER_FUNCTION: Moves the costs of exclusion — foreclosed opportunity, prolonged displacement, family separation, and physical risk at the border — from the receiving polity onto non-members, while concentrating labor-market, fiscal, and political-voice benefits inside the citizen polity.
% ABSENT_VOICES: Excluded migrants, asylum seekers, and sending-state governments have no vote and no forum within the receiving polity's self-determination framework to contest the underlying legitimacy of exclusion; they can contest only its procedural application, if that. Human rights bodies raise objections from outside the framework, which this reading treats as non-binding.
% DISAPPEARANCE_RATIONALE: If sovereign exclusion authority were withdrawn overnight, the receiving state's labor markets, welfare allocation, political membership rules, and enforcement apparatus would all have to reorganize; citizen polities would lose the exclusive claim on public goods and political voice that bordering currently secures, and migration flows would reallocate globally based on opportunity rather than admission decisions.
% FOUNDING_PROBLEM: Modern states arose to secure collective self-governance and mutual protection for a bounded membership; without a mechanism to define and defend that membership, the argument goes, self-determination is meaningless because the community cannot control its own composition or the terms of shared life.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists in the self-determination tradition and many democratic states' constitutional courts attest the problem remains live — self-governance requires a bounded demos. Refugee law scholars, sending-state officials, and human rights monitoring bodies attest from outside the beneficiary set that the modern border regime has drifted from securing self-governance toward externalizing displacement costs onto non-members with no correlative obligation-bearing framework, which they read as a shifted function rather than a live founding purpose.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is authored as substantial-but-moderate: the coordination function (a bounded, self-governing demos) is real under this reading's own premises, but the border apparatus also captures labor-market and fiscal benefits for the citizen polity while imposing severe, uncompensated costs on non-members who have no standing to contest the underlying legitimacy claim — only its application. Suppression (0.72) is high because the exclusion is backed by detention, deportation, and physical enforcement infrastructure, and because the reading itself forecloses the excluded party's principal avenue of contest (the legitimacy of exclusion, not merely its administration). Theater ratio is comparatively low (0.28): the self-governance function is not merely performative even on a skeptical read, though procedural review mechanisms (asylum hearings, humanitarian exceptions) have grown as compliance theater layered atop an unreviewable sovereign core. Accessibility collapse (0.62) reflects that under sovereignty-primary logic, alternatives to exclusion (open admission, proportionality review) are treated as off the table by design, not merely difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   The receiving state apparatus and citizen polity sit near the beneficiary end: the state administers the exclusion and the polity captures its benefits (labor protection, public-good exclusivity, political stability) while bearing none of its costs. Excluded migrants, asylum seekers, and displaced persons sit at the full-target end: trapped exit, no voice in the deciding polity, and — distinctively under this reading — no recognized standing to contest exclusion's legitimacy at all, only its procedural execution. This is the structural delta this reading introduces relative to its siblings: freedom_primary would treat these same agents as rights-holders whose exclusion requires extraordinary justification (making the state the target of a justificatory burden), and qualified_sovereignty would place them as claimants entitled to proportionality review. Here, they are structurally external to the polity's own self-determination calculus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a bounded demos is necessary for meaningful self-governance) remains live in the sense that federated and unitary states alike continue to require some membership boundary to allocate political voice. But the mismatch this story surfaces is between founding_problem_status (contested) and disappearance_verdict (world_rearranges): if genuinely still serving pure self-governance, exclusion would track membership-formation needs narrowly; instead, measured extraction has risen steadily (0.42 to 0.58) over 35 years even as the self-governance framing has hardened rather than softened, consistent with a coordination function increasingly used to justify externalized cost-shifting onto non-members with no correlative obligation — the signature of a tangled rope rather than a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_primary_vs_freedom_primary_foreclosure,
    'Does the sovereignty-primary premise (states have foundational, non-derivative authority to exclude) logically foreclose the freedom-primary premise (movement is a fundamental right restrictable only by extraordinary justification) within a single normative framework, or can a state hold both simultaneously in different domains?',
    'Examine whether any actual legal or philosophical framework successfully holds both premises without collapsing one into a qualification of the other — e.g., does any state''s constitutional order treat exclusion as both foundational and rights-constrained without one term becoming vacuous?',
    'If genuinely foreclosing, sovereignty_primary and freedom_primary cannot coexist within one party''s committed framework, which is the basis for the ''forecloses'' relation declared in cs_structure. If a coherent hybrid exists, the relation should be weakened toward ''coexists_with'' or ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primary_vs_freedom_primary_foreclosure, conceptual, 'Whether sovereignty-primary and freedom-primary readings can jointly occupy a single normative framework or are logically exclusive.').

omega_variable(
    externality_framing_stability,
    'Is treating migrant displacement harm as a legitimate externality of self-determination a stable normative position, or does it collapse under scrutiny into an unacknowledged proportionality test (i.e., a disguised version of qualified_sovereignty)?',
    'Track whether sovereignty-primary legal regimes, when challenged, actually articulate exclusion decisions without implicit reference to proportionality or necessity — if courts and legislators consistently smuggle in balancing language, the ''pure'' sovereignty-primary reading may not be practiced anywhere, only claimed.',
    'If the externality framing cannot be sustained without smuggled proportionality reasoning, this reading''s real-world instantiations may already be closer to qualified_sovereignty than the doctrine admits, which would lower this story''s authored ε toward that sibling''s or suggest the two readings are less structurally distinct than declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_framing_stability, empirical, 'Whether sovereignty-primary exclusion practice can be sustained without implicit proportionality reasoning.').

omega_variable(
    collective_self_determination_scope_ambiguity,
    'Does the collective self-determination doctrine that grounds this reading extend coherently to justify permanent, categorical exclusion of desperate claimants (refugees, stateless persons), or only to ordinary immigration control absent emergency circumstances?',
    'Compare the doctrine''s philosophical justification (bounded demos needed for self-governance) against its actual application scope (blanket exclusion including protection claims) — a gap would indicate doctrinal overreach beyond what the founding justification supports.',
    'If the doctrine''s justificatory scope does not extend to categorical exclusion of protection claimants, then part of the measured extraction in this story is unjustified even by this reading''s own premises, strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_self_determination_scope_ambiguity, conceptual, 'Whether the self-determination justification for exclusion authority extends coherently to protection claimants, or only to ordinary migration control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__sovereignty_primary, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(bord_tr_t1997, border_normative_status__sovereignty_primary, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(bord_tr_t2004, border_normative_status__sovereignty_primary, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(bord_tr_t2011, border_normative_status__sovereignty_primary, theater_ratio, 2011, 0.23).
narrative_ontology:measurement(bord_tr_t2018, border_normative_status__sovereignty_primary, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(bord_tr_t2025, border_normative_status__sovereignty_primary, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_normative_status__sovereignty_primary, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(bord_be_t1997, border_normative_status__sovereignty_primary, base_extractiveness, 1997, 0.46).
narrative_ontology:measurement(bord_be_t2004, border_normative_status__sovereignty_primary, base_extractiveness, 2004, 0.5).
narrative_ontology:measurement(bord_be_t2011, border_normative_status__sovereignty_primary, base_extractiveness, 2011, 0.53).
narrative_ontology:measurement(bord_be_t2018, border_normative_status__sovereignty_primary, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(bord_be_t2025, border_normative_status__sovereignty_primary, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_normative_status__sovereignty_primary, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(bord_su_t1997, border_normative_status__sovereignty_primary, suppression_requirement, 1997, 0.59).
narrative_ontology:measurement(bord_su_t2004, border_normative_status__sovereignty_primary, suppression_requirement, 2004, 0.64).
narrative_ontology:measurement(bord_su_t2011, border_normative_status__sovereignty_primary, suppression_requirement, 2011, 0.67).
narrative_ontology:measurement(bord_su_t2018, border_normative_status__sovereignty_primary, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(bord_su_t2025, border_normative_status__sovereignty_primary, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_normative_status kernel, each authored as a separate story with its own ε and stakeholder structure per the ε-invariance principle. sovereignty_primary (this file) authors the standing exclusion arrangement from the sovereignty-primary reading's own lights: extraction is substantial (0.58) because a real coordination function (bounded self-governance) coexists with asymmetric cost externalization onto non-members who have no standing to contest exclusion's legitimacy. freedom_primary would author the same standing arrangement with much higher ε (the state's exclusion authority itself is the extractive object, migrants are the primary victims of a rights violation). qualified_sovereignty would author a lower ε reflecting a genuine proportionality constraint that narrows the gap between coordination and extraction. All three share the underlying kernel — what normative status does a territorial boundary have — but instantiate structurally distinct constraints with distinct victim sets and distinct classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
