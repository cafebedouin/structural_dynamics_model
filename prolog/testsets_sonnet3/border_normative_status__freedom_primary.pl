% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Enforcement Under the Freedom-of-Movement-Primary Reading
 *   domain: political philosophy / international law / migration studies
 *
 * SUMMARY:
 *   This story authors ONE reading of the border-normative-status kernel: the
 *   freedom-of-movement-primary reading, under which freedom of movement is a
 *   fundamental human right and exclusion at borders requires extraordinary
 *   justification that is essentially never met by ordinary economic,
 *   cultural, or fiscal interests. Under this reading's own lights, the
 *   standing arrangement — territorial border enforcement as practiced by
 *   wealthy receiving states — is a rights violation wearing the costume of
 *   routine sovereign administration. The victim set under this reading
 *   includes excluded would-be migrants (who have no legitimate basis for
 *   exclusion) and, distinctively, displaced domestic workers and
 *   irregularized migrants inside the state, whose position is worsened by an
 *   enforcement regime that does not stop mobility so much as strip it of
 *   legal protection. This is a sibling of sovereignty_primary and
 *   qualified_sovereignty — those readings are separate constraint stories
 *   with their own ε values and victim sets; this file does not average
 *   across them.
 *
 * KEY AGENTS:
 *   - receiving_state_incumbent_citizens: primary beneficiary (organized/mobile) — captures exclusive residence and labor-market access
 *   - border_enforcement_industry: agenda-setter and secondary beneficiary (institutional/arbitrage) — administers and profits from the exclusion apparatus
 *   - capital_owners_using_immobilized_labor: beneficiary (powerful/arbitrage) — exploits asymmetric mobility between capital and labor
 *   - excluded_would_be_migrants: primary target (powerless/trapped) — bears the core rights violation this reading names
 *   - displaced_domestic_workers: secondary victim (powerless/constrained) — new to the victim set under this reading, harmed by irregularization rather than exclusion itself
 *   - irregularized_migrants_inside_the_state: victim (powerless/trapped) — bears the downstream cost of blocked regular channels
 *   - human_rights_monitoring_bodies: analytical observer — documents violations without enforcement power
 *   - sending_state_governments: excluded voice — bears consequences with no seat in the process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.72).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.81).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Enforcement Under the Freedom-of-Movement-Primary Reading").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political philosophy / international law / migration studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '1b637270-0b75-40bf-a92c-0dd773084611').
narrative_ontology:cs_kernel_codification('1b637270-0b75-40bf-a92c-0dd773084611', distributed).
narrative_ontology:cs_authority_grounding('1b637270-0b75-40bf-a92c-0dd773084611', distributed).
narrative_ontology:cs_reading_relation('1b637270-0b75-40bf-a92c-0dd773084611', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('1b637270-0b75-40bf-a92c-0dd773084611', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('1b637270-0b75-40bf-a92c-0dd773084611', foundational, movement_is_a_primary_right_not_a_state_grant).
narrative_ontology:cs_axiom_status(movement_is_a_primary_right_not_a_state_grant, holdable).
narrative_ontology:cs_axiom_grounding('1b637270-0b75-40bf-a92c-0dd773084611', movement_is_a_primary_right_not_a_state_grant, deontological).
narrative_ontology:cs_axiom('1b637270-0b75-40bf-a92c-0dd773084611', secondary, exclusion_requires_near_absolute_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_near_absolute_justification, holdable).
narrative_ontology:cs_axiom_grounding('1b637270-0b75-40bf-a92c-0dd773084611', exclusion_requires_near_absolute_justification, deontological).
narrative_ontology:cs_reference_frame('1b637270-0b75-40bf-a92c-0dd773084611', universal_declaration_freedom_of_movement_baseline).
narrative_ontology:cs_drift_state('1b637270-0b75-40bf-a92c-0dd773084611', contemporary_securitized_border_regime, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('1b637270-0b75-40bf-a92c-0dd773084611', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, receiving_state_incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, capital_owners_using_immobilized_labor).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, irregularized_migrants_inside_the_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a birthright allocation of residence and labor-market access inside a wealthy jurisdiction. The border is what makes that allocation exclusive: without it, wages, housing, and public-service access would be contested by anyone who wished to enter. They vote for enforcement, staff much of its administration, and rarely bear its direct costs.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_state_incumbent_citizens, beneficiary,
    organized, generational, mobile, national).

% Border agencies, detention contractors, and surveillance-technology vendors design, fund, and operate the exclusion apparatus. Their budgets, headcounts, and political relevance scale with enforcement intensity; from this reading's perspective they administer a rights violation dressed as routine administration.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_industry, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, border_enforcement_industry, beneficiary).

% Benefit doubly: from a domestic low-wage workforce whose bargaining power is suppressed by irregular status, and from offshore production where the same excluded populations remain locked out of higher-wage labor markets. Can relocate capital freely while the humans who would follow it cannot.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, capital_owners_using_immobilized_labor, beneficiary,
    powerful, generational, arbitrage, global).

% Denied entry, turned back at frontiers, or left to die in transit corridors as a direct consequence of enforcement whose burden of justification this reading treats as extraordinarily high and essentially never met by ordinary economic or cultural interests. From this reading's own vantage, there is no legitimate exclusion here — every turned-back person is a rights violation, not a permissible policy outcome.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Low-wage citizens and long-settled residents whose bargaining position is eroded by a large irregularized workforce created by the same border regime that excludes some and traps others inside without status. They compete for wages and housing against people the enforcement apparatus has rendered legally invisible, without the exclusionary border having actually stopped mobility, only stripped it of protection.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    powerless, biographical, constrained, national).

% Already inside the territory but without recognized status, because entry was blocked through regular channels that this reading holds should not have existed as a barrier at all. Live under threat of detention and deportation, cannot access labor protections, and are structurally required by employers who benefit from their unprotected labor.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, irregularized_migrants_inside_the_state, payer,
    powerless, biographical, trapped, national).

% UN special rapporteurs, treaty bodies, and NGOs document deaths at borders, family separations, and detention conditions, and assess state practice against the freedom-of-movement-primary standard. They can name violations but hold no enforcement power over sovereign border administration.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% Governments of origin countries have no seat in the receiving state's border policy process, despite bearing the consequences of blocked emigration (foregone remittances, pressure on domestic labor markets, political fallout from stranded or deported nationals). Their objections are diplomatic gestures with little binding force.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, sending_state_governments, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination problem the border apparatus nominally solves for the incumbent population: allocating scarce housing, public services, and labor-market access within a jurisdiction, and providing a stable administrative unit for taxation and representation. This reading does not deny that some coordination function exists — it denies that it rises to the level of justification required to override a fundamental right.
% TRANSFER_FUNCTION: The arrangement moves a right — freedom to relocate to seek a better life, safety, or reunification with family — away from non-citizens globally, concentrating residence security, labor-market access, and physical safety inside the receiving state's incumbent population and the enforcement apparatus's budget line. It also moves bargaining power from displaced domestic low-wage workers and irregularized migrants toward employers who benefit from a workforce split between full rights-holders and none.
% ABSENT_VOICES: Sending-state governments and, most acutely, the excluded migrants themselves have no institutional voice in receiving-state border policy; their objections are filed through asylum claims, litigation, or advocacy organizations that speak for them rather than with them, and are systematically outweighed by domestic electoral incentives.
% DISAPPEARANCE_RATIONALE: If exclusionary border enforcement vanished overnight under this reading's terms, global labor and residence patterns would reorganize substantially: wage differentials between jurisdictions would compress as migration flows equalized, receiving-state labor markets would face short-term disruption, and the enforcement industry (detention contractors, surveillance vendors, a large share of border agency budgets) would lose its object entirely.
% FOUNDING_PROBLEM: Historically, borders and exclusion regimes were built to consolidate sovereign control over territory, population, and revenue extraction, later re-justified as protecting national labor markets, cultural cohesion, and public order.
% FOUNDING_PROBLEM_CORROBORATION: Receiving-state governments and their electorates attest the founding problem (protecting scarce domestic resources and social cohesion) is still live and justifies continued enforcement. Human rights monitoring bodies, migration scholars, and sending-state governments — parties outside the beneficiary set — attest that the scale and lethality of current enforcement vastly exceeds any defensible reading of the original coordination problem, and that the modern border regime primarily serves incumbent-population rent protection and an entrenched enforcement industry rather than a narrowly tailored, still-live interest.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) and rising because, under this reading, the border's function has shifted from a plausibly justifiable coordination device toward an entrenched rent-protection and industry-sustaining mechanism whose justificatory burden is never actually met. Suppression is authored even higher (0.81) because persistence depends on physical enforcement infrastructure, detention, deportation, and interdiction — coercion, not consent. Theater ratio is moderate-low (0.28): most enforcement activity is functionally effective at excluding, not merely performative, though a growing share (rising over the interval) consists of visible deterrence theater (walls, high-profile raids) whose primary purpose is signaling resolve to domestic audiences. Accessibility collapse (0.6) reflects that once someone understands the enforcement architecture, legal alternatives for regular entry are extremely narrow; resistance (0.75) reflects substantial organized pushback from migrant-rights movements, litigation, and international monitoring bodies.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state incumbent citizens and the enforcement industry sit near the beneficiary end: the border's operation subsidizes their residence security, labor-market position, or institutional budget. Capital owners benefit from asymmetric mobility (capital can arbitrage, labor cannot). Excluded would-be migrants sit at the full-target end — trapped, powerless, bearing the constraint's core harm with no legitimate justification recognized by this reading. Displaced domestic workers and irregularized migrants inside the state are victims by a different mechanism: not exclusion itself but the downstream labor-market and legal-status effects of an enforcement regime that fails to actually stop movement while stripping it of protection — this is the structural delta this reading adds relative to the sovereignty-primary sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genealogy gap this reading treats as decisive: whatever narrow coordination function border control might once have served (allocating scarce local resources), the scale, lethality, and industrial entrenchment of contemporary enforcement vastly exceeds it. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is itself diagnostic — the world would rearrange not because the coordination function is still needed, but because a large industry and an incumbent-population rent allocation now depend on the enforcement apparatus continuing, independent of whether the original justification still holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_justification_threshold,
    'What burden of proof, if any, could a receiving state meet to legitimately exclude a non-citizen under a freedom-of-movement-primary framework, and has any actual state practice met it?',
    'Systematic review of state justifications for exclusion (security, fiscal capacity, cultural cohesion claims) against the reading''s own stated threshold for ''extraordinary justification,'' assessed by independent human rights adjudication rather than domestic political process.',
    'If no real-world exclusion has ever met the threshold, this reading treats essentially all current border enforcement as illegitimate, which is the classification this story author; if some narrow class of exclusions (e.g., individualized security threats) can meet it, the victim set and extractiveness estimate should be narrowed accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, conceptual, 'Whether any actual exclusion practice satisfies this reading''s own justificatory standard.').

omega_variable(
    kernel_reading_selection,
    'Is the freedom-of-movement-primary reading the correct lens for this constraint, or do the sovereignty_primary and qualified_sovereignty readings better capture the structural reality of state practice and international law as currently constituted?',
    'Cross-reference this story''s classification against the sibling readings'' independently authored classifications; examine which reading''s victim/beneficiary structure better predicts observed enforcement patterns and international legal doctrine (which currently leans toward qualified_sovereignty in most treaty regimes).',
    'Choosing freedom_primary maximizes the victim set (all excluded migrants) and treats virtually all enforcement as extractive; choosing sovereignty_primary would treat the same enforcement as a legitimate exercise of collective self-determination with no migrant victim set at all. This is exactly the committer-structure ambiguity the kernel format is designed to isolate rather than average over.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which kernel reading of border normative status best fits observed practice and doctrine.').

omega_variable(
    domestic_displacement_causal_mechanism,
    'Is the harm to displaced domestic workers actually caused by border enforcement''s irregularization effect, or by unrelated labor-market dynamics (automation, deunionization) that would occur regardless of border policy?',
    'Labor economics studies isolating the wage and employment effects of irregular-status workforce presence from other concurrent labor-market shifts, using natural experiments from regularization programs or enforcement surges.',
    'If the causal link is weak, displaced domestic workers should be removed or downweighted in the victim set, reducing the extractiveness estimate; if strong, it corroborates the structural delta this reading claims relative to sovereignty_primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_displacement_causal_mechanism, empirical, 'Whether irregularization from border enforcement, rather than other labor-market forces, actually harms domestic workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__freedom_primary, theater_ratio, 8, 0.17).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__freedom_primary, theater_ratio, 16, 0.2).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__freedom_primary, theater_ratio, 24, 0.23).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__freedom_primary, theater_ratio, 32, 0.26).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bord_be_t8, border_normative_status__freedom_primary, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(bord_be_t16, border_normative_status__freedom_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(bord_be_t24, border_normative_status__freedom_primary, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(bord_be_t32, border_normative_status__freedom_primary, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t8, border_normative_status__freedom_primary, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(bord_su_t16, border_normative_status__freedom_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(bord_su_t24, border_normative_status__freedom_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(bord_su_t32, border_normative_status__freedom_primary, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the border_normative_status kernel. freedom_primary (this file) treats virtually all exclusionary enforcement as an unjustified rights violation and includes excluded migrants and displaced domestic workers in its victim set. sovereignty_primary treats territorial exclusion as a legitimate exercise of collective self-determination and has no migrant victim set at all. qualified_sovereignty sits between the two, treating exclusion as conditionally legitimate subject to proportionality review, with a narrower and more contested victim set than freedom_primary. Each story authors its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
