% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Catastrophe-Memory Mourning Practice as Survival-Competence Training (Survival-Competence Reading)
 *   domain: religious/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A dispersed minority community maintains a calendrical architecture of
 *   mourning — fixed fasts, commemorations, and liturgies rehearsing each
 *   catastrophe that nearly ended it. This story authors that standing
 *   arrangement as the survival_competence_reading of the
 *   catastrophe_memory_kernel sees it: the rehearsal is survival training,
 *   transmitting an operational response repertoire to generations who did
 *   not live the events. The reading endorses the function and still records
 *   its costs honestly: participation is obligatory and communally enforced,
 *   members who would rather assimilate into host societies bear the
 *   boundary-maintenance costs, and descendant-carriers re-inhabit
 *   transmitted grief annually. Epsilon's referent is the standing
 *   mourning-practice arrangement as this reading assesses it — moderate
 *   extraction funding a genuine collective good — never the secularized
 *   commemoration the reading might prefer. KEY AGENTS (by structural
 *   relationship): communal_ritual_authorities — agenda-setter
 *   (institutional/identity_locked); threatened_diaspora_community —
 *   collective beneficiary (organized/constrained);
 *   observant_loyalist_members — beneficiary (moderate/identity_locked);
 *   assimilation_inclined_members — primary payer (moderate/constrained);
 *   catastrophe_descendant_members — payer and incidental beneficiary
 *   (moderate/identity_locked); ritual_excluded_women — excluded voice
 *   (powerless/identity_locked); comparative_ritual_scholars — analytical
 *   observer.
 *
 * KEY AGENTS:
 *   - communal_ritual_authorities: agenda-setter — set the calendar, rule on obligation, ordain successors; their standing rests on the calendar's continuity
 *   - threatened_diaspora_community: collective beneficiary — holds the encoded repertoire; the arrangement's declared purpose is its resilience under threat
 *   - observant_loyalist_members: beneficiary — keep the full practice; would rebuild it if lost
 *   - assimilation_inclined_members: primary payer — bear the boundary-maintenance costs of being marked distinct
 *   - catastrophe_descendant_members: payer and incidental beneficiary — re-inhabit transmitted grief annually; the same rehearsal is their inheritance
 *   - ritual_excluded_women: excluded — carry the practice's domestic and emotional labor with no seat in the councils
 *   - comparative_ritual_scholars: analytical observer — test the transmission claim from outside the community
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.55).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Catastrophe-Memory Mourning Practice as Survival-Competence Training (Survival-Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'e92822b8-819a-4f0f-9cf7-e0845ca9d873').
narrative_ontology:cs_kernel_codification('e92822b8-819a-4f0f-9cf7-e0845ca9d873', formalized).
narrative_ontology:cs_authority_grounding('e92822b8-819a-4f0f-9cf7-e0845ca9d873', lineage).
narrative_ontology:cs_interpretation_layer_present('e92822b8-819a-4f0f-9cf7-e0845ca9d873').
narrative_ontology:cs_reading_relation('e92822b8-819a-4f0f-9cf7-e0845ca9d873', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e92822b8-819a-4f0f-9cf7-e0845ca9d873', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_reading_relation('e92822b8-819a-4f0f-9cf7-e0845ca9d873', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('e92822b8-819a-4f0f-9cf7-e0845ca9d873', foundational, ritual_rehearsal_transmits_operational_competence).
narrative_ontology:cs_axiom_status(ritual_rehearsal_transmits_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('e92822b8-819a-4f0f-9cf7-e0845ca9d873', ritual_rehearsal_transmits_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('e92822b8-819a-4f0f-9cf7-e0845ca9d873', foundational, communal_continuity_justifies_boundary_costs).
narrative_ontology:cs_axiom_status(communal_continuity_justifies_boundary_costs, holdable).
narrative_ontology:cs_axiom_grounding('e92822b8-819a-4f0f-9cf7-e0845ca9d873', communal_continuity_justifies_boundary_costs, instrumental).
narrative_ontology:cs_reference_frame('e92822b8-819a-4f0f-9cf7-e0845ca9d873', survival_competence_archive).
narrative_ontology:cs_drift_state('e92822b8-819a-4f0f-9cf7-e0845ca9d873', contemporary_long_security_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e92822b8-819a-4f0f-9cf7-e0845ca9d873', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_diaspora_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, observant_loyalist_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_inclined_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, catastrophe_descendant_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, catastrophe_descendant_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, embodied_rehearsal_transmission_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, catastrophe_response_rehearsal_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and maintain the mourning calendar: fix the dates of communal fasts and commemorations, authorize the liturgies, rule on who is obligated to attend and what counts as fulfillment. Train and ordain successors, decide which new catastrophes enter the calendar, and adjudicate disputes over practice. Their standing within the community rests on the continuity of the calendar they administer; leaving the role would mean abandoning the position their training and family history built toward.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, communal_ritual_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% The dispersed community as a collective body. It holds the encoded response patterns — how the group reconstituted after each destruction, where it went, whom it trusted, what it refused — in the form of annually rehearsed practice. The community cannot exit its history or relocate out of its memory; it can transform how the practice is kept, and has done so after each catastrophe. Its resilience under renewed threat is the arrangement's declared purpose.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_diaspora_community, beneficiary,
    organized, generational, constrained, continental).

% Members who keep the full practice and would rebuild it if it were lost. They receive belonging, a usable past, and — on this reading — a rehearsed repertoire for surviving rupture. Exit would mean severing family ties and self-understanding at once; few take it, and those who do are mourned as losses.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, observant_loyalist_members, beneficiary,
    moderate, biographical, identity_locked, regional).

% Members who would prefer to blend into the surrounding society. The calendar marks them as distinct whether or not they feel distinct: obligatory observance, absence from host-society civic life on mourning days, restricted intermarriage and social integration, and communal pressure up to formal sanction for open defection. They can leave — some do — but the cost is family rupture, loss of community, and starting over outside every network they have.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_inclined_members, payer,
    moderate, biographical, constrained, regional).

% Descendants of survivors for whom the annual rehearsal re-activates transmitted grief: each commemoration requires them to inhabit the catastrophe their grandparents lived. They carry the heaviest emotional cost of the practice, and the same rehearsal is also where they received their competence and their connection to the dead. Their relationship to the calendar is constitutive — declining it would feel like abandoning the family dead.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, catastrophe_descendant_members, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, catastrophe_descendant_members, beneficiary).

% Women who bear much of the practice's domestic and emotional labor — preparing mourning observances, transmitting them to children, absorbing the household cost of communal obligation — but hold no seat in the councils that set the calendar or rule on obligation. In several communities they are barred from leading the public rehearsal itself. Their objections to how the burden is distributed are voiced informally, if at all.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_excluded_women, excluded,
    powerless, biographical, identity_locked, regional).

% Researchers in ritual studies, collective-memory scholarship, and disaster sociology who test whether calendrical catastrophe-rehearsal actually transmits operational competence, compare communities with and without maintained rehearsal under matched threat, and document how the calendar absorbs new catastrophes. They take no side in the community's internal disputes.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, threatened_diaspora_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real intergenerational transmission problem: a dispersed minority under recurring threat must carry operational knowledge — how to reconstitute after destruction, how to hold cohesion under pressure, when concealment beats assertion, how to rebuild institutions from a remnant — across generations who did not live the catastrophe, without relying on seizable texts or single carriers. Calendrical communal rehearsal transmits the pattern to every member simultaneously, embodied and socially reinforced.
% TRANSFER_FUNCTION: Moves attendance, attention, and emotional labor from all members — most heavily from the reluctant and from descendant-carriers — into a maintained group boundary and a rehearsed response repertoire; moves interpretive authority and communal standing to the calendar's administrators.
% ABSENT_VOICES: Women who carry the practice's domestic and emotional labor have no seat in the councils that set the calendar or rule on obligation; their objections surface informally if at all. Assimilation-inclined members are present but structurally outvoted. Outside the community, historians and trauma researchers who dispute the survival-competence causal claim take no part in the deliberation that maintains the practice. The survival-framing's unanimity inside the councils partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the mourning-rehearsal arrangement vanished overnight, the community would not simply continue minus a custom: the calendrical infrastructure that synchronizes memory, marks boundaries, and rehearses response patterns would be gone, commemoration would scatter into private and textual forms, boundary maintenance would weaken within a generation, and — on this reading — the community's capacity to reconstitute after the next catastrophe would decay. Adjacent institutions (education, life-cycle ritual, communal charity) are organized around the same calendar and would rearrange with it.
% FOUNDING_PROBLEM: Repeated catastrophes — destructions, expulsions, massacres, forced conversions — each came close to ending the community outright, and each time survival depended on patterns the destroyed generation had learned too late or failed to pass on. The arrangement was built to make catastrophe memory operational: institutionalized, calendrically rehearsed mourning so that every generation carries the response repertoire of the last.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the community's persecutors treated the practice as functionally load-bearing — inquisitorial and Soviet-era campaigns specifically targeted mourning assemblies and commemorative rites, which they would not bother suppressing if the practice were inert. Historical demographers document the recurrence of destruction-and-reconstitution cycles independent of the community's own account, and comparative disaster sociology finds faster post-atrocity reconstitution in communities with maintained catastrophe-rehearsal. No outside source attests that the founding problem is dead; the security-era dispute over its liveness comes from inside.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.55 at interval end): the arrangement imposes real, identifiable costs — obligatory participation, social marking, annual grief-reactivation — on top of a transmission function this reading holds to be genuine. Suppression (0.62) is a raw structural property, unscaled by power or scope: communal sanction, family rupture, and network loss are the structural share; the internalized share (obligation to the dead, identity fusion with the calendar) is carried by the suppression-mechanism omega rather than folded into the scalar. Theater (0.40) reflects a substantial rote share in the current security-era state without functional atrophy. Accessibility collapse is moderate (0.45): secular commemoration, historiography, and family storytelling partially substitute, but none reproduces the calendrical compulsion and communal embodiment. Resistance (0.50) is real — reform currents, secularization, feminist challenge to exclusion — without overthrowing the calendar. All three series run on one shared grid (t = 0, 15, 30, 45, 60, 75, 90, 105, 120; two full threat–security cycles). The cycle: in threat eras, external persecution marks members and performs boundary-maintenance regardless of the ritual, so the arrangement's marginal imposition falls while its value is visibly realized (extractiveness 0.38–0.42, theater 0.15–0.22, enforcement demand low — external coercion does the holding); in security eras the assimilation counterfactual opens, boundary costs become fully salient, rote performance accumulates, and the councils must spend real enforcement to hold participation (extractiveness 0.55–0.58, theater 0.40–0.45, suppression 0.62–0.68). Base_properties are the interval-end (security-era) state. The claimed type is authored independently of these metrics: this reading holds the arrangement is a tangled structure — a genuine transmission function and real cost-bearers held in the same enforced practice — and is not tuned to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the councils' seat the arrangement is the community's survival infrastructure, built and maintained across catastrophes; from the assimilation-inclined seat it is a standing charge on a life they could otherwise lead, enforced by people who do not bear its heaviest costs; from the descendant-carriers' seat the same rehearsal is both wound and inheritance, and their dual position is the clearest seat-level evidence that the coordination function and the cost run through one structure; from the excluded women's seat the burden is distributed by councils their objections cannot reach. Same practice, four structurally different constraints — the engine computes the divergence from power, exit, and role; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The councils and the loyalists sit near the beneficiary end: the calendar subsidizes their standing and their self-understanding, and neither seat's exit is real (identity-locked). The community seat is the collective beneficiary whose only exit is transformation of the practice, not departure from it. Assimilation-inclined members sit near the target end — they bear the transfer, and their exit, while possible, costs family and network. Descendant-carriers sit mid-range: they pay the grief-reactivation costs and receive the inheritance the same practice delivers. The excluded women's burden is recorded in their situation, not in a victim declaration — an authored absence is commentary-grade and must not drive classification. Scope is continental at the collective seats and regional at the member seats; the engine scales effective extraction with scope, so the diaspora-wide, hard-to-verify character of the arrangement modestly amplifies what the member seats experience. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the right relationships, and the override mechanism keys on power atoms too coarsely to differentiate the three moderate-power seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrent catastrophe threatening communal extinction — is live and corroborated from outside the benefiting parties (the persecutors' own targeting of mourning assemblies; comparative reconstitution data), so no mandate-obsolescence mismatch fires and there is no dead-mandate capture flag to resolve. The classification work is elsewhere: the tangled_rope claim keeps both halves visible where each mislabel would hide one. Calling it pure coordination would erase the identifiable cost-bearers — the assimilation-inclined and the descendant-carriers — whose constrained exit is structural, not incidental. Calling it pure extraction would erase the transmission function the threat-cycle record keeps re-demonstrating: theater and enforcement demand fall precisely when catastrophe arrives, which a pure extraction structure does not do. A piton reading fails on the same evidence — the function is not atrophied but dormant-and-reviving on a threat cycle, and fixing is prohibitive because the function is real, not because inertia blocks a cheap repair. What this story cannot settle internally is whether the cycle's re-demonstrations are evidence of function or the intermittent-reinforcement mechanism that manufactures consent for the function — that is the threat_cycle_reinforcement_dependency omega, and it is the story's live edge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the catastrophe_memory_kernel — the survival_competence_reading. Sibling readings (symbol_continuity_reading, trauma_encoding_reading, boundary_maintenance_reading) instantiate different constraints over the same practice; which reading a corpus adopts changes the beneficiary set, the victim set, and epsilon itself. Where is the disagreement located, and what would each sibling change structurally?',
    'Corpus-level comparison across the four sibling stories. The disagreement is located in what the rehearsed content IS: operational response patterns (this reading), identity-bearing symbols (symbol_continuity), transmitted trauma (trauma_encoding), boundary markers (boundary_maintenance). Adopting symbol_continuity lowers epsilon toward identity-good coordination; adopting trauma_encoding shifts victims toward descendant psychological health and raises epsilon; adopting boundary_maintenance raises suppression and makes the extraction deliberate rather than incidental.',
    'Classification of the standing arrangement is reading-relative: the same practice classifies as moderate-extraction training under this reading, low-extraction identity continuity under the symbol sibling, and higher-extraction harm under the trauma sibling. Only cross-reading comparison, not within-story refinement, resolves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'One-of-four readings of the catastrophe-memory kernel; sibling readings change beneficiary/victim structure and epsilon.').

omega_variable(
    survival_function_empirical_status,
    'Does calendrical catastrophe-rehearsal actually transmit operational survival competence, or is the survival-competence attribution a retrospective justification projected onto practices maintained for other reasons?',
    'Matched comparison of threatened communities with and without maintained catastrophe-rehearsal: reconstitution speed after atrocity, cohesion retention under pressure, and transmission fidelity across generations; plus natural experiments where rehearsal lapsed and threat later arrived.',
    'If the transmission function is not real, the arrangement loses its coordination half and the same structure reclassifies toward pure boundary enforcement with a functionalist cover story; if real, the measured extraction is partly the price of a genuine collective good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_function_empirical_status, empirical, 'Whether the rehearsal-to-competence causal claim holds.').

omega_variable(
    suppression_mechanism_ambiguity,
    'How much of the members'' inability to leave the practice is structural (communal sanction, family rupture, network loss) and how much is internalized (obligation to the dead, guilt, identity fusion with the calendar)?',
    'Post-exit suppression trajectory of leavers: if the sense of obligation and grief-reactivation persist after network ties are replaced, the internalized share is substantial; if leavers report clean relief, the structural share dominates.',
    'If internalized, effective suppression is higher than the structural measure suggests — the constraint travels with the leaver — and the payer seats'' exit options are effectively worse than the authored ''constrained''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized share of exit-suppression.').

omega_variable(
    threat_cycle_reinforcement_dependency,
    'The arrangement''s measured extraction and theater fall in threat eras and rise in security eras: is the low threat-era value its true steady state, or is the oscillation itself the maintenance mechanism — catastrophe intermittently re-demonstrating a value that security eras erode (intermittent reinforcement)?',
    'Long-series analysis across multiple threat cycles: if each cycle''s function re-demonstration is what resets theater and restores compliance, the arrangement''s persistence depends on catastrophe recurring; if compliance holds through security eras on transmitted conviction alone, the cycle is exogenous noise.',
    'If intermittent reinforcement is the maintenance mechanism, the constraint''s persistence has a grim dependency — it is best maintained by the catastrophes it trains against — and its security-era trajectory is decay toward theatrical maintenance rather than a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_cycle_reinforcement_dependency, empirical, 'Whether the threat-cycle oscillation maintains the arrangement (intermittent reinforcement) or merely perturbs it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cata_tr_t45, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement(cata_tr_t90, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 90, 0.15).
narrative_ontology:measurement(cata_tr_t105, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 105, 0.35).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 120, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(cata_be_t45, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement(cata_be_t90, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 90, 0.38).
narrative_ontology:measurement(cata_be_t105, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 105, 0.52).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 120, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cata_su_t45, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 75, 0.38).
narrative_ontology:measurement(cata_su_t90, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 90, 0.3).
narrative_ontology:measurement(cata_su_t105, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 105, 0.58).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 120, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition, not epsilon decomposition: catastrophe_memory_kernel is a single persisting commitment (the mourning-practice arrangement) read four ways. Per the epsilon-invariance rule the readings are separate constraints, each with its own epsilon, beneficiaries, victims, and classification — this file is the survival-competence reading (moderate extraction; community-resilience beneficiary; boundary-cost and grief-reactivation victims). The sibling readings are linked as network neighbors: the symbol-continuity reading is the least contested and lends legitimacy the more contested survival and trauma claims borrow, and the boundary-maintenance reading shares this story's enforcement structure. No single story should attempt to classify 'the mourning practice' across readings — the colloquial label conflates structurally distinct claims with different failure modes and different victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
