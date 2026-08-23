% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__social_scaffold_reading, []).

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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly House-Rules Social Playability Regime (Social Scaffold Reading)
 *   domain: game theory/social coordination/institutional design
 *
 * SUMMARY:
 *   A recurring board-game group plays the property-trading game whose
 *   printed rules concentrate wealth and eliminate failing players, typically
 *   hours before the evening ends. At casual tables the printed text is
 *   corrected in play: jackpot funds accumulate on a rarely-visited square,
 *   passing the starting corner pays double, bankrupt players receive loans
 *   or mercy terms, and the harshest endgame the text mandates is slowed
 *   until the gathering can close on its own schedule. This story authors
 *   THAT arrangement — the house-rules regime as practiced — under the social
 *   scaffold reading of the monopoly_rulebook kernel: the text's correction
 *   mechanism is treated as a community-adjustable parameter whose governing
 *   test is whether the evening stays socially playable. The ε referent is
 *   the standing arrangement under contest (the practiced regime at casual
 *   tables), assessed by this reading's own lights — not the raw-text game
 *   the reading modifies, and not the regimes the sibling readings
 *   instantiate. Sibling readings (extraction_demo_reading,
 *   tournament_orthodoxy_reading) are separate constraint stories linked
 *   through network.affects_constraints; per the ε-invariance principle each
 *   carries its own ε, beneficiaries, and type, and none is averaged into
 *   this one. KEY AGENTS (by structural relationship): -
 *   game_night_social_group: Primary beneficiary (organized/mobile) —
 *   collects continued cohesion, full tables, repeat attendance -
 *   trailing_players: Secondary beneficiary (moderate/constrained) — receive
 *   injected liquidity that keeps them seated and invested - leading_player:
 *   Primary cost-bearer (moderate/constrained) — pays dilution of an earned
 *   advantage; consented ex ante - rules_purist: Secondary cost-bearer
 *   (moderate/identity_locked) — bears the fidelity cost of playing under
 *   rejected rules - host: Agenda-setter (moderate/mobile) — convenes
 *   consensus, frames which corrections are in force, enforces by tone -
 *   game_publisher: Institutional observer (institutional/analytical) —
 *   watches the practice, occasionally absorbs corrections into official
 *   variants
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.44).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.26).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly House-Rules Social Playability Regime (Social Scaffold Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game theory/social coordination/institutional design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '464a5d49-8ffd-49ea-ac3d-f15e3f79cd64').
narrative_ontology:cs_kernel_codification('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', fixed_text).
narrative_ontology:cs_authority_grounding('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', practice).
narrative_ontology:cs_interpretation_layer_present('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64').
narrative_ontology:cs_reading_relation('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', foundational, correction_must_remain_socially_playable).
narrative_ontology:cs_axiom_status(correction_must_remain_socially_playable, holdable).
narrative_ontology:cs_axiom_grounding('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', correction_must_remain_socially_playable, instrumental).
narrative_ontology:cs_axiom('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', foundational, table_consensus_authorizes_rule_variation).
narrative_ontology:cs_axiom_status(table_consensus_authorizes_rule_variation, holdable).
narrative_ontology:cs_axiom_grounding('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', table_consensus_authorizes_rule_variation, conventional).
narrative_ontology:cs_reference_frame('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', community_corrected_social_instrument).
narrative_ontology:cs_drift_state('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', contemporary_casual_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('464a5d49-8ffd-49ea-ac3d-f15e3f79cd64', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, game_night_social_group).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, trailing_players).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, leading_player).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, rules_purist).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, host).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, rules_purist).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, community_correction_doctrine).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, social_playability_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A recurring circle of friends or family that meets to play the property-trading game together. It adopts, revises, and retires table rules by loose consensus, and its members keep coming back because the evenings stay inclusive. Its alternatives — switching games, skipping weeks, letting the tradition lapse — are real but carry the cost of losing a shared ritual.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_night_social_group, beneficiary,
    organized, biographical, mobile, local).

% Owns the board, sets the date, and frames which table rules are in force ('at our table we play with the jackpot'). Proposes corrections when a session turns sour and enforces them lightly by tone rather than penalty. Gains full tables and repeat attendance; can end the tradition or switch games at will.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, host, agenda_setter,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, host, beneficiary).

% The participants currently behind in cash and property. Jackpot windfalls, doubled starting-corner salaries, and forgiven loans keep them solvent and seated instead of bankrupt and bored. They advocate loudest for the redistribution rules but cannot unilaterally impose them; walking out mid-evening costs them the social night, not just the game.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, trailing_players, beneficiary,
    moderate, immediate, constrained, local).

% The participant whose play has accumulated the strongest position. Table rules tax the durability of that lead — rescued opponents, inflated purses, longer resistance — and objections are read as poor sportsmanship. Consent was given before play began, and social standing rewards gracious acceptance; declining future game nights is the real exit.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, leading_player, payer,
    moderate, immediate, constrained, local).

% A member who believes the game should be played exactly as printed and says so before every session, then loses the vote. Keeps attending because the evening's company matters more than the rulebook, and quietly enjoys the gatherings the corrected rules make possible. Dropping the fidelity commitment would mean revising a self-image built around knowing and honoring the text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rules_purist, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, rules_purist, beneficiary).

% Prints and licenses the rulebook the tables correct. Watches house-rule culture from outside and occasionally absorbs popular corrections into official speed-play variants sold as editions or expansions. Its commercial interest lies in the folk practice staying lively enough to sell to, not in any particular table's arrangements.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_publisher, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, trailing_players).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a multi-hour competitive leisure activity socially viable: the printed correction mechanism removes failing participants from play, and the table's adjustments stagger and soften those removals so every seat stays occupied and invested until the gathering winds down on its own schedule.
% TRANSFER_FUNCTION: Moves purchasing power and staying-power from the current leader's accumulated position to currently trailing participants (jackpot funds, bonus salaries, forgiven debt), and moves rule-authority from the printed text to the table's standing consensus.
% ABSENT_VOICES: The rules purist is present but chronically overridden — their objection is heard and outvoted every session. Genuinely absent: newcomers and children who inherit entrenched table rules as if they were the printed canon and never get the chance to opt into the text; and stronger players who never join the table at all because its reputation signals casual play. Both sit outside the room the consensus governs.
% DISAPPEARANCE_RATIONALE: If the table's corrections vanished overnight, sessions would revert to the printed arc: early bankruptcies, mid-evening eliminations, shortened and thinner gatherings. Groups would respond within weeks — shrinking rosters, switching to shorter games, or migrating to official speed variants — because the raw endgame cannot sustain a full evening of mixed-skill company.
% FOUNDING_PROBLEM: The printed game eliminates its losers hours before the evening ends, turning a social gathering into spectatorship for the bankrupt and an awkward early-departure problem for the host; tables built correction rules to keep every chair filled until the night was over.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the game's publisher — with no stake in table autonomy — introduced official speed-play variants precisely because the printed game's duration and elimination arc deterred casual buyers; game-design commentary and widely replicated surveys of play habits independently document that unmodified sessions run long and eliminate players early. No one outside the benefiting tables needs convincing that the raw endgame strains a mixed-company evening.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__social_scaffold_reading_tests).
:- end_tests(monopoly_rulebook__social_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores describe the regime at the interval's end (month 24 of a tracked group lifecycle). Extractiveness 0.44: the leader seat surrenders real positional value — rescued opponents, inflated purses, prolonged resistance — but the surrender is consented ex ante, bounded by the session, and reciprocated in kind when positions rotate. Suppression 0.26: the regime is held by majority norms and host framing rather than penalty; a dissenter can lose a vote but not a livelihood. Theater 0.27: most corrections still do their work, though a growing minority of retained rules (ceremonial jackpot counts, legacy mercy terms) persist past their function as the group adopts faster formats. Accessibility collapse 0.30 and resistance 0.30: the printed text remains visibly available — anyone may propose a by-the-book session — and the purist's recurring objection documents that alternatives are outvoted, not foreclosed. All three temporal series share one grid (months 0–24 at 4-month steps). The lifecycle arc is deliberate: extractiveness and enforcement rise through entrenchment (months 8–12) as skill divergence peaks, then ease as the group matures, rotates positions, and adopts official speed variants — the support structure bearing load, then partially retiring. Suppression is authored as a raw structural property; the engine scales only extractiveness. The purist's identity lock is hobbyist-constitutive: 'games are played as written' fuses rule-fidelity with self-image, so exit into cheerful acceptance costs more than the material stakes justify; if that frame broke, the seat would migrate to competitive subcultures where the text is honored, and its burden would vanish without the regime changing. Suppression here is predominantly structural-social (majority expectation, host framing) with a smaller internalized component (politeness norms that make objection feel like spoiling the evening) — roughly 70/30 by the observed cost of dissent.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the trailing seat the regime is rescue: liquidity arrives exactly where the printed game would expel them. From the leader seat the same rules are a tax on demonstrated skill, softened only by consent and rotation. From the host seat it is hospitality infrastructure — the difference between a full table and a dwindling one. From the purist seat it is a weekly small defeat. The engine derives these divergences from the declared roles, power, and exit options; the authored scaffold claim does not adjudicate them, and a computed hybrid verdict at the leader seat would be signal, not error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the group (mobile exit, collective consent) and for trailing players (constrained but net-collecting). Victim declarations drive high directionality for the leader and the purist; the purist's identity_locked exit pushes them toward the full-target end, which honestly encodes that the regime's suppression of their preferred alternative is the sharpest cost anyone at the table bears. The host sits near the beneficiary pole as agenda-setter who also collects attendance. Spatial scope is uniformly local — verification is trivial at table range, so no scope amplification is expected. No directionality overrides are authored: the derivation from declared roles and exit options captures the structural relationships, and the tempering facts (ex-ante consent, position rotation) are interpretive caveats recorded here rather than corrections imposed on the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim disciplines both neighboring errors. Read as pure coordination, the regime's asymmetric cost on the leader seat disappears — but that cost is real and recurring. Read as extraction, the regime's transitional job vanishes — yet the arrangement demonstrably retires pieces of itself as groups mature and as official variants absorb the need, which pure extraction never does. The sunset clause is the load-bearing declaration: the regime's justification is the fragile middle phase of a group's life and the vulnerable middle phase of each session, not a steady state. Mandatrophy is not resolved: the mandate (keep mixed-company evenings playable) is still live wherever novices meet the printed game, though individual retained rules increasingly outlive their functions — the slowly rising theater ratio is that residue, not yet the regime's substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_realization_uncertainty,
    'Is the table-rules regime genuinely transitional — retiring as groups mature, sessions end, or official speed variants absorb the need — or does it self-perpetuate as a permanent parallel constitution handed down informally?',
    'Longitudinal observation of recurring groups: track whether redistribution rules survive into a group''s skilled, mature phase or are retired once faster formats arrive.',
    'If self-perpetuating, the arrangement is a steady-state institution misdescribed as transitional support, and the classification shifts from scaffold toward a permanent coordination regime with the leader seat''s cost baked in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_realization_uncertainty, empirical, 'Whether the regime realizes its built-in sunset or entrenches.').

omega_variable(
    consent_depth_ambiguity,
    'Is participant consent to the table''s corrections substantive — informed, revisable, revocable — or acquiescence under social expectation?',
    'Observe exit behavior and veto attempts: whether dissenting members successfully decline rules, skip sessions without sanction, or face escalating social cost for opting out.',
    'If acquiescence dominates, suppression is higher than authored, the leader seat''s burden approaches the coerced end, and the arrangement drifts toward hybrid extraction rather than supported transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_depth_ambiguity, empirical, 'Depth of consent beneath the regime''s consensus surface.').

omega_variable(
    kernel_design_intent_location,
    'This story is one reading of the monopoly_rulebook kernel (social_scaffold_reading); sibling readings extraction_demo_reading and tournament_orthodoxy_reading instantiate different constraints. Which reading tracks the rulebook''s design intent, and where exactly do the readings diverge?',
    'Historical and designer-record evidence: the game''s lineage traces to a deliberately didactic predecessor, while the mass-market edition''s own supplementary variants concede the duration problem — locate whether the text''s correction mechanism was authored as demonstration, as adjustable parameter, or as immutable standard.',
    'If the text''s intent is demonstration, this reading is a user-side override of design intent and the constraint''s authority rests wholly on table practice; if the text concedes adjustability, the scaffold reading is continuous with the artifact''s own logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_design_intent_location, conceptual, 'Location of the inter-reading disagreement: what the text mandates about its own correction mechanism.').

omega_variable(
    enjoyment_peak_duration_tradeoff,
    'Does liquidity injection maximize aggregate enjoyment across the session, or does it push past the pleasure peak into the drag that gives house rules their ''game never ends'' reputation?',
    'Session-length and enjoyment curves across groups using varying redistribution intensities; compare reported satisfaction at 90, 150, and 210 minutes.',
    'If the regime systematically overshoots the peak, part of its operation is negative-value persistence — maintenance without benefit — and theater indicators deserve upward revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enjoyment_peak_duration_tradeoff, empirical, 'Whether redistribution overshoots the enjoyment optimum.').

omega_variable(
    authority_framing_ambiguity,
    'Is the adjudicating authority for this reading best framed as table practice (the playing group''s action IS the standard, with a local interpretive layer) or as distributed authority (no designated interpreter; every table legislates differently and no center arbitrates)?',
    'Examine dispute resolution: when two members disagree about a table rule mid-session, is there a recognized local adjudicator (host, majority) whose ruling binds, or does resolution vary arbitrarily with no binding center?',
    'Under the distributed framing, the declared interpretive layer loses validity and the reading''s authority claim weakens from ''practitioners legislate'' to ''nobody arbitrates''; classification consequences follow the authority structure''s stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_ambiguity, conceptual, 'Framing under-determination in the reading''s authority declaration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__social_scaffold_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__social_scaffold_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__social_scaffold_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__social_scaffold_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(mono_tr_t24, monopoly_rulebook__social_scaffold_reading, theater_ratio, 24, 0.27).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mono_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(mono_be_t24, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 24, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(mono_su_t4, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(mono_su_t8, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(mono_su_t12, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(mono_su_t16, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(mono_su_t24, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 24, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Monopoly rules' decomposes into three structurally distinct constraints under the ε-invariance principle — the extraction-demo reading (high-ε; victims are the players subjected to the mandated harsh endgame as demonstration), this social-scaffold reading (moderate-ε; transitional redistribution regime; beneficiaries are the social group), and the tournament-orthodoxy reading (text-as-standard; victims are players whose comparative rankings are invalidated by variation). The printed artifact feeds all three; this reading links both siblings because house-rule practice is the live interface where the demonstration claim and the orthodoxy claim are daily negotiated. Each file carries its own ε, beneficiaries, and claimed type; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
