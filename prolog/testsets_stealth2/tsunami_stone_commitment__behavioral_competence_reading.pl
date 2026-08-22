% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Siting Commitment — Behavioral Competence Reading
 *   domain: disaster anthropology/commitment systems/institutional memory
 *
 * SUMMARY:
 *   Along the Sanriku coast and elsewhere in tsunami-prone Japan, villagers
 *   erected inscribed stones after the 1896 and 1933 tsunamis marking the
 *   reach of the water, with instructions to build and take refuge above the
 *   line. This story authors the arrangement under the
 *   behavioral_competence_reading: the inscriptions retained live behavioral
 *   force, held in place by active intergenerational transmission — elders
 *   teaching the stones' meaning, ritual renewal, correction of violations,
 *   and later school hazard education and heritage maintenance. The 2011
 *   tsunami supplied a harsh natural experiment: villages that kept the line,
 *   such as Aneyoshi, lost essentially no one, while towns that had developed
 *   seaward of their stones suffered mass casualties. Under this reading the
 *   arrangement is a functioning intergenerational coordination norm: it
 *   solves a memory problem no individual can solve, its participants are net
 *   beneficiaries, and its enforcement is social transmission rather than
 *   coercive machinery.
 *
 * KEY AGENTS:
 *   - - coastal_village_descendants: primary beneficiary (moderate/constrained) — inherit, transmit, and are protected by the siting norm; bear the cost of steeper, less convenient house sites
 *   - - village_elders_and_transmitters: agenda-setter (moderate/identity_locked) — the transmission practice that constitutes the arrangement's enforcement runs through them
 *   - - municipal_heritage_and_disaster_boards: secondary agenda-setter (organized/mobile) — institutionalize maintenance, re-engraving, and hazard education; gain legitimacy from the stones' authority
 *   - - coastal_developers_and_planners: cost-bearer (organized/mobile) — face the inscribed line as a binding restriction on the most valuable shorefront parcels
 *   - - shorefront_dependent_households: excluded cost-bearer (powerless/trapped) — pay the norm's daily access costs with no seat in the norm-setting conversation
 *   - - hazard_ethnographers: analytical observer (analytical/analytical) — document compliance, transmission intensity, and the line-versus-runup record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.24).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Siting Commitment — Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster anthropology/commitment systems/institutional memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '1bfae3ca-f181-4703-865d-7e5da207f9cc').
narrative_ontology:cs_kernel_codification('1bfae3ca-f181-4703-865d-7e5da207f9cc', fixed_text).
narrative_ontology:cs_authority_grounding('1bfae3ca-f181-4703-865d-7e5da207f9cc', lineage).
narrative_ontology:cs_interpretation_layer_present('1bfae3ca-f181-4703-865d-7e5da207f9cc').
narrative_ontology:cs_reading_relation('1bfae3ca-f181-4703-865d-7e5da207f9cc', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bfae3ca-f181-4703-865d-7e5da207f9cc', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('1bfae3ca-f181-4703-865d-7e5da207f9cc', foundational, inscriptions_retained_behavioral_force).
narrative_ontology:cs_axiom_status(inscriptions_retained_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('1bfae3ca-f181-4703-865d-7e5da207f9cc', inscriptions_retained_behavioral_force, empirically_contingent).
narrative_ontology:cs_axiom('1bfae3ca-f181-4703-865d-7e5da207f9cc', foundational, transmission_constitutes_enforcement).
narrative_ontology:cs_axiom_status(transmission_constitutes_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('1bfae3ca-f181-4703-865d-7e5da207f9cc', transmission_constitutes_enforcement, instrumental).
narrative_ontology:cs_reference_frame('1bfae3ca-f181-4703-865d-7e5da207f9cc', live_transmissive_commitment).
narrative_ontology:cs_drift_state('1bfae3ca-f181-4703-865d-7e5da207f9cc', contemporary_post_2011, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('1bfae3ca-f181-4703-865d-7e5da207f9cc', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_descendants).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, coastal_developers_and_planners).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, shorefront_dependent_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, municipal_heritage_and_disaster_boards).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, cross_generational_warning_transmission).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, precautionary_siting_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in the stone-line villages who inherit the siting rule as part of community membership: they build and rebuild above the inscribed inundation line, teach it to children, and accept the cost of steeper, less convenient house sites. Their payoff arrives at recurrence intervals longer than any individual's memory of the last event. Leaving the arrangement means moving away from fishing grounds and kin networks; staying inside it means honoring a line set by ancestors they never met.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_village_descendants, beneficiary,
    moderate, generational, constrained, local).

% Older villagers, teachers, and ritual officiants who carry the duty of transmission: they explain the stones' meaning, correct building and evacuation behavior that ignores them, lead anniversary rites, and decide what the terse inscriptions require in new situations — a new road, a seawall, a school site. Their standing in the village is bound up with the memory-keeping office; setting it aside would mean becoming an ordinary elder with no office at all.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, village_elders_and_transmitters, agenda_setter,
    moderate, generational, identity_locked, local).

% Town-level boards that designate stones as cultural properties, fund cleaning and re-engraving, and fold them into school hazard education and evacuation planning. They receive administrative legitimacy and a low-cost hazard-communication asset from the stones' authority, and they can defund maintenance without personal risk.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, municipal_heritage_and_disaster_boards, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, municipal_heritage_and_disaster_boards, beneficiary).

% Construction firms, landowners, and town planners whose most valuable parcels lie seaward of the inscribed lines. Where transmission is strong they build above the line and absorb the opportunity cost; where it weakens they have repeatedly extended development into the inundation zone and pressed for engineered defenses as a substitute for siting discipline. Capital can move to other coasts; the line follows the land, not the firm.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_developers_and_planners, payer,
    organized, biographical, mobile, regional).

% Fishing families and small shorefront businesses whose livelihoods pull them toward the water: gear sheds, boat access, and customers all sit at the shoreline the stones warn away from. They pay the norm's cost daily — longer hauls, steeper plots, lost foot-traffic — while the survival benefit accrues to the household at unpredictable intervals. The inscribed line was fixed by generations now dead; no living forum exists in which they could renegotiate it, only comply, quietly evade, or leave.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, shorefront_dependent_households, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, shorefront_dependent_households, excluded).

% Researchers and disaster ethnographers who map inscribed lines against measured runup, reconstruct transmission practices from village records and ritual calendars, and compare mortality across heeding and non-heeding towns. They bear none of the arrangement's costs and collect none of its benefits; their output is the record the other seats argue from.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, hazard_ethnographers, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational memory problem in tsunami hazard management: recurrence intervals run decades to a century, longer than the lifespan of lived witness, so each generation otherwise relearns the danger by dying. The inscribed stone fixes the observed inundation boundary in a durable, self-explanatory marker and attaches a behavioral rule to it — settle, build, and flee above this line — that a community can transmit without specialist infrastructure.
% TRANSFER_FUNCTION: Moves settlement and construction activity from the inundation zone to the safe zone; transfers the cost of caution from future generations, who cannot bargain or vote, to present ones, who give up shorefront convenience; and transfers epistemic authority from living witnesses, who die, to inscribed testimony, which persists.
% ABSENT_VOICES: The line's authors are dead, so no living party can renegotiate the boundary — the conversation the stones permit is affirmation, not amendment. Shorefront-dependent households and developers who would trade siting bans for engineered defenses (higher seawalls, faster warnings) have no seat in the transmission circle where the norm is maintained; their objections surface only as quiet evasion or post-disaster rebuilding disputes.
% DISAPPEARANCE_RATIONALE: If the stones and their transmitted force vanished overnight, siting discipline would depend entirely on living memory and official hazard maps. Memory of the last event fades within two to three generations; seaward development would resume in the intervals between events, as it did in towns where transmission lapsed, and the next tsunami would find housing, workplaces, and critical facilities back inside the inundation zone. Engineered defenses and warning systems would slow the rearrangement but not replace the siting norm — in 2011, defenses were overtopped in town after town, while villages that kept the line lost almost no one.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly 22,000 people, and again after the 1933 Shōwa Sanriku tsunami, villagers erected inscribed stones marking how far the water had reached, with instructions to build and take refuge above the line. The founding problem: a hazard whose recurrence interval exceeds communal living memory, which each generation must otherwise relearn at the cost of mass death.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: hazard researchers have mapped inscribed lines against the 2011 runup and the mortality record — villages that kept the line, such as Aneyoshi, lost essentially no one, while towns that had built seaward of their own stones suffered mass casualties — and municipal and national disaster-mitigation programs act on the problem's liveness by funding new stones and folding the old ones into mandatory school hazard education. No party disputes that the hazard recurs; the live dispute concerns whether the stones caused the compliance, not whether the founding problem exists.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is floor-level (0.05) because the stone collects nothing for anyone: the cost of the norm (foregone shorefront convenience) and its benefit (survival at recurrence intervals) fall on the same population across time, which is the signature of coordination rather than extraction. Suppression (0.24) is authored as a raw structural property — it is not scaled by power or scope — and reflects the post-2011 enforcement surface: communal sanction plus regulatory limits on inundation-zone building; between events it decays (see the series). Theater (0.11) is low: ritual veneration exists, but the 2011 record shows the transmitted line did load-bearing work. Accessibility collapse is moderate (0.45): the stone forecloses siting socially, not physically — building below the line remained materially open everywhere and was taken wherever transmission weakened. Resistance (0.30) records real but unsustained pushback: shorefront development pressure and the recurring preference for engineered substitutes. On type: the manifest's expected delta suggested piton, but the framework's piton requires an atrophied function maintained by inertia and theater, with no party benefiting enough to maintain it and no party hurt enough to fix it — that signature belongs to the commemorative-husk sibling of this kernel, not to a reading whose premise is retained live force and net-beneficiary transmission. The structurally true claim here is rope. Coordination type is authored as identity_coordination: the scarce commodity was never information about tsunamis but durable behavioral commitment — the stone coordinates membership in a community that heeds its own dead — and the type's conservative floor (0.08) sits above the authored ε, correctly treating the residue as coordination cost rather than extractive overhead. The measurement series share one time grid (t = 0, 20, 40, 60, 80, 100, 115, 130); their shape is event-cyclic rather than monotonic: enforcement spikes after each validating catastrophe and decays across the memory gap between events, while theater humps as witness generations die and is pulled back down by institutionalized transmission and by each new test. The oscillation is not an extraction mechanism — it is the arrangement's designed refresh cycle, in which each catastrophe re-validates the testimony that memory decay erodes. Base properties are measured at interval end, just past the 2011 re-arming phase as it settles into institutionalized form. fixing_cost is authored cheap: the arrangement's force decays without active transmission — it is cheap to lose and catastrophe-priced to rebuild — which is why the transmission practice, not the stone, is the load-bearing element.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the descendant and elder seats the arrangement reads as inheritance and duty: coordination they maintain because they are its beneficiaries. From the developer seat the same line reads as a binding restriction on the most valuable parcels — mild, mobile, and periodically contested. From the excluded shorefront-household seat it reads as an unrenegotiable edict from the dead: daily cost without a voice in the rule. The elders' exit is identity-locked in the relational sense: their standing in the village is constituted by the memory-keeping office itself, so abandoning transmission would dissolve the self, not merely forfeit a benefit; if that identity frame broke — if transmission were fully absorbed by schools and municipal boards — the elder seat would collapse into an ordinary mobile beneficiary and the enforcement structure would shift from relational to institutional. The engine computes these per-seat classifications from the declared power, exit, and role data; the divergence between the descendant seat and the developer seat on the same stones is the perspectival fact this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   coastal_village_descendants are declared beneficiaries and sit near the beneficiary end: the arrangement subsidizes their survival at zero fiscal cost. village_elders_and_transmitters administer the arrangement and live inside its protection. municipal_heritage_and_disaster_boards gain legitimacy and a low-cost hazard-communication asset — a modest, non-extractive benefit. coastal_developers_and_planners are declared victims in the cost-bearing sense: they forgo shorefront parcels and receive no episodic survival payoff of their own, so their derived directionality sits toward the target end, damped by mobile capital. shorefront_dependent_households are the override case: the derivation from victim-plus-trapped would push their d high, but they are also the arrangement's primary intended beneficiaries — their costs are concentrated and daily while their benefit is episodic and survival-scale, putting their net structural position near symmetric. The override sets d = 0.45 for that seat. No seat collects the arrangement's negligible extraction; gain_flow is authored as diffuse — an affirmative finding after checking every seat, not a default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a hazard whose recurrence outlives living memory — remains live, so the arrangement has not outlived its function and mandatrophy does not attach. The classification work here is preventing a mislabel in both directions. Between catastrophes the arrangement can look piton-like: enforcement thins, ritual grows, and decades pass with no observable compliance to point to — a naive reading mistakes the memory-gap interim for atrophy. The 2011 differential-mortality record shows the function was live where transmission held, which blocks the piton mislabel for this reading. In the other direction, the floor-level extraction and gentle enforcement block a snare or tangled-rope mislabel: nothing is extracted from anyone for anyone else's benefit, and the cost-bearing seats (developers, shorefront households) are damped by mobility and by their own survival stake respectively. The R5 fields record the congruence the mismatch consumer checks for: founding_problem_status = live with disappearance_verdict = world_rearranges — no zombie flag, no capture signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is the behavioral_competence_reading of the tsunami_stone_commitment kernel; what would change structurally if the commemorative_husk_reading were adopted as the corpus''s account of the same stones?',
    'Comparative read of the linked sibling story: same stones, same interval, with husk-side ε, theater, and beneficiary structure authored independently.',
    'Under the husk reading the same arrangement authors as theater-maintained inertia with negligible live function — type shifts toward piton, theater_ratio dominates the profile, and the transmission practice re-describes as ceremony rather than enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Committer index: one reading of the tsunami_stone_commitment kernel; sibling readings are separate constraints.').

omega_variable(
    compliance_causal_attribution,
    'Was compliance with the inscribed lines caused by live transmission of the norm, or coincidental with topography, household wealth, and engineered defenses?',
    'Stone-line versus 2011 runup mapping with controls for elevation, distance to coast, seawall presence, and settlement age; transmission-intensity reconstruction from village records, school curricula, and ritual calendars.',
    'If coincidence dominates, the behavioral reading collapses toward the husk sibling: theater_ratio rises, the live-force axioms lose their empirical grounding, and the arrangement reclassifies toward inertial maintenance. If transmission carries the effect, this reading''s low-ε rope profile is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_causal_attribution, empirical, 'Whether the stones'' behavioral force was causal or coincidental — the empirical substance of the kernel contest.').

omega_variable(
    engineering_substitution_question,
    'Do modern defenses (seawalls, warning systems, official hazard maps) now substitute for the stones'' behavioral function, converting the commitment toward commemorative status going forward?',
    'Compare siting and evacuation compliance in towns with and without stone-transmission traditions under equivalent engineering investment; test whether stone-based education changes behavior that warnings alone do not.',
    'If substitutable, the arrangement''s forward function decays and the husk reading becomes descriptively true of the future even if not of the past; if complementary, the live-force reference frame persists and the post-2011 re-arming is functional rather than ceremonial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_substitution_question, empirical, 'Forward-looking substitution risk from engineered defenses to the transmitted norm.').

omega_variable(
    line_renegotiation_rigidity,
    'Is the unrenegotiable character of the inscribed line (its authors are dead) a protective feature against present-bias erosion, or a cost that blocks incorporation of new bathymetry, engineering, and settlement data?',
    'Compare hazard outcomes and adaptation speed between stone-governed siting and map-governed siting across successive revisions of official hazard maps.',
    'If rigidity is protective, the excluded-seat grievance is the price of the coordination function and the rope profile holds; if it blocks needed adaptation, a hybrid form (stone as anchor, periodic formal revision) would dominate and the enforcement structure would need re-description.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(line_renegotiation_rigidity, conceptual, 'Whether the line''s irrevisability is functional discipline or maladaptive rigidity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsunami_behavioral_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t0, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t20, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t20, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t40, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t40, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t60, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t80, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t80, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t100, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t115, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 115, 0.1).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t115, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t130, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 130, 0.11).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t130, observed).

% Extraction over time
narrative_ontology:measurement(tsunami_behavioral_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t0, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t20, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.03).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t20, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t40, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t40, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.04).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t60, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t80, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 80, 0.04).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t80, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t100, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t115, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 115, 0.05).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t115, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t130, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 130, 0.05).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t130, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsunami_behavioral_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t0, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t20, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t20, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t40, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t40, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t60, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t60, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t80, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t80, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t100, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t100, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t115, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 115, 0.26).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t115, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t130, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 130, 0.24).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t130, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% The kernel tsunami_stone_commitment decomposes into readings with structurally distinct ε: this behavioral_competence_reading (live force via transmission, floor-level ε, rope-claimed), the commemorative_husk_reading (decayed symbol, theater-dominated, piton-shaped), and the catastrophe_validation_axis (the 2011 event as binary test evidence). The colloquial label 'the tsunami stones' covers distinct structural claims about whether the inscribed line caused compliance, so each reading is authored as its own story with its own ε, beneficiaries, and claimed type, linked here per the ε-invariance principle. This reading sits upstream of the validation-axis reading in one respect: the compliance record this reading asserts is the evidence the validation reading consumes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__behavioral_competence_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
