% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Legitimation Pathway (Adoption Precedes Mandate)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   Across the state-formation episodes this reading covers, the same
 *   arrangement recurs: a costly self-enforcing convention loses its grip
 *   from below. Reform associations publish pledge registers; prominent
 *   families marry according to the new practice; adoption spreads through
 *   marriage-market arithmetic rather than police action; and the state's
 *   prohibition edicts arrive years after adoption has already peaked in the
 *   core regions, functioning as certification of a shift already
 *   accomplished. The epsilon referent is that standing arrangement,
 *   adoption-first legitimation with the mandate as ratifier, assessed by
 *   this reading's own lights: a coordination solution to a convention trap,
 *   with enforcement costs near zero because compliance is voluntary once
 *   expectations flip. KEY AGENTS (by structural relationship):
 *   central_state_authority, agenda-setting ratifier
 *   (institutional/arbitrage); early_adopter_households, first-moving
 *   beneficiaries who absorb pre-tipping-point penalties
 *   (moderate/constrained); majority_adopting_households, mass beneficiaries
 *   who move once expectations flip (organized/mobile); local_reform_elites,
 *   broker-beneficiaries running pledge societies
 *   (organized/identity_locked); holdout_communities, residual cost-bearers
 *   past the flip (moderate/trapped); historical_sociologists, analytical
 *   observers reconstructing adoption curves (analytical/analytical). Family
 *   note: this file is one of three readings of imposition_mechanism_kernel;
 *   the sibling readings are separate constraint stories linked through
 *   network.affects_constraints, each with its own epsilon and victim
 *   structure.
 *
 * KEY AGENTS:
 *   - central_state_authority: Agenda-setting ratifier (institutional/arbitrage) — issues the mandate after adoption peaks, converts momentum into administrative legitimacy at near-zero enforcement cost
 *   - early_adopter_households: First-moving beneficiaries (moderate/constrained) — absorb pre-tipping-point penalties and supply the visible proof the change was wanted
 *   - majority_adopting_households: Mass beneficiaries (organized/mobile) — move once expectations flip, turning an early-adopter experiment into common knowledge
 *   - local_reform_elites: Broker-beneficiaries (organized/identity_locked) — run pledge societies and registers; their standing is fused to the cause
 *   - holdout_communities: Residual cost-bearers (moderate/trapped) — keep the old practice past the flip and pay escalating penalties no one collects
 *   - historical_sociologists: Analytical observers (analytical/analytical) — reconstruct adoption curves and date mandates; their findings adjudicate the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.19).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.19).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Climb Legitimation Pathway (Adoption Precedes Mandate)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, 'b7be0c56-8eec-4966-817b-4ab2b6c1a8dc').
narrative_ontology:cs_kernel_codification('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', distributed).
narrative_ontology:cs_authority_grounding('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', expertise).
narrative_ontology:cs_interpretation_layer_present('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc').
narrative_ontology:cs_reading_relation('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', foundational, legitimacy_originates_in_popular_adoption).
narrative_ontology:cs_axiom_status(legitimacy_originates_in_popular_adoption, holdable).
narrative_ontology:cs_axiom_grounding('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', legitimacy_originates_in_popular_adoption, empirically_contingent).
narrative_ontology:cs_axiom('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', secondary, mandate_ratifies_rather_than_creates_legitimacy).
narrative_ontology:cs_axiom_status(mandate_ratifies_rather_than_creates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', mandate_ratifies_rather_than_creates_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', popular_adoption_precedence).
narrative_ontology:cs_drift_state('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', contemporary_microhistorical_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('b7be0c56-8eec-4966-817b-4ab2b6c1a8dc', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_households).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, majority_adopting_households).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, local_reform_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, central_state_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_households).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, holdout_communities).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, convention_devolution_theory).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, common_knowledge_tipping_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the mandate that ratifies the already-spreading practice change: drafting edicts, appointing inspection offices, publishing compliance statistics. By the time the mandate lands, most target households have already moved, so the office's main work is registering a shift it did not originate and converting the momentum into administrative legitimacy. Declining to ratify would forfeit the credit; timing the announcement is the lever this seat holds.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, central_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, central_state_authority, beneficiary).

% Abandon the practice before their neighbors do, accepting years of marriage-market penalty and gossip in exchange for ending the practice's direct costs sooner. They join pledge associations, sign public registers, and supply the visible examples later cited as proof the change was wanted. Once the majority moves, their early sacrifice converts into standing; before it, they cannot unilaterally return without paying twice.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_households, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_households, payer).

% Wait until the shift is unmistakably underway, then move with the crowd, abandoning the practice once the marriage market and neighborhood expectations have visibly flipped. They bear little transition risk and capture most of the benefit of ending the practice; their participation is what turns an early-adopter experiment into common knowledge.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, majority_adopting_households, beneficiary,
    organized, biographical, mobile, regional).

% Gentry, merchants, returned students, and association officers who fund pledge societies, publish registers of compliant families, and lobby the provincial office. Their local standing rides on the reform cause; retreating from it would cost them the identity and networks the campaign built. They broker between household readiness and official ratification.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, local_reform_elites, beneficiary,
    organized, biographical, identity_locked, regional).

% Remote counties and conservative lineages that keep the old practice past the tipping point. As surrounding districts flip, their daughters' marriage prospects narrow to within-group matches and traveling brokers, and the penalty grows yearly. No office collects anything from them; their loss is the residual price of living on the losing side of a convention that moved without them. Rejoining means admitting a decade of defiance; staying means shrinking pools.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, holdout_communities, payer,
    moderate, biographical, trapped, regional).

% Reconstruct adoption curves from marriage registers, association rosters, and mission reports; date mandates against them; publish the sequencing findings that decide which account of norm legitimation the era's transitions support. They collect nothing and pay nothing; their leverage is evidential.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, central_state_authority).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the mutual-expectation trap that keeps costly self-enforcing practices alive: no household can abandon the practice alone without marrying its children into penalty, so abandonment requires common knowledge that everyone is ready. Pledge registers, association memberships, prominent examples, and finally the state mandate supply graded signals that convert private readiness into public fact, letting large populations move within the same few seasons.
% TRANSFER_FUNCTION: Moves assurance and standing rather than goods: early adopters' transition risk is pooled and amortized across the whole adopting population; the adopting public extends legitimacy to the state free of charge when it ratifies; and the state returns certification and insurance (registration of compliant families, protection for late movers) to adopters. Nothing material is taken from anyone; the flows are informational and reputational.
% ABSENT_VOICES: Holdout communities and the practice's traditional defenders enter the record mainly through reformers' pamphlets and officials' compliance tables, not through their own testimony; the people who lived under the practice bodily speak through reformers' and missionaries' transcription, if at all. They sit in village registers and lineage genealogies, outside the mandate's drafting rooms: the mandate ratified a shift that no deliberation had consulted them on.
% DISAPPEARANCE_RATIONALE: Without the common-knowledge machinery, costly conventions persist indefinitely: prohibition edicts issued into unreformed expectation structures fail for generations, which is the recorded fate of bans in regions the climb never reached, while communities that achieve the shift sustain abandonment with almost no enforcement. Remove the arrangement and the world reverts to convention traps plus ineffective decrees.
% FOUNDING_PROBLEM: Costly self-enforcing conventions, practices each family would gladly drop if others dropped them simultaneously but cannot drop alone, blocking welfare-improving norm change during state consolidation and cultural transformation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: missionary and consular archives recording the repeated failure of prohibition edicts where adoption had not occurred; twentieth-century public-health campaign literature against parallel practices, which independently rediscovers and redeploys the pledge-and-register mechanism; and economic historians' replication of the marriage-market penalty arithmetic. None of these sources answers to the state offices or reform associations that benefited from the arrangement.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.19, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.19) because nothing material is taken: the arrangement's currency is assurance, and its one concentrated gain, the legitimacy the state banks on ratifying, is paid for by no one. Suppression is low (0.12) because persistence requires no coercion: expectations flip first, edicts second; the mild residual is post-shift social pressure on holdouts. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream by the engine. Theater (0.30) is the honest blemish: once adoption succeeds, the mandate apparatus increasingly commemorates rather than initiates, a drift the measurement series tracks on a single shared seven-point grid. Accessibility collapse (0.62) reflects the convention mechanics: after the flip, remaining outside the new practice is nearly unavailable inside affected marriage markets, though peripheral communities retain workable alternatives far longer than a natural law would permit. Resistance (0.14) is minimal because the mandate confronts a fait accompli; opposition is confined to holdout regions already losing the arithmetic. Claimed type is rope on the structural facts: a genuine collective-action problem solved with negligible coercive overhead, participants as net beneficiaries, no suppressed alternative. Coordination type is identity_coordination: the machinery coordinates reputation and marriage-market membership claims against shifting criteria, and the identity framing carries no hidden extraction here since extraction is independently low. Metrics and claim are authored independently; the engine computes per-seat verdicts from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   Seats should diverge sharply. From the central_state_authority seat the arrangement is nearly free authority: ratify, register, take credit. From the adopting-household seats it is deliverance from a trap they solved themselves, with the mandate as welcome insurance. From the holdout_communities seat the same structure reads as something done to them: a convention flipped without their consent, imposing yearly penalties they cannot escape, so that from that chair the shape resembles enforced extraction even though no one collects. Holdout communities could in principle coalize to restore the old equilibrium, but coordination is precisely what they lack, which is why their trapped exit does not convert into resistance. The engine computes these per-seat verdicts from power, exit, and directional data; the divergence between the state's rope and the holdouts' extraction-shadow is the perspectival measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: early and majority adopting households, reform elites, and the state all sit near the subsidized end, the state lightest-loaded of all since its gain costs it only the drafting of edicts. Holdout communities carry the arrangement's only real costs and derive d near the target end; their trapped exit amplifies the effective load the engine assigns them. Because their losses are diffuse and uncaptured by any seat, aggregate epsilon stays low even though the holdout seat's effective extraction is the story's maximum; that asymmetry between seat-level and aggregate readings is intended signal, not noise. Every paying seat operates at regional scope, where verification is comparatively easy, so no scope amplification inflates the figures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, convention traps blocking welfare-improving norm change, is live: every new reform campaign re-runs the machinery, so the arrangement is not obsolete and mandatrophy is unresolved. Classification discipline cuts both ways. Reading the state's involvement as coercion would mislabel a rope as a snare and invent victims who do not exist; reading the post-ratification ceremony as harmless decoration would miss the theater drift the series records. The guard condition: if commemorative mandates displace initiating ones and enforcement activations fall toward zero while the apparatus persists, the arrangement ages into inertial maintenance, coordination remembered and function gone. The omega post_ratification_theater_drift watches that seam.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'This story instantiates the endogenous_climb_reading of imposition_mechanism_kernel; are the corpus transitions it draws on actually governed by endogenous climb rather than by the sibling readings (exogenous_override_reading, hybrid_legitimation_reading)?',
    'Per-case sequencing tests: reconstruct adoption curves from marriage registers, association rosters, and mission reports; date mandates against them; measure enforcement expenditure. Mandate-after-adoption with low enforcement cost supports this reading; mandate-before with heavy enforcement supports the override sibling; mixed signatures support the hybrid sibling.',
    'If a sibling reading governs the cases, this constraint''s low epsilon and rope classification are wrong: the same colloquial label decomposes into a snare-shaped (override) or tangled_rope-shaped (hybrid) story with materially higher extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment, empirical, 'Which reading of the imposition-mechanism kernel the underlying norm transitions instantiate.').

omega_variable(
    mandate_sequencing_identifiability,
    'Can mandate-versus-adoption sequencing actually be established from the surviving record, given that ratification documents tend to backdate popular sentiment and adoption curves are reconstructed from fragmentary registers?',
    'Microhistorical triangulation: enrollment ledgers, pledge lists, subscription rolls, and mission reports dated against edict composition dates and court deliberation minutes, with sensitivity analysis over dating uncertainty.',
    'If mandates in fact preceded adoption in the flagship cases, this reading''s low-suppression, low-cost profile is misattributed: suppression existed but was deferred to the post-ratification period, and epsilon rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_sequencing_identifiability, empirical, 'Whether the archival record can settle the sequencing premise this reading stands on.').

omega_variable(
    successful_climb_survivorship,
    'Is this reading induced only from transitions that succeeded, leaving failed or stalled climbs outside the sample and biasing the measured costliness of the arrangement downward?',
    'Collect stalled-transition cases (campaigns that plateaued below the tipping point, pledge collapses, reversed mandates) and score their outcomes alongside the successes.',
    'Failed climbs impose real costs (polarization, burned signals, half-shifted marriage markets); including them raises the arrangement''s measured costliness and may push effective extraction above the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successful_climb_survivorship, empirical, 'Survivorship bias in the case base behind the endogenous-climb account.').

omega_variable(
    post_ratification_theater_drift,
    'Does the rising post-ratification ceremonial activity (anniversary edicts, compliance statistics, credit-claiming commemorations) mark the beginning of inertial drift, in which the mandate apparatus persists theatrically after its coordinating work is done?',
    'Track whether later mandates initiate transitions or merely commemorate completed ones; count enforcement activations against registered compliance over successive campaigns.',
    'If commemoration displaces initiation, the arrangement decays toward inertial maintenance and the theater_ratio series should be read as a leading indicator of that decay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_ratification_theater_drift, empirical, 'Whether the mandate apparatus is drifting from coordination toward ceremony.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endo_climb_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(endo_climb_tr_t0, observed).
narrative_ontology:measurement(endo_climb_tr_t5, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(endo_climb_tr_t5, observed).
narrative_ontology:measurement(endo_climb_tr_t10, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(endo_climb_tr_t10, observed).
narrative_ontology:measurement(endo_climb_tr_t15, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(endo_climb_tr_t15, observed).
narrative_ontology:measurement(endo_climb_tr_t20, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(endo_climb_tr_t20, observed).
narrative_ontology:measurement(endo_climb_tr_t25, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(endo_climb_tr_t25, observed).
narrative_ontology:measurement(endo_climb_tr_t30, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(endo_climb_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(endo_climb_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(endo_climb_be_t0, observed).
narrative_ontology:measurement(endo_climb_be_t5, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement_basis(endo_climb_be_t5, observed).
narrative_ontology:measurement(endo_climb_be_t10, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement_basis(endo_climb_be_t10, observed).
narrative_ontology:measurement(endo_climb_be_t15, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(endo_climb_be_t15, observed).
narrative_ontology:measurement(endo_climb_be_t20, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(endo_climb_be_t20, observed).
narrative_ontology:measurement(endo_climb_be_t25, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 25, 0.19).
narrative_ontology:measurement_basis(endo_climb_be_t25, observed).
narrative_ontology:measurement(endo_climb_be_t30, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(endo_climb_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(imposition_mechanism_kernel__endogenous_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how new norms gained legitimacy during state formation' decomposes per the epsilon-invariance principle into three readings of imposition_mechanism_kernel, each a separate story with its own epsilon: this file (endogenous climb, mandate ratifies prior adoption, low epsilon, rope-shaped), imposition_mechanism_kernel__exogenous_override_reading (coercion creates legitimacy, high epsilon, snare-shaped), and imposition_mechanism_kernel__hybrid_legitimation_reading (symbolic authority transfer plus incentives, intermediate epsilon, tangled_rope-shaped). The endogenous reading sits upstream of the hybrid: its demonstrated successes made pure-coercion accounts untenable and created the conditions in which the hybrid synthesis was proposed. Sibling files link back to this ID.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
