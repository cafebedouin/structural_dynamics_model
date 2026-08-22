% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability Doctrine as Noble Lie Screening Covert Adaptation
 *   domain: political philosophy/constitutional theory/commitment systems
 *
 * SUMMARY:
 *   This story authors the adaptive_fiction_reading of the Lycurgan
 *   settlement: the celebrated immutability of Sparta's constitution — the
 *   orally fixed Rhetra, oracle-sealed, sworn unamendable — operated less as
 *   a binding limit than as a legitimating screen behind which the ephorate,
 *   the gerousia, and the kings continuously adjusted the order through
 *   interpretation, precedent, and quiet non-enforcement. The fiction solved
 *   a real coordination problem (no factional reopening of the fundamental
 *   settlement; a sacral warrant that economized on enforcement) while
 *   concentrating adaptive discretion in the few and fixing ordinary citizens
 *   to the letter of a text their rulers were free to bend. The epsilon
 *   referent is the standing arrangement — the immutability regime as
 *   actually practiced across the archaic interval — assessed by this
 *   reading's own lights; the sibling readings of the same kernel are
 *   separate constraints linked in the network, not hedges inside this one.
 *   Claim and metrics are independent authored facts: the claim is
 *   tangled_rope (real coordination leg, real extraction leg), while the
 *   metrics describe moderate extraction, articulation-targeted suppression,
 *   rising theatrical maintenance, and monotonically decaying enforcement.
 *   Interval mapping assumption: one time unit approximates twelve years, T=0
 *   at the mid-eighth-century consolidation of the settlement, T=24 at the
 *   early fifth-century strain period when citizen-roll decline and helot
 *   unrest became visible.
 *
 * KEY AGENTS:
 *   - ephorate_magistrates: agenda-setting interpreter (organized/constrained) — converts the immutable text into whatever the moment requires; collects the adaptive discretion the fiction generates
 *   - dual_kingship: sacral beneficiary (powerful/identity_locked) — dynastic standing guaranteed by a doctrine the office cannot repudiate
 *   - gerousia: continuity beneficiary (organized/identity_locked) — lifetime elders whose personal authority and the doctrine's authority are one asset
 *   - spartiate_homoioi: principal paying seat (moderate/trapped) — enjoys the machine's guarantees, bound to the letter, no revision channel
 *   - hypomeiones_degraded_citizens: excluded casualty (powerless/trapped) — unnamed by the equality vocabulary of the settlement
 *   - helot_population: excluded casualty (powerless/trapped) — subordination frozen as eternal by the same permanence doctrine
 *   - ancient_analysts: analytical observer (analytical/analytical) — external testimony on the gap between the permanence claim and observed practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.52).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.58).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability Doctrine as Noble Lie Screening Covert Adaptation").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political philosophy/constitutional theory/commitment systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, 'ec0d36f1-b9a0-4264-a992-839254804887').
narrative_ontology:cs_kernel_codification('ec0d36f1-b9a0-4264-a992-839254804887', fixed_text).
narrative_ontology:cs_authority_grounding('ec0d36f1-b9a0-4264-a992-839254804887', lineage).
narrative_ontology:cs_interpretation_layer_present('ec0d36f1-b9a0-4264-a992-839254804887').
narrative_ontology:cs_reading_relation('ec0d36f1-b9a0-4264-a992-839254804887', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('ec0d36f1-b9a0-4264-a992-839254804887', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('ec0d36f1-b9a0-4264-a992-839254804887', foundational, immutability_doctrine_is_operative_fiction).
narrative_ontology:cs_axiom_status(immutability_doctrine_is_operative_fiction, holdable).
narrative_ontology:cs_axiom_grounding('ec0d36f1-b9a0-4264-a992-839254804887', immutability_doctrine_is_operative_fiction, empirically_contingent).
narrative_ontology:cs_axiom('ec0d36f1-b9a0-4264-a992-839254804887', foundational, interpretation_constitutes_unofficial_amendment).
narrative_ontology:cs_axiom_status(interpretation_constitutes_unofficial_amendment, holdable).
narrative_ontology:cs_axiom_grounding('ec0d36f1-b9a0-4264-a992-839254804887', interpretation_constitutes_unofficial_amendment, conventional).
narrative_ontology:cs_reference_frame('ec0d36f1-b9a0-4264-a992-839254804887', oracle_ratified_immutable_charter).
narrative_ontology:cs_drift_state('ec0d36f1-b9a0-4264-a992-839254804887', late_archaic_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec0d36f1-b9a0-4264-a992-839254804887', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate_magistrates).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, dual_kingship).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartiate_homoioi).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, hypomeiones_degraded_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartiate_homoioi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five magistrates elected annually; they convene and preside over the councils and assembly, supervise the kings, and guard the settled order. Their decisive asset is interpretive: when novel circumstances arise, they pronounce what the founder's unalterable rules require, and the pronouncement stands. The pretense of permanence places the polis's entire adaptive capacity in their hands without requiring them to admit anything has changed. After their single year they return to the citizen ranks, which keeps their attention on short-term patching rather than long-horizon redesign.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephorate_magistrates, agenda_setter,
    organized, immediate, constrained, national).

% Two hereditary royal houses holding military command and sacred offices. The doctrine that the founder's settlement may never be altered guarantees their preeminence against ordinary legislation — their rank predates and outranks anything the assembly could enact. They cannot repudiate the doctrine without dissolving the sacral ground of their own office, so leaving the framework is unavailable from inside it. In practice they chafe under the magistrates' oversight while depending on the same permanence claim for their standing.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, dual_kingship, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, dual_kingship, agenda_setter).

% Twenty-eight elders drawn from the aristocracy, serving for life, who prepare business for the assembly and hold veto over its decisions. Lifetime tenure under a rule of no-change insulates them from every accountability pressure; their personal authority and the authority of the ancestral settlement are a single asset, and defending one is defending the other.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, gerousia, beneficiary,
    organized, biographical, identity_locked, national).

% Full citizens formed from childhood by the common upbringing; they mess together, serve in the army, and enjoy a formal equality that is the settlement's proudest promise. They carry the corresponding burdens: fixed contributions to the common meals, lifelong liability for service, and no legitimate way to propose altering the terms — a proposal to change the founder's laws registers as impiety, not politics. Those who fall behind on contributions slide toward lesser standing while official language continues to describe them as equals. Leaving means forfeiting citizenship, kin ties, and the identity the upbringing built, all at once.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartiate_homoioi, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartiate_homoioi, beneficiary).

% Men of citizen stock who lost full standing — through failed contributions, tainted service records, or disputed birth. The settlement's vocabulary of universal equality has no category that acknowledges them, so their grievance cannot even be phrased in public terms. They remain physically inside the society while standing outside its constitutional conversation, with no route back that the official story permits.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, hypomeiones_degraded_citizens, excluded,
    powerless, biographical, trapped, national).

% The unfree farming population whose labor feeds the citizen body and makes its leisure possible. The permanence doctrine fixes their subordination as part of the unalterable order itself — no emancipation pathway is even expressible within the settlement. Periodic terror campaigns substitute for any negotiation over their condition, and their children inherit the same status as a matter of the unchangeable law.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, excluded,
    powerless, generational, trapped, regional).

% The outside analytical tradition — Thucydides and Xenophon at first hand, Aristotle systematizing, Plutarch and Polybius transmitting — examines Spartan stability from abroad. They record both the celebrated durability of the order and the observable gap between the permanence claim and actual practice, including Aristotle's notice of the magistrates' near-tyrannical discretion and the decay of the original land distribution. They testify from outside every Spartan seat.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ancient_analysts, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, ephorate_magistrates).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of constitutional stability without a written, amendable code: fixes expectations across generations, prevents factional reopening of the fundamental settlement among rival kingships and clans, reproduces a conformist citizen body through the common upbringing, and supplies a sacral warrant that makes the social order substantially self-enforcing.
% TRANSFER_FUNCTION: Moves interpretive authority and adaptive discretion upward — from the citizen body at large to the magistrates, elders, and kings — and moves the costs of surface-level rigidity (unacknowledged decline, fixed contribution burdens, status degradation without remedy) onto ordinary citizens and the unfree population, while moving legitimacy, continuity, and office security to the holders of the major magistracies.
% ABSENT_VOICES: The degraded citizens, whose condition the equality vocabulary cannot name; the helot population, wholly outside the constitutional conversation; and would-be reformers, whose proposals the framework converts into impiety rather than policy. They are absent because the permanence doctrine defines the only terms on which anyone may speak about the settlement — the fiction polices the boundaries of the discussion itself.
% DISAPPEARANCE_RATIONALE: If the permanence fiction vanished overnight, the settlement would lose its sacral warrant: open constitutional politics would erupt exactly as it did historically when the fiction finally broke under the reform kings, the magistrates' discretion would demand explicit legal grounding it never had, the elders' veto would face immediate challenge, and the citizen identity built by the common upbringing would destabilize. Every office, obligation, and status in the polis is denominated in the currency of the unchangeable law.
% FOUNDING_PROBLEM: After the Dorian settlement, Sparta needed a stable order binding two hereditary kingships, rival clans, and a citizen army into one polity resistant to tyranny and internal faction; the Lycurgan answer was a mixed constitution sealed by oracle and oath, with revision made illegitimate in advance so that no winning faction could rewrite the terms.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous corroboration from outside the benefiting parties is thin — the surviving witnesses to the founding problem (the poet Tyrtaeus, the oracle tradition) are themselves instruments of the settlement. The load-bearing corroboration is later and analytical: Aristotle's Politics corroborates the civil-strife genealogy and the subsequent decay of the land distribution from outside every Spartan seat, and Thucydides independently attests the order's unusual stability and secrecy. These sources support the founding problem's reality while dating its resolution early, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.52: the transfer is discretion and acknowledgment rather than bullion — ordinary citizens fund the order and receive its protections, but adaptive capacity is monopolized by the interpreters and the epistemic cost of the pretense falls on everyone governed by it; moderate because the coordination delivered (stability, cohesion, freedom from factional constitutional politics) is genuine and large. Suppression 0.58 is a raw structural property, unscaled by power or scope: overt revisionism was suppressed — a proposal to alter the founder's settlement registered as impiety rather than politics — while covert adaptation was licensed, so suppression aims at articulation, not at change. Theater 0.44: ritual reaffirmation of permanence (oath renewals, festival recitation, invocation of the founder) grows as enforcement substance decays, approaching but staying below half of the arrangement's activity. Accessibility collapse 0.52: open amendment collapses as an option once the doctrine is understood, but an elite interpretive channel remains, so alternatives narrow rather than vanish. Resistance 0.32: recurring friction between kings and magistrates and individual noncompliance, but no sustained movement against the settlement inside the interval. The measurement series run on one shared time grid (T=0,4,8,12,16,20,24) with every tracked metric authored at every point: the enforcement requirement falls monotonically (capacity decay), theater rises (ritual substitution for enforcement), and extraction creeps up (the widening gap between claim and practice). The trajectory is monotonic drift, not cyclical. Coordination typing: identity_coordination — the dominant function is maintaining the homoioi as an identity class (boundary maintenance, membership reproduction through the common upbringing, cowardice stigma as reputation enforcement); the conservative floor of that type is appropriate here because the identity framing is genuine but the pretense rides on it, and excess burden should surface rather than be excused as belonging's price. Coalition note: the excluded seats could not coordinate — the helot majority was unarmed, dispersed, and terrorized on schedule, while the degraded citizens remained half-inside the identity they might have organized against.
 *
 * PERSPECTIVAL GAP:
 *   From the ephorate seat the arrangement computes as the machine it personally operates — coordination it administers, discretion it collects. From the homoioi seat the same structure computes as obligation without voice: duties fixed, adaptations invisible, the vocabulary for grievance deleted by the official story. From the kings' seat it computes as a guarantee fused with the office itself — institutional identity lock, since repudiating the doctrine dissolves the sacral ground of kingship; the gerousia sits in the same fusion, its members' lifetime authority identical with the doctrine's authority. From the helot seat it computes as a sentence pronounced in perpetuity by a text no one may touch. Identity lock here is institutional and ideological at once: the common upbringing built the citizen's self out of the settlement's categories, so exit was unthinkable before it was impossible. Same epsilon referent, four different computed positions — the engine derives this divergence from the power, exit, and role data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map as follows: ephorate, gerousia, and kings sit at the beneficiary pole — the permanence fiction subsidizes them with discretion, continuity, and sanctity, and their exits (identity-locked or rotation back into the ranks) do not arbitrage away their gains. The homoioi are nominally enrolled among the beneficiaries — they do collect the machine's guarantees — but they bear the fiction's costs asymmetrically: contribution obligations, service liability, and total exclusion from the adaptation channel. Deriving their position from the beneficiary roll alone would understate their target-side exposure, hence the explicit override for the moderate power atom to d=0.45, encoding payer-primary position with residual beneficiary standing. The degraded citizens and the helot population sit at the target pole with trapped exits, which amplifies their effective burden; their exclusion from the conversation is not incidental but the enforcement object. Spatial scope is polis-scale (national), close enough that the pretense required active maintenance — oaths, festivals, supervision — rather than surviving on distance alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The immutability rhetoric is a false-summit invitation: read naively, the settlement presents as an unchangeable, ancestral-divine fixture — mountain-shaped. Declaring its beneficiaries routes any such reading through false-summit evaluation instead of letting the rhetoric certify itself. Under this reading the arrangement is tangled_rope: the coordination leg is real (expectations fixed, faction suppressed, sacral warrant economizing on coercion), and the extraction leg is real (discretion monopoly, articulation suppression, casualties the official vocabulary cannot name). Mandatrophy: the founding problem — archaic faction among rival kingships, clans, and a citizen army susceptible to tyranny — was substantially resolved within living memory of the settlement, yet the arrangement persisted because it kept solving successor problems (helot control, mass military mobilization). The parties dispute whether the live justification is continuous with the founding one, so founding_problem_status is authored contested rather than dead: this records that the original mandate is gone while avoiding a false zombie flag, since the arrangement demonstrably still performs coordination the polis depended on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the adaptive_fiction_reading of the lycurgan_laws kernel; how would the sibling readings (sacral_fidelity_reading, demographic_trap_reading) restructure the classification?',
    'Generate the sibling stories and compare victim sets, epsilon referents, and computed types: sacral_fidelity treats the doctrine as sincerely divine ordinance (naturalness pressure, false-summit evaluation if beneficiaries are declared); demographic_trap treats unrevisability as substantively binding (suppression rises, future generations enter as victims).',
    'Under sacral_fidelity the arrangement migrates toward mountain/false-summit analysis; under demographic_trap it migrates toward enforced-snare analysis with intergenerational victims; the present tangled_rope verdict holds only while the fiction-and-adaptation structure is granted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one reading of a contested constitutional kernel, not the kernel itself.').

omega_variable(
    sincere_belief_vs_known_fiction,
    'Was the immutability doctrine sincerely believed by most Spartans (suppression largely internalized and self-policing) or widely known among elites to be a maintained pretense (suppression actively enforced against articulation)?',
    'Source analysis distinguishing doctrinal assertion from behavioral accommodation: if practice routinely diverged from the letter without scandal, the pretense was open knowledge among officeholders and enforcement targeted only public articulation of change.',
    'If sincerely believed, the measured suppression is largely carried inside the agents and the arrangement''s coercive overhead is lower than authored; if known pretense, suppression is structural and concentrated on speech, raising the share of the burden attributable to the lie itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_belief_vs_known_fiction, empirical, 'Suppression mechanism ambiguity: internalized belief versus enforced pretense.').

omega_variable(
    enforcement_failure_vs_tolerated_drift,
    'Did the archaic enforcement machinery (upbringing supervision, common-meal audits, allotment protection) fail by incapacity, or did officeholders deliberately tolerate drift that benefited them?',
    'Correlate the timing of land-concentration evidence and citizen-roll shrinkage with recorded magisterial interventions; deliberate tolerance predicts intervention patterns that punish individual default while sparing the consolidating causes.',
    'Deliberate tolerance raises effective extraction (selective non-enforcement as rent) and pushes seat computations toward snare; incapacity decay supports the tangled_rope-with-decaying-enforcement profile authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_failure_vs_tolerated_drift, empirical, 'Attribution of demographic decline: enforcement failure versus strategic tolerance.').

omega_variable(
    apella_participation_breadth,
    'Did the citizen assembly retain meaningful input into the interpretive adaptations, or was adaptation confined to the magistrates, elders, and kings?',
    'Reconstruct assembly procedure from the constitutional fragments: whether the assembly could originate measures or only ratify council proposals, and whether interpretive rulings ever required its assent.',
    'Meaningful assembly input would narrow the discretion asymmetry and pull the classification toward rope; confinement to the magistracies confirms the extraction leg of the tangled_rope verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apella_participation_breadth, conceptual, 'Breadth of participation in the covert adaptation channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lycu_tr_t4, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(lycu_tr_t8, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(lycu_tr_t12, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(lycu_tr_t16, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(lycu_tr_t24, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(lycu_be_t4, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(lycu_be_t8, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lycu_be_t12, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(lycu_be_t16, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(lycu_be_t24, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement(lycu_su_t4, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 4, 0.71).
narrative_ontology:measurement(lycu_su_t8, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(lycu_su_t12, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(lycu_su_t16, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(lycu_su_t24, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the immutable Lycurgan laws' decomposes into three structurally distinct constraints sharing one kernel. This member (adaptive_fiction) authors epsilon near 0.52 for the practiced regime under the fiction hypothesis — moderate burden riding a large genuine coordination function. The sacral_fidelity sibling authors the doctrine as sincerely binding (mountain-pressure referent; false-summit evaluation if beneficiaries are declared). The demographic_trap sibling authors substantive unrevisability with intergenerational victims (higher suppression, snare-pressure referent). Ordering: sacral_fidelity is the doctrine's self-description and supplies the legitimacy premise the other two readings parasitize; adaptive_fiction and demographic_trap are rival causal accounts of the same observed decline. Each story links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
