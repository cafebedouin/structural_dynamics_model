% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Gentlemanly Honor-Satisfaction Mechanism — Decline Reading (Persistence at Falling Frequency to Fringe)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   A single arrangement — the gentlemanly obligation to answer affronts with
 *   regulated combat — persisted across the interval at steadily falling
 *   frequency, ending the period as a fringe practice kept alive by ceremony,
 *   corps custom, and memory while remaining fully conceptually available:
 *   everyone still knew what a challenge was, and occasional real ones
 *   occurred. This file instantiates the decline_reading of the
 *   honor_satisfaction_mechanism kernel and authors ONE continuous epsilon
 *   over ONE persisting arrangement: the mechanism weakened because answering
 *   a challenge got more dangerous (prosecution, courts-martial) and
 *   declining got cheaper (bourgeois condemnation flipped the sanction
 *   balance), not because the category lapsed. Family note:
 *   contraction_reading authors the terminal cognitive condition (epsilon
 *   driven toward zero by unthinkability); composite_reading decomposes the
 *   causation into separately measurable mechanisms (state violence monopoly,
 *   bourgeois norms, insurance markets, category shift). Each sibling is a
 *   separate file with its own epsilon, beneficiaries, and classification;
 *   the edges in network.affects_constraints express explanatory dependency,
 *   not shared identity. Claim/metric independence is preserved: the
 *   tangled_rope claim and the declining metric series are authored
 *   independently. KEY AGENTS (by structural relationship): -
 *   gentleman_class_collectivity: primary beneficiary (organized /
 *   identity_locked) - honor_code_administrators: agenda setter + secondary
 *   beneficiary (institutional / mobile) -
 *   compelled_challengers_and_acceptors: primary target (moderate / trapped)
 *   - posted_refusers: secondary target (moderate / constrained) -
 *   national_justice_establishments: external antagonist bearing containment
 *   costs (institutional / mobile) - evangelical_and_utilitarian_reformers:
 *   excluded opposition (organized / constrained) - status_capital_duelists:
 *   secondary beneficiary (moderate / mobile) - mensur_corps_students:
 *   terminal-fringe beneficiary (moderate / mobile) - analytical observer:
 *   historiographic/engine seat — sees the full structure across the interval
 *
 * KEY AGENTS:
 *   - gentleman_class_collectivity: primary beneficiary (organized / identity_locked) — collects internal peace, class boundary, and credible-commitment assurance; locked in by what being a gentleman means
 *   - honor_code_administrators: agenda setter and secondary beneficiary (institutional / mobile) — runs courts of honor, posting, and meeting protocol; collects deference
 *   - compelled_challengers_and_acceptors: primary target (moderate / trapped) — bears lethal risk under compulsion of ruinous refusal
 *   - posted_refusers: secondary target (moderate / constrained) — bears social death for declining; later forms pledge coalitions
 *   - national_justice_establishments: external antagonist bearing containment costs (institutional / mobile)
 *   - evangelical_and_utilitarian_reformers: excluded opposition (organized / constrained) — supplies the rising social cost of participation
 *   - status_capital_duelists: secondary beneficiary (moderate / mobile) — converts prowess into standing
 *   - mensur_corps_students: terminal-fringe beneficiary (moderate / mobile) — voluntary scar-credential seekers
 *   - analytical observer: historiographic/engine seat — sees the full structure across the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.25).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.2).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Gentlemanly Honor-Satisfaction Mechanism — Decline Reading (Persistence at Falling Frequency to Fringe)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '3be085c2-6e80-4f82-900f-d0129f76a798').
narrative_ontology:cs_kernel_codification('3be085c2-6e80-4f82-900f-d0129f76a798', formalized).
narrative_ontology:cs_authority_grounding('3be085c2-6e80-4f82-900f-d0129f76a798', practice).
narrative_ontology:cs_interpretation_layer_present('3be085c2-6e80-4f82-900f-d0129f76a798').
narrative_ontology:cs_reading_relation('3be085c2-6e80-4f82-900f-d0129f76a798', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3be085c2-6e80-4f82-900f-d0129f76a798', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('3be085c2-6e80-4f82-900f-d0129f76a798', foundational, continuity_of_conceptual_availability).
narrative_ontology:cs_axiom_status(continuity_of_conceptual_availability, holdable).
narrative_ontology:cs_axiom_grounding('3be085c2-6e80-4f82-900f-d0129f76a798', continuity_of_conceptual_availability, empirically_contingent).
narrative_ontology:cs_axiom('3be085c2-6e80-4f82-900f-d0129f76a798', foundational, price_not_category_explains_decline).
narrative_ontology:cs_axiom_status(price_not_category_explains_decline, holdable).
narrative_ontology:cs_axiom_grounding('3be085c2-6e80-4f82-900f-d0129f76a798', price_not_category_explains_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('3be085c2-6e80-4f82-900f-d0129f76a798', operative_honour_regime).
narrative_ontology:cs_drift_state('3be085c2-6e80-4f82-900f-d0129f76a798', fin_de_siecle_observation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3be085c2-6e80-4f82-900f-d0129f76a798', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, gentleman_class_collectivity).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, honor_code_administrators).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, status_capital_duelists).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, mensur_corps_students).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, compelled_challengers_and_acceptors).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, posted_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, national_justice_establishments).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, point_of_honor_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, gentleman_credible_commitment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European gentry and officer class as a body. Its members extend credit, command regiments, marry, and legislate among themselves, and for most of the period a man's willingness to answer for his word under fire is treated as the guarantee behind all of it. The class as a whole gains internal peace (quarrels end at the field of honor rather than in feuds), a sharp boundary against those beneath it, and assurance that allies and subordinates will not flinch. Individual members who wish to opt out of the code find that opting out costs them the standing that makes them gentlemen at all; the class reproduces the code generationally through education, regimental custom, and example.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, gentleman_class_collectivity, beneficiary,
    organized, generational, identity_locked, continental).

% Senior officers, regimental messes, club committees, and experienced seconds who keep the machinery running: they receive challenges, vet causes, negotiate apologies, arrange ground and weapons, certify satisfaction, and post men who refuse without cause. Administration confers deference and gatekeeping power, and administrators can decline further service at little personal cost, though doing so forfeits the standing that comes with the office.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_code_administrators, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, honor_code_administrators, beneficiary).

% Renowned shots and swordsmen whose demonstrated skill deters challenges and attracts admiration. They volunteer readily, win more often than chance allows, and convert the practice's dangers into reputation, promotion, and marriage-market advantage. Their fortunes rise and fall with the practice's frequency.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, status_capital_duelists, beneficiary,
    moderate, biographical, mobile, national).

% Members of German university fraternities in the period's closing decades, who fence protected bouts under corps rules seeking facial scars as visible credentials. Participation is formally voluntary and framed as camaraderie and proof of nerve rather than as answer to an insult; peer expectation inside the corps makes abstention socially expensive for a member, though resigning the corps ends the pressure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, mensur_corps_students, beneficiary,
    moderate, biographical, mobile, national).

% Officers and gentlemen who would rather not fight but see no third path: an unanswered insult ends a career, while a refused challenge ends it faster. They bear the risk of death or maiming, prosecution where the law has turned, and the grief their deaths visit on families. Younger sons and men without private means feel the compulsion most sharply, since their commissions and prospects hang on regimental opinion.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, compelled_challengers_and_acceptors, payer,
    moderate, biographical, trapped, national).

% Men who decline to fight — on principle, conscience, or fear — and are publicly marked for it. Posting costs them invitations, promotions, and marriage prospects; some transfer regiments, emigrate, or endure social half-exile. Toward the period's end, refusers begin finding each other, signing mutual-protection pledges, and discovering that numbers loosen the penalty.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, posted_refusers, payer,
    moderate, biographical, constrained, national).

% Parliaments, courts, judges, and army hierarchies acting against the practice: statutes declaring it felony, courts-martial punishing it in the ranks, prosecutions that frequently fail against perjury and class solidarity, and repeated official inquiries. Containment consumes real money and attention, results vary widely by jurisdiction, and enforcement is a policy choice these bodies can intensify or relax.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, national_justice_establishments, payer,
    institutional, generational, mobile, national).

% Religious societies, utilitarian writers, editors, and later mass-circulation newspapers that condemn the practice as sin and waste. Initially dismissed as fanatics outside polite conversation, they accumulate sermons, pamphlets, statistics of the slain, and eventually electoral weight. Their condemnation raises the price of participating for anyone who cares what the wider public thinks.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, evangelical_and_utilitarian_reformers, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, gentleman_class_collectivity).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converted potentially endless private vengeance between armed equals into a rule-governed, terminating encounter with agreed cause, weapons, and satisfaction conditions; simultaneously certified membership and trustworthiness within the gentleman class, giving officers, creditors, and allies a visible guarantee behind a man's word.
% TRANSFER_FUNCTION: Moved the risk of death or maiming from the class in general onto individual disputants and their families; moved standing and promotion from refusers to fighters; moved adjudication authority to seconds, code-keepers, and senior officers; and, as the practice declined, moved ever more of the enforcement burden from private opinion machinery onto parliaments, courts, and courts-martial.
% ABSENT_VOICES: Refusers had no seat in the courts of honor and regimental opinion that judged them; widows and maimed survivors could not contest a code's verdicts; evangelical and utilitarian critics were excluded from polite deliberation as fanatics until their numbers forced admission — the unanimity that sustained the code was partly an artifact of who was allowed in the room.
% DISAPPEARANCE_RATIONALE: Through most of the interval the class's honor economy ran on the mechanism: commissions, invitations, credit, and marriages referenced a man's standing under the code, and its sudden removal would have forced immediate construction of substitutes — written defamation procedures, formal courts of honor, explicit military loyalty tests — while exposing a generation of men trained to no other settlement. By the interval's end the dependence had thinned to ceremony and corps custom, and removal would have rearranged little.
% FOUNDING_PROBLEM: Unresolved insults among armed gentlemen could not be taken to law — the point of honor sat above the legal system as a class prerogative — yet leaving them unanswered invited ambush, vendetta, and factional bloodshed; separately, armies and alliances needed demonstrable guarantees that an officer would stand behind his word under fire.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: parliamentary debate records (British Commons inquiries into dueling, 1808–1844), army regulations' own stated purposes, and anti-dueling society pamphlets all name the same founding problems; their quarrel is over whether courts, libel law, and professional military discipline had genuinely absorbed those functions by the interval's end. No source outside the practice's defenders attests that the original private-adjudication problem remained unsolved in its original form after 1900.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state scores describe the mechanism at 1920. Extractiveness 0.25: participation is nearly voluntary, frequency is fringe, and residual compulsion survives mainly as corps peer pressure (abstention inside a corps is socially expensive; resignation ends it). Suppression 0.20: refusing a challenge no longer destroys a career in any major jurisdiction's daily practice. Theater_ratio 0.60: surviving activity is predominantly symbolic — scar-seeking bouts, commemorative challenges, antiquarian code-keeping — relative to live insult-adjudication, which is this reading's signature: function atrophied faster than form. Accessibility_collapse 0.30: alternatives (apology through seconds, legal redress, studied silence) were always available and became cheap as surrounding norms turned. Resistance 0.65: the dominant surrounding normative order condemns the practice, refusers formed pledge coalitions, and states prosecuted. Trajectories run on one shared grid: base_extractiveness falls (0.72 to 0.25) as enforcement risk and social cost rise — the reading's causal claim; theater_ratio rises (0.12 to 0.60) as function atrophies; suppression_requirement falls (0.78 to 0.20) as the opinion machinery loses grip. Boltzmann coordination_type is enforcement_mechanism: the machinery (published codes, seconds, courts of honor) is dedicated enforcement infrastructure whose failure returns the founding problem of vendetta. Claim/metric independence: claimed_type tangled_rope is asserted on structural grounds — coordination (vendetta-suppression, credible commitment), asymmetric burden (compelled risk, posted refusers), and active enforcement (courts of honor, posting) all remain present in attenuated form through the interval's end; the engine may compute terminal seats differently, and that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the administrator seat the arrangement is a vocation and a civilization: administering satisfaction is honorable work, and the practice's fade reads as declining standards. From the compelled seat the same arrangement is a trap that loosened: before mid-century refusal meant ruin and acceptance meant risk; afterward both prices fell, and the seat experiences release. From the justice-establishment seat the practice is a costly nuisance approaching victory — its borne costs are real but adversarial, chosen policy rather than transfer to the code's holders, so its computed burden reflects imposition-by-persistence, not rent flowing to beneficiaries. From the reformer seat the trajectory is vindication arriving slower than hoped. From the corps-student seat the residue is harmless camaraderie with a credential attached. Same structure, incompatible experiences; the engine derives the divergence from power, exit, and role data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the class collectivity, administrators, status duelists, and corps students near the subsidized pole: the mechanism manufactures assurance, boundary, deference, and credential for them. Victim declarations place compelled challengers and posted refusers near the target pole; the compelled seat's trapped exit amplifies its exposure early in the interval, and its effective burden tracks the falling compulsion curve. The justice establishment carries payer role for borne containment costs but sits adversarially — its exit is mobile and its payments discretionary — so its derived position should read as cost-bearing opponent rather than harvested source. Corps students sit nearest symmetric: a voluntary exchange of bodily risk for visible credential, with peer pressure supplying the only residual compulsion. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings threaten this story. Calling the whole arc pure predation erases the coordination the mechanism genuinely delivered — quarrels terminated at the field instead of in vendettas, and the class's credible-commitment economy ran on it; the tangled_rope claim keeps that term on the books. Calling the terminal remnant still-functional coordination ignores that its persistence ran on inertia, nostalgia, and corps peer pressure long after insult-adjudication had migrated to courts and libel law; the rising theater_ratio series lets the engine date the drift toward inertial character without the author asserting a mid-story type flip. The R5 interview answers 'contested': the founding adjudication problem is substantially absorbed by state institutions, while credible-commitment signaling persists in transformed guises, so the mandatrophy boolean is deliberately left unset rather than forced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the honor_satisfaction_mechanism kernel does this story''s epsilon measure — continuous attenuation to fringe (this file), terminal cognitive unthinkability (contraction_reading), or plural mechanism replacement (composite_reading)?',
    'Cross-file comparison of the three sibling stories'' terminal states and causal attributions, adjudicated against surviving practice records, code publications, and challenge correspondence through the interval''s end.',
    'Adopting contraction_reading would drive terminal epsilon toward zero and recast the remnant as inert vestige; adopting composite_reading would decompose this story into linked stories with separate epsilon curves per mechanism; this reading''s single continuous curve is falsified if the practice''s conceptual availability demonstrably lapsed before the interval''s end.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    residual_frequency_floor,
    'Is the terminal fringe practice the same mechanism attenuated (challenges still answered as honor adjudication) or already a category-shifted successor (voluntary scar-seeking, sport)?',
    'Motive analysis of surviving practice records: challenge correspondence citing specific offenses and demanding satisfaction marks continuity; voluntary bout scheduling without antecedent insult marks category shift.',
    'If category-shifted, the decline curve should terminate earlier and the terminal-state epsilon attributed to this constraint falls further, with the remnant reassigned to a successor arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_frequency_floor, empirical, 'Continuity versus category shift in the terminal fringe practice.').

omega_variable(
    enforcement_cost_attribution,
    'How much of the measured epsilon decline is attributable to state enforcement risk versus autonomously rising social cost?',
    'Jurisdictional comparison at matched dates between regimes with vigorous prosecution (post-1844 British army articles, Prussian courts-martial) and lax ones, holding class composition constant.',
    'An enforcement-dominated result makes the decline contingent on state policy and counterfactual persistence plausible; a social-cost-dominated result makes it self-sustaining norm change; the split recalibrates which of the composite_reading''s mechanisms actually pulled the load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_attribution, empirical, 'Attribution of the decline between prosecution and moral-economy shift.').

omega_variable(
    class_identity_lock_valence,
    'Did the gentleman class''s identity lock bind members into compliance (amplifying the mechanism''s hold) or merely mark the boundary — and did professionalizing career paths outside the gentry break the lock independently of enforcement?',
    'Career-path analysis across the interval: growth of non-gentry routes into commissions and professions predicts lock erosion preceding behavioral decline.',
    'If the lock broke endogenously, the fall in suppression_requirement reflects lock failure rather than enforcement defeat, and the mechanism''s persistence after lock-break is inertia rather than compulsion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_identity_lock_valence, conceptual, 'Dual valence of the class identity lock during the decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1770, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_decline_tr_t1770, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1770, 0.12).
narrative_ontology:measurement_basis(honor_decline_tr_t1770, observed).
narrative_ontology:measurement(honor_decline_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement_basis(honor_decline_tr_t1800, observed).
narrative_ontology:measurement(honor_decline_tr_t1830, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1830, 0.24).
narrative_ontology:measurement_basis(honor_decline_tr_t1830, observed).
narrative_ontology:measurement(honor_decline_tr_t1860, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1860, 0.38).
narrative_ontology:measurement_basis(honor_decline_tr_t1860, observed).
narrative_ontology:measurement(honor_decline_tr_t1890, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1890, 0.52).
narrative_ontology:measurement_basis(honor_decline_tr_t1890, observed).
narrative_ontology:measurement(honor_decline_tr_t1920, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1920, 0.6).
narrative_ontology:measurement_basis(honor_decline_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(honor_decline_be_t1770, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1770, 0.72).
narrative_ontology:measurement_basis(honor_decline_be_t1770, observed).
narrative_ontology:measurement(honor_decline_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement_basis(honor_decline_be_t1800, observed).
narrative_ontology:measurement(honor_decline_be_t1830, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1830, 0.55).
narrative_ontology:measurement_basis(honor_decline_be_t1830, observed).
narrative_ontology:measurement(honor_decline_be_t1860, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1860, 0.42).
narrative_ontology:measurement_basis(honor_decline_be_t1860, observed).
narrative_ontology:measurement(honor_decline_be_t1890, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1890, 0.31).
narrative_ontology:measurement_basis(honor_decline_be_t1890, observed).
narrative_ontology:measurement(honor_decline_be_t1920, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1920, 0.25).
narrative_ontology:measurement_basis(honor_decline_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(honor_decline_su_t1770, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1770, 0.78).
narrative_ontology:measurement_basis(honor_decline_su_t1770, observed).
narrative_ontology:measurement(honor_decline_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.74).
narrative_ontology:measurement_basis(honor_decline_su_t1800, observed).
narrative_ontology:measurement(honor_decline_su_t1830, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1830, 0.62).
narrative_ontology:measurement_basis(honor_decline_su_t1830, observed).
narrative_ontology:measurement(honor_decline_su_t1860, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1860, 0.48).
narrative_ontology:measurement_basis(honor_decline_su_t1860, observed).
narrative_ontology:measurement(honor_decline_su_t1890, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1890, 0.33).
narrative_ontology:measurement_basis(honor_decline_su_t1890, observed).
narrative_ontology:measurement(honor_decline_su_t1920, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1920, 0.2).
narrative_ontology:measurement_basis(honor_decline_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the decline of dueling' covers structurally distinct claims. This story authors one continuous epsilon over one persisting arrangement (decline_reading); contraction_reading authors the terminal cognitive condition; composite_reading decomposes causation into separately measurable mechanisms. Each carries its own epsilon, beneficiaries, and classification; the edges here express explanatory dependency, not shared identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
