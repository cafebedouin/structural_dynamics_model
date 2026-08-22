% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor-Satisfaction Substrate — Cultural Contraction Reading (Honor-to-Dignity Transformation)
 *   domain: historical sociology/cultural anthropology/legal history
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the cultural_contraction_reading —
 *   of the kernel honor_satisfaction_substrate: the normative economy in
 *   which male honor among armed social equals was staked, insulted, and
 *   satisfied through ritualized combat. Per the epsilon-invariance
 *   principle, the referent of epsilon here is the standing arrangement
 *   itself — the honor code governing gentlemanly dispute settlement from its
 *   consolidation (T0, circa 1650) to the completion of the honor-to-dignity
 *   transformation in most of the Atlantic world (T250, circa 1900) —
 *   assessed by this reading's own lights. On those lights the code was a
 *   hybrid: it genuinely coordinated (converting open-ended feud into
 *   rule-governed single combat with negotiated exits, and minting a common
 *   currency of reputation) while it extracted heavily (lives, maiming,
 *   coerced participation), and it was held up by active social enforcement
 *   machinery (seconds, courts of honor, social-death sanctions). The
 *   reading's distinctive claim concerns the dissolution mechanism: the
 *   code's force was interpretive-substrate-deep, and dueling ended because
 *   the substrate transformed — cultures of honor gave way to cultures of
 *   dignity, and dueling became unthinkable rather than merely forbidden. The
 *   sibling readings (practice_decline_reading: substrate persists, exogenous
 *   enforcement explains decline; composite_overdetermined_reading: joint
 *   non-independent causation) are separate constraints in separate files,
 *   linked via network.affects_constraints; their contest is routed to omega
 *   variables, not described inside this constraint. Assumption stated
 *   explicitly: the reading's 'mountain erosion' language is modeled as
 *   substrate collapse (axiom_overriding drift, severe, unacknowledged)
 *   rather than as a mountain claim about the arrangement's type — the mature
 *   code had identifiable beneficiaries, identifiable victims, and
 *   enforcement machinery, which is why the claimed type is tangled_rope.
 *
 * KEY AGENTS:
 *   - gentleman_class: Primary beneficiary (organized/identity_locked) — collects the status order and class boundary the code produces; individual members pay inside it and cannot exit without ceasing to be gentlemen
 *   - military_officer_corps: Secondary beneficiary (institutional/identity_locked) — institutionalizes the code in commissions and mess standing; buries a steady share of its own
 *   - duel_casualties: Primary victim (powerless/trapped) — bear the mortal accounting; no exit once the pistols are loaded, no legal redress after
 *   - honor_coerced_gentlemen: Victim with incidental gains (moderate/identity_locked) — privately dread the affair, publicly cannot refuse; refusal is priced above death
 *   - courts_of_honor_and_seconds: Agenda setter (institutional/constrained) — administer the code, negotiate satisfactions, collect authority from the machinery
 *   - anti_dueling_reformers: Excluded voice (organized/mobile) — clergy, evangelicals, utilitarians, coroners; object from outside a conversation that admits no outsiders
 *   - commercial_professional_classes: Excluded voice turned substrate successor (organized/mobile) — build the rival dignity-culture status economy whose existence dissolves the code's premises
 *   - sovereign_state_and_courts: Observer (institutional/analytical) — legislates prohibition, prosecutes sporadically and class-skewed; its long non-enforcement is datum any account of the end must explain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.34).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.26).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor-Satisfaction Substrate — Cultural Contraction Reading (Honor-to-Dignity Transformation)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical sociology/cultural anthropology/legal history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '35c79fb2-13b9-4d45-97b1-6f537809a6ed').
narrative_ontology:cs_kernel_codification('35c79fb2-13b9-4d45-97b1-6f537809a6ed', distributed).
narrative_ontology:cs_authority_grounding('35c79fb2-13b9-4d45-97b1-6f537809a6ed', practice).
narrative_ontology:cs_interpretation_layer_present('35c79fb2-13b9-4d45-97b1-6f537809a6ed').
narrative_ontology:cs_reading_relation('35c79fb2-13b9-4d45-97b1-6f537809a6ed', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('35c79fb2-13b9-4d45-97b1-6f537809a6ed', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('35c79fb2-13b9-4d45-97b1-6f537809a6ed', foundational, honor_force_is_interpretive_substrate_deep).
narrative_ontology:cs_axiom_status(honor_force_is_interpretive_substrate_deep, holdable).
narrative_ontology:cs_axiom_grounding('35c79fb2-13b9-4d45-97b1-6f537809a6ed', honor_force_is_interpretive_substrate_deep, empirically_contingent).
narrative_ontology:cs_axiom('35c79fb2-13b9-4d45-97b1-6f537809a6ed', foundational, personal_worth_is_innate_not_staked).
narrative_ontology:cs_axiom_status(personal_worth_is_innate_not_staked, holdable).
narrative_ontology:cs_axiom_grounding('35c79fb2-13b9-4d45-97b1-6f537809a6ed', personal_worth_is_innate_not_staked, deontological).
narrative_ontology:cs_reference_frame('35c79fb2-13b9-4d45-97b1-6f537809a6ed', organic_honor_consensus).
narrative_ontology:cs_drift_state('35c79fb2-13b9-4d45-97b1-6f537809a6ed', dignity_culture_transition, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('35c79fb2-13b9-4d45-97b1-6f537809a6ed', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, duel_casualties).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, honor_coerced_gentlemen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, honor_coerced_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, extralegal_peer_arbitration_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the tone of polite society and keeps the honor ledger: its members issue and answer challenges, and the class as a whole collects the product — a ranked order of standing, a boundary against tradesmen and the unlettered, and a dispute forum answerable to no outsider. Individual members pay heavily inside that economy: sons die, reputations shatter, and no member can decline an affair without forfeiting the standing the code defines. Walking away means emigrating from the class's good opinion — a few manage it (clergy, Quakers, professed dissenters), for most it is social death.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class, payer).

% Institutionalizes the code: commissions, promotions, and mess standing ride on reputation for courage, and regiments treat an unanswered insult as career-ending. Officers gain a protected status order and a ready private channel for quarrels the chain of command handles badly; they also bury a steady share of their own — in some armies peacetime dueling deaths are a recognized attrition line. Exit runs through resignation or courts-martial, both ruinous to a career built on the same reputation the code polices.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps, beneficiary,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps, payer).

% Carry the code's mortal accounting: killed or maimed in affairs many entered to escape worse shame, their families left without breadwinners and without legal redress, since reluctant juries return verdicts of chance-medley or manslaughter for gentlemen. Once the pistols are loaded there is no exit at all; beforehand, the only alternatives were the ones the code refused to count as alternatives.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, duel_casualties, payer,
    powerless, immediate, trapped, national).

% Men who privately dread the affair and publicly cannot refuse: the challenged man who thinks the charge frivolous, the young heir pressed by his father's name, the professional whose clients are watching. Refusal costs credit, office, and marriage prospects — the code prices cowardice above death. Surviving an affair buys standing, which is exactly what keeps the price credible for the next man.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, honor_coerced_gentlemen, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, honor_coerced_gentlemen, beneficiary).

% Run the code day to day: seconds negotiate apologies, distances, and exchange protocols; regimental and club tribunals rule on points of provocation and precedence; code-duello manuals supply procedure. They collect authority and prestige from the machinery — the colonel who presides over affairs matters more than the one who merely fights them — and they can reshape practice at the margin, arranging bloodless satisfactions, without being able to redefine what honor requires.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, courts_of_honor_and_seconds, agenda_setter,
    institutional, generational, constrained, continental).

% Clergy, evangelical societies, utilitarian pamphleteers, and coroners who count the bodies. They preach, publish, petition, and prosecute where they can, and they are not in the room: the code's meaning is set in messes, dressing rooms, and seconds' negotiations that admit no outsider. Their leverage grows only as their audience stops believing the code's premises — which is to say, as the substrate shifts beneath the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, anti_dueling_reformers, excluded,
    organized, generational, mobile, national).

% Merchants, lawyers, manufacturers, and professionals building a rival status economy — creditworthiness, licensure, print reputation — in which a man's worth is not staked in risked blood. They stand outside the honor conversation by design, and their indifference corrodes it: every quarrel settled in a newspaper column or a courtroom demonstrates that satisfaction can be had elsewhere. Across the interval they grow from fringe to majority culture.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, commercial_professional_classes, excluded,
    organized, generational, mobile, national).

% Legislatures pass anti-dueling statutes and courts occasionally try survivors, but enforcement is sporadic and class-skewed — officers duel with impunity while poor men hang for the same act. The state watches the practice it formally forbids, moving mainly when scandal forces it, and its long non-enforcement is part of the historical record that any account of the practice's end has to explain.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, sovereign_state_and_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converted open-ended aristocratic vengeance — feud, ambush, assassination between armed peers — into rule-governed single combat with negotiated exits: seconds could arrange satisfaction short of fire, apology could close an affair, and the code defined what counted as an insult requiring response. It also minted a shared currency of reputation that made deference and ranking predictable across a dispersed elite.
% TRANSFER_FUNCTION: Moved bodily risk and death from the class's quarrels into the honor ledger — each affair transferred certain standing to the survivor and mortality risk to both principals — and moved dispute-resolution authority from royal courts and church (venues gentlemen scorned as fit for inferiors) to the peer honor community itself.
% ABSENT_VOICES: Duel casualties (dead men cannot testify), coerced refusers facing social death, widows and orphans without redress, clergy and evangelical societies locked out of honor forums, and the unenfranchised classes the code ranked beneath consideration. They stood outside the seconds' negotiations and tribunals where the code's meaning was fixed; their objection enters the record only through reform literature, coroner inquests, and criminal trials.
% DISAPPEARANCE_RATIONALE: Gentlemanly status competition, officer promotion politics, and dispute settlement between social equals all ran through the satisfaction economy. When it vanished, political quarrels migrated to partisan press and elections, officer conflicts to courts-martial and administrative channels, and insult grievances to defamation law — the arrangements rebuilt themselves around dignity-culture premises: worth taken as innate, remedy sought in courts and opinion markets rather than risked blood.
% FOUNDING_PROBLEM: Early-modern armed elites could not carry grievances between equals to royal law without dishonor: courts were for inferiors, private vengeance was a right, and unchecked feud destabilized a peace the crown was too weak to impose on armed nobles. The satisfaction code answered by regulating the vengeance — bounding it in ritual, staffing it with seconds, and building in exits short of death.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: legal historians document the displacement of honor arbitration by state courts and defamation actions; anthropologists of honor cultures corroborate that the 'unsatisfiable insult between armed equals' problem-category disappears where state jurisdiction and equal legal standing obtain; nineteenth-century reform testimony and inquest records attest the founding problem was already moribund while the practice lingered on. No modern institution attests a live version of the founding problem.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).
:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All three tracked series run on one shared time grid (T0, 50, 100, 150, 200, 250; years since circa 1650), so every metric is authored at every examined point. Base extractiveness rises to a mid-interval peak (0.66 at T150, the classical dueling era) and then falls to 0.34 as the governed population stops believing — practice follows meaning with a lag. Suppression_requirement FALLS across the second half (0.60 to 0.26): this is the reading's fingerprint. An enforcement-victory story requires a rising or sustained suppressive force defeating the practice; the record shows the opposite — the code needed less and less force because fewer and fewer needed forcing. Theater_ratio rises monotonically (0.12 to 0.72): affairs increasingly resolve without shots (deloping, negotiated satisfactions, ritualized student duels), form outliving meaning. End-state scalars match the terminal measurements: extractiveness 0.34, suppression 0.26, theater 0.72. Accessibility_collapse (0.3) and resistance (0.45) are authored as end-state descriptions: by T250 the alternatives the code once refused to count — courts, defamation actions, print reputation, dignified apology — are fully open, and what resistance remains comes from enclave defenders of the custom rather than the near-unanimous belief-community of the mature era (at maturity, accessibility_collapse approached 0.85 for insiders and resistance among the governed was minimal: belief, not fear, held it). Claim/metric independence: claimed_type tangled_rope is stated from the mature arrangement's structure — genuine coordination function (feud containment, dispute settlement between equals), asymmetric extraction (deaths, coerced participation), active social enforcement (seconds, courts of honor, social-death sanctions). The end-state metric profile computes piton-flavored; that divergence is the lifecycle finding this reading exists to document, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From inside the gentleman_class seat the code is the shape of the world — near-zero felt burden, exit unimaginable because leaving means ceasing to be what one is. From the duel_casualties and honor_coerced_gentlemen seats the same structure is pure cost with locked exits. Courts_of_honor_and_seconds experience administration and collected authority. The excluded seats see the whole structure from outside and name it what the insiders cannot. Same-level lateral divergence is sharp: two gentlemen of equal rank and standing face different choice sets depending on their position in a given affair — challenger, challenged, second, or bystander whose opinion is the enforcement — and officer corps embedding (career-riding honor) differentiates officers from civilian gentry at nominally identical rank. The state's seat sees corpses and unenforced statutes; the mess sees honor. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for gentleman_class and military_officer_corps: the code subsidizes them with a ranked status order, a class boundary, and a private dispute forum, and their identity_locked exit places them deep in the subsidized end despite individual members paying with their lives — the class-level rent is real even where member-level cost is fatal. Victim declarations drive high directionality for duel_casualties (trapped: zero exit) and honor_coerced_gentlemen (identity_locked: refusal priced above death), and trapping amplifies their effective burden. Courts_of_honor_and_seconds sit near symmetric: they administer, collect prestige, and absorb the machinery's costs. The excluded seats (anti_dueling_reformers, commercial_professional_classes) sit outside the pull — mobile exit, no stake in the code's continuance — and their growing weight over the interval is the substrate transformation itself. Scope is continental for the transnational honor culture, national for its victims and successors; larger scope made verification of satisfaction harder and the machinery more necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — containing vengeance between armed equals whom royal law could not reach without dishonor — died well before the practice did, and the temporal series shows the classic signature: theater rising while suppression falls means the arrangement outlived its function and was carried by habit and performance in shrinking enclaves while its real work had migrated to state courts, defamation law, print reputation, and electoral politics. The (founding_problem_status=dead x disappearance_verdict=world_rearranges) mismatch flag will fire, and here it is correct signal: the residue was zombie-form. The classification prevents two opposite errors. Against the pure-suppression reading: suppression_requirement fell, so the end cannot be modeled as enforcement defeating practice. Against the pure-predation reading: the mature code solved a real coordination problem (feud containment among armed peers) that made it sticky for two centuries — calling it a snare misses why gentlemen defended it at the cost of their lives. Mandatrophy resolution here is historical, not administrative: no sunset clause was ever declared; the mandate expired when the world that posed the problem expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_position_kernel_contest,
    'Is the dissolution of the dueling obligation attributable to collapse of the honor code as interpretive substrate (this reading), to exogenous enforcement operating on a persisting substrate (practice_decline_reading), or to overdetermined joint causation (composite_overdetermined_reading)?',
    'Cross-jurisdictional comparison where legal prohibition was constant but honor-substrate indicators (officer-corps embedding, gentry density, print honor culture) varied: if survival tracks substrate indicators rather than enforcement intensity, this reading is load-bearing.',
    'Resolves which sibling reading carries the causal weight; a substrate-driven survival pattern confirms this file''s characterization of the end as meaning-collapse rather than defeat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_position_kernel_contest, empirical, 'Committer-frame position: this constraint is one reading of kernel honor_satisfaction_substrate; sibling readings relocate the dissolution mechanism.').

omega_variable(
    suppression_marginal_effect,
    'What was the marginal effect of anti-dueling statute and prosecution on practice survival, net of the simultaneous transformation of the honor value system?',
    'Natural experiments with sharp enforcement variation against matched substrates: antebellum US South versus North, Restoration France versus revolutionary interludes, British Army versus Royal Navy prosecution rates.',
    'If dueling survived maximal prohibition wherever the substrate stayed intact, suppression was not load-bearing and the mountain-erosion characterization of the end stands; if enforcement gaps predict survival, the practice_decline_reading regains weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_marginal_effect, empirical, 'Disentangling enforcement effect from substrate transformation in the decline record.').

omega_variable(
    unthinkability_vs_disapproval,
    'Did dueling exit the thinkable action-set (premises rejected) or merely become dangerous and disapproved (costs inflated within unchanged premises)? Behaviorally identical, structurally different.',
    'Close reading of refusal discourse across the interval: do refusers justify refusal in code-internal terms (cowardice priced against death, satisfaction deferred) or reject the premise that blood-satisfaction answers insult at all?',
    'Code-internal refusals mean the action-set persisted and this reading overstates the transformation; premise-rejecting refusals confirm action-set contraction and distinguish this reading decisively from a cost-inflation account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unthinkability_vs_disapproval, conceptual, 'Whether the end-state is unthinkability or expensive disapproval — the reading''s central conceptual distinction.').

omega_variable(
    late_theater_meaning,
    'Does the late-interval rise in ritualized, frequently bloodless affairs (deloping, negotiated satisfactions, scar-collecting student duels) evidence substrate decay with form outliving meaning, or successful adaptation that preserved the code''s function?',
    'Compare participant testimony and institutional uptake across the theatrical turn: did bloodless satisfaction still confer the standing the code promised, or did observers begin discounting it?',
    'If theatrical satisfaction stopped purchasing standing, the theater ratio indexes decay and the residue is inertial; if it kept purchasing standing, the late arrangement remained functional and the theater reading is wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_theater_meaning, empirical, 'Interpretation of the rising theater trajectory that anchors this reading''s decay narrative.').

omega_variable(
    substrate_naturality,
    'Was the honor code''s grip on its holders experienced as the shape of reality (natural, unquestionable) or as enforceable convention backed by sanction?',
    'Period self-understanding: memoirs, advice literature, and courts-of-honor proceedings — did gentlemen treat the code as optional law they might reform, or as simply what honor is?',
    'Natural-law-experienced grip predicts dissolution-by-unthinkability (no defeat required, matching this reading); convention-experienced grip makes suppression-centered accounts more plausible and would soften the axiom_overriding drift characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_naturality, conceptual, 'Whether the substrate presented as natural law to its holders — governs how its dissolution should be modeled.').

omega_variable(
    suppression_internalization_split,
    'How much of the code''s hold was structural (social-death sanctions administered by peers) versus internalized (self-concept constituted by honor, compulsion persisting absent observers)?',
    'Trajectories of men who exited the honor world (clergy converts, emigres, class-dissenters): did the compulsion persist after peer sanctions became unreachable?',
    'A large internalized share means effective suppression exceeded the structural measure and explains why practice decay lagged belief decay; a small share strengthens the substrate-collapse account, since belief change would translate directly into conduct change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the code''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hono_tr_t50, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement(hono_tr_t150, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(hono_tr_t200, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 200, 0.55).
narrative_ontology:measurement(hono_tr_t250, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 250, 0.72).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hono_be_t50, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 100, 0.64).
narrative_ontology:measurement(hono_be_t150, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 150, 0.66).
narrative_ontology:measurement(hono_be_t200, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(hono_be_t250, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 250, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hono_su_t50, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(hono_su_t150, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 150, 0.52).
narrative_ontology:measurement(hono_su_t200, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(hono_su_t250, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 250, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of kernel honor_satisfaction_substrate, decomposed per the epsilon-invariance principle because the colloquial label 'the end of dueling' conflates structurally distinct claims about where the arrangement's support lay. This member (cultural_contraction_reading) authors epsilon for the honor code read as interpretive substrate and locates dissolution in meaning-transformation; practice_decline_reading authors the same arrangement read as enforcement-defied practice atop a persisting substrate; composite_overdetermined_reading authors the joint-causal variant. Evidential structure: this reading and practice_decline_reading are direct rivals on the substrate-persistence question; the composite reading sits downstream of both, citing each as a partial pathway. Family members are linked pairwise through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
