% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem as Interior Warfare - Allegorical Displacement Reading of Deuteronomy 7
 *   domain: religious/hermeneutical/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the allegorical_displacement_reading of the
 *   herem_command_dt7 kernel: the command to destroy the seven nations and
 *   show them no mercy is read typologically, its targets relocated from
 *   ethnic groups to the practitioner's own vices, its warfare relocated from
 *   campaign to conscience. Within this frame the arrangement is an
 *   identity-coordination discipline - a standing obligation of unremitting
 *   interior war that binds the community together and gives it a way to keep
 *   the canon without licensing violence against neighbors. KEY AGENTS (by
 *   structural relationship): pastoral_interpretive_office
 *   (institutional/constrained) - administers the typology and accrues
 *   interpretive authority; typologically_formed_communities
 *   (organized/identity_locked) - principal beneficiary, receives a coherent
 *   moral identity; scrupulous_devotees (moderate/identity_locked) -
 *   dual-positioned insider bearing disproportionate psychological cost;
 *   lapsed_or_escaping_members (powerless/mobile) - excluded witnesses whose
 *   testimony the tradition discounts; comparative_religion_scholars
 *   (analytical/analytical) - observer seat. FAMILY NOTE (epsilon
 *   decomposition across the kernel): the same text supports three
 *   structurally distinct constraints. The durable_separation_reading authors
 *   high extraction with living ethnic victim sets; the
 *   contextual_supersession_reading authors moderate extraction with a
 *   closed-but-lingering historical harm; this allegorical reading authors
 *   low extraction with the victim set collapsed to abstractions. Each is a
 *   separate file with its own epsilon; this story's values describe only its
 *   own frame, and the linked files document theirs.
 *
 * KEY AGENTS:
 *   - - pastoral_interpretive_office: agenda-setter and collector (institutional/constrained) - administers the typological method, prescribes the interior disciplines, and accrues the interpretive authority the method generates
 *   - - typologically_formed_communities: primary beneficiary (organized/identity_locked) - receives canon continuity and moral identity; exit experienced as self-loss
 *   - - scrupulous_devotees: dual-positioned beneficiary/cost-bearer (moderate/identity_locked) - receives formation, carries disproportionate guilt-vigilance load
 *   - - lapsed_or_escaping_members: excluded witness (powerless/mobile) - bears firsthand testimony of cost, positioned outside the interpretive conversation
 *   - - comparative_religion_scholars: analytical observer (analytical/analytical) - sees the displacement mechanism across traditions, collects nothing from the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.24).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.26).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Interior Warfare - Allegorical Displacement Reading of Deuteronomy 7").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious/hermeneutical/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '1ed2b338-9761-47dc-a3ba-9ea7f3bc389d').
narrative_ontology:cs_kernel_codification('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', fixed_text).
narrative_ontology:cs_authority_grounding('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', lineage).
narrative_ontology:cs_interpretation_layer_present('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d').
narrative_ontology:cs_reading_relation('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_axiom('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', foundational, nations_are_typological_vices).
narrative_ontology:cs_axiom_status(nations_are_typological_vices, holdable).
narrative_ontology:cs_axiom_grounding('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', nations_are_typological_vices, theological).
narrative_ontology:cs_axiom('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', foundational, unremitting_mortification_of_sin).
narrative_ontology:cs_axiom_status(unremitting_mortification_of_sin, holdable).
narrative_ontology:cs_axiom_grounding('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', unremitting_mortification_of_sin, deontological).
narrative_ontology:cs_reference_frame('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', perennial_interior_warfare_template).
narrative_ontology:cs_drift_state('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', contemporary_pluralist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ed2b338-9761-47dc-a3ba-9ea7f3bc389d', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, typologically_formed_communities).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, pastoral_interpretive_office).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, scrupulous_devotees).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, scrupulous_devotees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preaches, teaches, and adjudicates the typological method: maps the seven named nations onto categories of vice, prescribes the regimens of self-examination, confession, and mortification, and answers challenges to the reading from inside and outside the community. The office's standing, curriculum, and publishing economy rest on the method's continued authority; its members are formed inside that vocation and their exegetical training carries nowhere else.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, pastoral_interpretive_office, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, pastoral_interpretive_office, beneficiary).

% Inherit the canon through this reading: liturgy and catechesis present the ancient command as a summons to war on their own pride, greed, lust, and fear rather than on any neighboring people. Marriage,child-rearing, and mutual accountability are organized around the interior disciplines. Leaving would mean redescribing the moral selves the practice built, which most members experience as self-annihilation rather than as an option among others.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, typologically_formed_communities, beneficiary,
    organized, generational, identity_locked, global).

% Practice the discipline at an intensity that turns examination into compulsion: repeated confession, doubt over absolution, ordinary desire treated as occupied enemy territory. They report the war as ceaseless and its victories as provisional; some seek relief outside the community while fearing that doing so proves the enemy won. They receive the formation and meaning the practice offers, and they carry a disproportionate share of its psychological weight.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, scrupulous_devotees, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, scrupulous_devotees, payer).

% Former members who left after concluding the interior war was damaging them. They describe vigilance-guilt cycles, shame at recurring relapse, and marked relief after exit. The tradition reads their testimony as evidence of spiritual defeat rather than as data about the practice, so they speak from outside the interpretive conversation their experience would most directly inform.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, lapsed_or_escaping_members, excluded,
    powerless, biographical, mobile, national).

% Study how textual communities relocate violent commands into interiority across traditions: compare this reading's operation with historically literal applications and with supersessionist reframings, trace the Alexandrian and midrashic lineages of the method, and publish under academic rather than ecclesial norms. They neither fund nor perform the practice.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, pastoral_interpretive_office).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives canon-holding communities a stable shared answer to an unsolvable external problem: how to keep reading an authoritative command of total war against named peoples. The typological method converts that external problem into an internal, perpetually solvable one - the mortification of one's own vices - preserving continuity of text, liturgy, and communal identity while removing the ethnic referent.
% TRANSFER_FUNCTION: Moves attentional and disciplinary labor - daily self-scrutiny, confession, fasting, vigilance against relapse - out of adherents' ordinary lives and into a regulated regimen, and moves interpretive authority, pulpit platform, and publishing prestige to the offices that administer the regimen. No wealth, land, or labor-service changes hands.
% ABSENT_VOICES: Descendants of the populations the text originally named are absent from the typological conversation: the reading speaks of sin, not of them, and their historical experience appears only in historiography and post-trauma hermeneutics conducted largely outside the tradition. Lapsed members are also effectively absent - present in the pews' memory but disqualified as witnesses by the category of backsliding.
% DISAPPEARANCE_RATIONALE: Overnight removal would hand canon-holding communities the command raw. Some congregations would revive separatist literalism; some would quietly shelve the passages; some would adopt supersessionist reframings; none would simply continue as before. Liturgy, catechesis, pastoral counseling, and the teaching office's vocation would all reorganize around whatever replacement resolution each community chose.
% FOUNDING_PROBLEM: How can a covenant community keep reading an authoritative text that commands destruction of named nations without either carrying out such destruction or surrendering the text's authority - a problem made acute when the original settlement context became irrecoverable, and again for the church when gentile inclusion raised the Marcionite question of the old canon's place.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: historians of early Christianity independently attest the second-century canon crisis that drove Alexandrian allegoresis; academic biblical scholarship, confessional and secular alike, attests that communities still confront the command in the lectionary and the liturgical cycle; post-Holocaust theologians writing from outside these communities attest the problem's renewed urgency. The founding problem's persistence is not asserted by the tradition alone.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.24 at interval end) because the arrangement's costs are mostly self-directed formation labor borne by the same people who receive its benefits, sitting only modestly above what an identity-coordination mechanism inherently costs; the residue is the prestige flow to the teaching office and the unpriced load carried by scrupulous members. Suppression is low-moderate (0.26) and DECLINING over the interval: the arrangement sustains itself through formation and plausibility rather than enforcement machinery, and interpretive pluralism has progressively eroded what coercive capacity remained - hence a falling suppression_requirement series modeling enforcement decay, not buildup. Theater is low but rising (0.10 to 0.19) as 'spiritual warfare' hardened into a performative genre - conferences, deliverance spectacles, martial devotional media - whose dramatization increasingly substitutes for the quiet examination the method prescribes. Accessibility_collapse is low (0.30): therapeutic, secular, and rival hermeneutical alternatives remain fully available once the discipline is understood. Resistance is moderate (0.34): antinomian fatigue inside, secular critique and literalist scorn outside. All three tracked series run on ONE shared time grid (points 0,3,6,9,12,15,18; one unit approximates four years, spanning the post-Holocaust era to the present), so no metric is sampled against another metric's end-state. The interval's initial values describe the arrangement as it stood when postwar scrutiny made literal readings untenable in mainstream communities and the allegorical frame became the load-bearing resolution. The claimed_type (rope) and the metrics are independently authored: I believe the coordination function is genuine and dominant and the extraction marginal; the engine computes each seat's classification from the structural data, and if the ascetic-cost or continuity-motive omegas resolve adversely, seat-level drift toward a hybrid profile is the expected finding, not an error.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the pastoral_interpretive_office seat the arrangement is a venerable formation tradition it stewards and transmits; from typologically_formed_communities it is the fabric of membership itself; from scrupulous_devotees it is a war that never grants discharge; from lapsed_or_escaping_members it is the thing they fled, remembered as harm; from comparative_religion_scholars it is one instance of a general displacement mechanism. Same text, same method, four different lived objects - produced by differing power, exit, and horizon atoms, not by any authored verdict. The engine derives these per-seat classifications; the prose only explains why they diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low-directionality seats: typologically_formed_communities (subsidized in identity and meaning) and pastoral_interpretive_office (subsidized in authority, though it also administers - hence its dual agenda_setter/beneficiary declaration rather than a bare beneficiary tag, which would understate its stake in the method's continuation). No victim group is declared anywhere in this story: the reading's defining structural move is the collapse of the victim set to abstract vices, which bear nothing, and authoring human victims here would falsify the frame. The one correction the automatic derivation cannot make is captured by an override: scrupulous_devotees carry a moderate power atom and a primary beneficiary role, so derivation alone would place them deep at the subsidized end; their actual position is near-symmetric with a cost-lean (override d=0.48), reflecting formation received against disproportionate psychological burden. Lapsed members sit at the high-directionality end historically - they bore the discipline's costs and exited - though as an excluded seat they no longer feed the live arrangement. Suppression is authored as a raw structural property (0.26) and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation, which is why the wide-scope, low-power seats matter more for chi than the suppression figure suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions of mislabeling. Reading this arrangement as pure extraction (as some external critics would) erases the genuine collective problem it solves - canon inheritance without licensed violence - and would predict enforcement machinery and suppressed exits that the data contradict (falling suppression, low accessibility collapse). Reading it as a natural or permanent feature would naturalize what is in fact a maintained interpretive choice with a documented founding problem. The founding problem is LIVE (corroborated outside the beneficiary set), so this is not a mandatrophy case: the R5 mismatch consumer finds status=live crossed with disappearance_verdict=world_rearranges, the healthy cell - no zombie flag. The forward risks are tracked by omegas rather than by premature type claims: theater drift toward performative spiritual-warfare genres, cost concentration on scrupulous members, and motive substitution toward office continuity are the three accumulation vectors that could degrade this rope over subsequent intervals, and each is instrumented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the herem_command_dt7 kernel; what structurally changes if a community adopts a sibling reading instead?',
    'Not resolvable by data inside any single framework - resolution is a framework choice. Document the locating signals (reception history, magisterial and rabbinic rulings, prevailing homiletic practice) and treat adoption events as switches between the linked stories rather than as parameter updates within this one.',
    'Adoption of the durable_separation_reading restores ethnic-outsider victim sets and high extraction on designated populations; adoption of the contextual_supersession_reading closes the command in the past and dissolves the standing internal obligation. This story''s low-extraction profile holds only within the allegorical frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: the disagreement between readings is located in the referent of ''nations'' (peoples versus vices) and in the command''s temporal scope (standing versus closed).').

omega_variable(
    displacement_stability_under_crisis,
    'Does typological displacement durably prevent reactivation of the literal herem template, or does the literal sense remain banked beneath the allegory, recoverable under existential threat?',
    'Comparative historical coding of crisis episodes in typological communities: identify occasions on which preachers or settlers reached for the literal application despite an inherited allegorical tradition (crusade-era preaching, colonization-era invocations) and characterize the conditions under which the allegorical layer held or failed.',
    'If the displacement is stable, this reading''s low-extraction profile is robust; if unstable, the arrangement carries latent tail-risk and its benign classification holds only conditional on the absence of crisis conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_stability_under_crisis, empirical, 'Whether allegorical displacement is load-bearing or merely decorative over a retained literal sense.').

omega_variable(
    ascetic_cost_benefit_balance,
    'For scrupulous practitioners, does the perpetual interior-war discipline yield formation gains exceeding its psychological costs?',
    'Clinical and pastoral-outcome literature on religious scrupulosity: compare distress, daily functioning, and reported meaning across intensity strata of observance within the same communities.',
    'If costs dominate for a sizable subgroup, the scrupulous seat''s effective burden rises above its derived subsidy and that seat''s classification drifts from coordinated-and-benefited toward coordinated-and-charged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ascetic_cost_benefit_balance, empirical, 'Proportionality of the discipline''s internal costs for the scrupulous minority.').

omega_variable(
    cs_framing_underdetermination,
    'Is the fixed-text-plus-lineage framing of this commitment system the only defensible one, or does an alternative framing - the interpretive tradition itself as a distributed kernel administered through rabbinic and homiletic practice - fit the reception data equally well?',
    'Test both framings against reception behavior: if interpretive authority tracks living practice and precedent more than the verse''s wording, the distributed/practice framing fits; if rulings and sermons cite the wording itself as controlling, fixed-text/lineage holds.',
    'Under the distributed/practice framing, foreclosure relations between readings soften (a multi-referent practice can tolerate sibling readings side by side) and this story''s reading_relations and drift_state would need recomputation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative commitment-system framings for the same kernel, with different relational consequences.').

omega_variable(
    institutional_continuity_motivation,
    'Does the reading persist because it solves the canon-inheritance problem, or because it sustains the teaching office''s authority and publishing economy?',
    'Counterfactual and historical test: examine cases where hermeneutical authority democratized (lay-led study movements retaining typology after clerical disestablishment) and ask whether the method survived without the office that administered it.',
    'If continuity-of-office motivation dominates, the theater_ratio is understated, the agenda-setting seat sits nearer the target end than its beneficiary declaration implies, and pressure grows toward a hybrid coordination/extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_continuity_motivation, empirical, 'Motive attribution for the reading''s persistence: problem-solution versus institution-serving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t3, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(here_tr_t6, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(here_tr_t9, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement(here_tr_t12, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(here_tr_t15, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(here_tr_t18, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 18, 0.19).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(here_be_t3, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 3, 0.19).
narrative_ontology:measurement(here_be_t6, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(here_be_t9, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 9, 0.22).
narrative_ontology:measurement(here_be_t12, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 12, 0.23).
narrative_ontology:measurement(here_be_t15, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(here_be_t18, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 18, 0.24).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(here_su_t3, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 3, 0.36).
narrative_ontology:measurement(here_su_t6, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(here_su_t9, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 9, 0.31).
narrative_ontology:measurement(here_su_t12, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(here_su_t15, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement(here_su_t18, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 18, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the herem command' covers three structurally distinct constraints distinguished by the referent of 'nations' and the command's temporal scope. Decomposition follows the epsilon-invariance principle: measuring the command as ethnic-boundary mandate, as closed historical directive, and as standing interior-warfare discipline yields different epsilon, different victim sets, and different failure modes, so they are authored as three stories linked through affects_constraints. Reception-history direction runs durable_separation (plain sense, upstream) -> allegorical_displacement (Alexandrian/midrashic relocation, developed as a response that presupposes the plain sense it displaces) -> contextual_supersession (which borrows the allegorical move's moral relief while rejecting its standing-obligation corollary). This file is the middle term: it influences the supersession reading's legitimacy conditions (if the command is really about sin, supersession loses its urgent motive) while foreclosing the durable separation reading's core premise outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__allegorical_displacement_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
