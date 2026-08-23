% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor-Settlement Legitimacy Regime - Contraction Reading (Cognitive Framework Exit)
 *   domain: historical sociology / legal history / cultural anthropology
 *
 * SUMMARY:
 *   For roughly three centuries, European honor culture made the duel the
 *   legitimate terminal instrument of dispute settlement among gentlemen: an
 *   insult created a debt that only a rule-governed exchange of shots or
 *   blades could discharge, refusal meant social death, and courts of honor,
 *   seconds, and the press policed the code. This story instantiates the
 *   contraction_reading of the honor_settlement_legitimacy kernel: the
 *   arrangement's terminal state was full cognitive exit - the honor
 *   framework itself left the normative possibility space, so that dueling
 *   became not merely prohibited but incomprehensible as legitimate action.
 *   The epsilon referent is the standing arrangement under contest, the
 *   operative honor-settlement regime, assessed by this reading's own lights:
 *   a constructed (not natural) normative framework that genuinely contained
 *   feud violence while coercing participation and consuming lives. KEY
 *   AGENTS (by structural relationship): see key_agents; the regime's
 *   operator seat (courts of honor), its rent-collecting beneficiary seat
 *   (established honor elites), its coerced payer seats (juniors, aspirant
 *   civilians, dissenters, bereaved families), its excluded rival (the
 *   bourgeois status order), and its external observers (reformers, the
 *   nominally prohibiting state). Sibling readings are separate constraints,
 *   not positions inside this one: drop_reading authors a persisting fringe
 *   remnant with live coerced participants past 1930; composite_reading
 *   authors a multi-mechanism decline process. Their epsilon values differ
 *   accordingly - a persisting remnant carries nonzero continuing extraction
 *   this story's terminal series does not, and a multi-mechanism process
 *   distributes causal weight this story concentrates in framework
 *   transformation.
 *
 * KEY AGENTS:
 *   - officer_honor_tribunals: Agenda setter (institutional/constrained) - administers courts of honor, codifies satisfaction rules, enforces participation by dismissal
 *   - established_honor_elites: Primary beneficiary (powerful/identity_locked) - converts honor standing into command, marriage, and political capital; identity-fused with the code
 *   - duel_infrastructure_professionals: Secondary beneficiary (moderate/mobile) - seconds, surgeons, fencing masters, code-keeping journalists collecting fees from the traffic
 *   - junior_officers_and_subalterns: Primary target (moderate/trapped) - coerced participation, highest fatality exposure, no lawful recourse
 *   - civilian_gentlemen_of_limited_means: Target with aspirational buy-in (moderate/constrained) - purchases standing by risking the ground
 *   - duel_widows_and_bereaved_families: Ultimate cost bearer (powerless/trapped) - no seat, no redress, losses booked as accident
 *   - honor_class_dissenters: Internal resisters (moderate/constrained) - refusers bearing ostracism and career destruction
 *   - bourgeois_excluded_classes: Excluded rival (organized/mobile) - builds the alternative status order that outcompetes honor
 *   - religious_and_reform_opponents: External moral opposition (organized/analytical) - supplies the successor moral vocabulary
 *   - state_judicial_apparatus: Nominal prohibitor, de facto bystander (institutional/analytical) - three centuries of unenforced bans
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.65).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.78).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor-Settlement Legitimacy Regime - Contraction Reading (Cognitive Framework Exit)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical sociology / legal history / cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '25ca948b-9e5d-4c6e-8ba1-59f6b08d4409').
narrative_ontology:cs_kernel_codification('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', formalized).
narrative_ontology:cs_authority_grounding('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', practice).
narrative_ontology:cs_interpretation_layer_present('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409').
narrative_ontology:cs_reading_relation('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', foundational, framework_transformation_extinguishes_action_legitimacy).
narrative_ontology:cs_axiom_status(framework_transformation_extinguishes_action_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', framework_transformation_extinguishes_action_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', secondary, honor_code_was_contingent_construction).
narrative_ontology:cs_axiom_status(honor_code_was_contingent_construction, holdable).
narrative_ontology:cs_axiom_grounding('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', honor_code_was_contingent_construction, empirically_contingent).
narrative_ontology:cs_reference_frame('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', operative_honor_satisfaction_order).
narrative_ontology:cs_drift_state('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', post_great_war_normative_settlement, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('25ca948b-9e5d-4c6e-8ba1-59f6b08d4409', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, established_honor_elites).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, duel_infrastructure_professionals).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, junior_officers_and_subalterns).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, civilian_gentlemen_of_limited_means).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, duel_widows_and_bereaved_families).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, honor_class_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, civilian_gentlemen_of_limited_means).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Panels of senior officers convened under regimental and army regulations to judge questions of insult, apology adequacy, and refusal. They publish procedural codes, certify which grievances demand satisfaction, and can strip an officer's commission for declining a properly issued challenge. Their rulings are backed by dismissal and by the corps' own opinion-forming hierarchy. Members rotate through the seat; the institution outlasts any member.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, officer_honor_tribunals, agenda_setter,
    institutional, generational, constrained, national).

% Senior officers, titled families, and office-holding gentry whose reputations function as capital: command assignments, regimental patronage, marriage alliances, and political deference all price in honor standing. A reputation for readiness to answer insults lets them police slights without fighting most of the time; when they do fight, seconds and surgeons manage the risk. Family names carry the standing forward a generation. Leaving the honor economy would mean renouncing the currency their position is denominated in.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, established_honor_elites, beneficiary,
    powerful, generational, identity_locked, continental).

% Seconds who negotiate terms, fencing masters who train and sometimes stand as proxies, surgeons who attend the ground, and journalists who report and adjudicate affairs of honor in print. They collect fees, subscriptions, and professional standing from the traffic in challenges. Their skills and clienteles transfer to sport fencing, medicine, and ordinary journalism if the traffic dries up.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, duel_infrastructure_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Young commissioned men at the bottom of the regimental hierarchy. A refused challenge follows them through every promotion board and mess dinner; acceptance puts them on the grass at dawn against men with more practice. They cannot take their grievance to a lawyer without ending their careers, and they cannot resign without poverty. Most fight; some die; the ones who refuse are usually finished.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, junior_officers_and_subalterns, payer,
    moderate, biographical, trapped, national).

% Lawyers, doctors, journalists, and minor officials who buy admission to elite standing by demonstrating willingness to fight. A duel fought and survived purchases entry into drawing rooms and newspaper columns that birth alone would not open. The price is injury or death at odds they did not set, in a currency they can only earn by risking it. Declining the game means accepting permanent exclusion from the rooms they aspire to.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, civilian_gentlemen_of_limited_means, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, civilian_gentlemen_of_limited_means, beneficiary).

% Wives, children, and dependents of men killed in settlements. The death arrives without legal remedy: coroners record misadventure, prosecutors decline to treat participants as murderers, and provision for officers' families is grudging. They had no voice in the quarrel, the negotiation, or the ground. Their loss is the practice's output, borne entirely off its books.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, duel_widows_and_bereaved_families, payer,
    powerless, generational, trapped, national).

% Men inside the honor world, often moved by evangelical religion or by plain fear, who decline challenges or refuse to issue them. Courts of honor rule their refusals dishonorable, messes freeze them out, newspapers print their names, and promotion stops. A minority find patrons or marry into safety; most endure quiet ruin. Some become public lecturers against the practice after leaving.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_class_dissenters, payer,
    moderate, biographical, constrained, national).

% Commercial and professional families whose wealth grows while their status orders are officially worthless: the honor code bars them from giving or receiving satisfaction and codes their legal and financial ways of settling quarrels as cowardice. They respond by building rival currencies - creditworthiness, professional licensure, electoral office, press ownership - and by funding and voting for the reform movement. Their exit from caring about honor approval is available and, over the interval, taken.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_excluded_classes, excluded,
    organized, generational, mobile, national).

% Clergy of every confession, tract writers, anti-dueling societies, and moral philosophers who condemn the practice as sin and murder. They bury the casualties, counsel refusers, petition legislatures, and supply the moral vocabulary later generations inherit. They are never admitted to the courts where refusals are judged; their influence runs through conscience and print, not through the honor forums.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, religious_and_reform_opponents, observer,
    organized, civilizational, analytical, continental).

% Royal edicts, parliamentary statutes, magistrates, and prosecutors. Formal prohibitions multiply for three centuries; enforcement does not follow. Duels are prosecuted sporadically, deaths are certified as accidents, and officers are rarely cashiered by the state for what their regiments reward. The apparatus holds the legal power to end the practice at nearly any point and declines to spend it, treating the honor world as someone else's jurisdiction.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, state_judicial_apparatus, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, established_honor_elites).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bounded, ritualized settlement of status disputes among armed elites: it converted potentially unlimited vendetta into a single rule-governed violent event with defined endpoints, witnesses, and reconciliation norms, and it sustained a shared status currency (honor) that ordered the class internally.
% TRANSFER_FUNCTION: Moved life-and-limb risk and death onto the duel ground, borne disproportionately by juniors and the challenged; moved honor standing from refusers and losers to acceptors and victors; moved fees to seconds, surgeons, and fencing masters; moved deference and obedience to courts of honor and the code-keeping elite.
% ABSENT_VOICES: Duel widows and bereaved families had no seat in any honor forum; the men killed in settlements had, at the moment of settlement, no exit the code recognized; servants and witnesses who knew the truth of quarrels were ignored or bound to silence; the rising commercial classes whose ways of settling disputes were coded cowardly were barred from the conversation defining satisfaction. Religious objectors spoke publicly but were never admitted to the courts where refusals were judged.
% DISAPPEARANCE_RATIONALE: Honor partisans insisted the social order would come apart without the settlement mechanism: the status currency would lose its enforcement layer and quarrels would slide into vendetta. Reformers answered that courts, police, and a press-based public sphere could carry dispute settlement instead. The dispute was settled empirically by the actual disappearance: after the framework exited, elite interpersonal violence fell without feud resurgence, because judicial monopoly, professional policing, and liberal dispute law had matured into functional substitutes. Mid-interval removal would have rearranged the world; the removal that actually happened, late and gradual, did not.
% FOUNDING_PROBLEM: In the absence of impartial centralized adjudication credible to armed elites, private quarrels among gentlemen threatened to escalate into vendettas, factional violence, and civil disorder; the regulated duel bounded this violence into a single, rule-governed settlement event with witnesses and reconciliation norms.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by state-formation historiography on the judicial monopoly of legitimate violence, by eighteenth- and nineteenth-century legal commentators documenting the transfer of dispute settlement to courts, and by the post-dueling record itself: elite interpersonal violence declined without feud resurgence. The honor partisans' own testimony that abolition would produce chaos was falsified by the outcome. No living party attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.65, anchored at the regime's operative plateau (roughly 1815-1848), the period when the standing arrangement actually governed: participation was coerced under social-death threat, deaths recurred annually, and status rents flowed to established reputations, while a real feud-containment service was delivered. Suppression is authored high (0.78) because the code's hold fused structural sanctions (courts of honor, dismissal, ostracism, press ruin) with internalized honor identity; note suppression is a raw structural property in the engine's arithmetic - it is not scaled by power or scope, unlike extractiveness. Theater at the anchor is low-moderate (0.25) because the settlement function was earnest and lethal; the series shows Goodhart drift crossing 0.5 by 1880 as ritualization, first-blood conventions, and exhibition duels displaced function. Accessibility collapse is 0.70: once the code is understood, within-framework alternatives (litigation, unaccompanied apology, ignoring a slight) are foreclosed - going to law was coded cowardice - and the remaining exit was leaving the class entirely at ruinous cost, which keeps the value below the natural-law range. Resistance is 0.58: three centuries of clerical condemnation, statutory prohibition, reform societies, and celebrated refusers, persistently ineffective because enforcement was withheld; resistance shaped the terminal form of the collapse (cognitive exit) more than the operating form. Claim and metrics are independent authored facts: the claimed type tangled_rope rests on structure (genuine coordination function plus asymmetric coerced costs plus active enforcement), not on the metric values. The measurement series runs on one shared seven-point grid (1770, 1789, 1815, 1848, 1880, 1900, 1930) with all three tracked metrics authored at every point. The series is non-monotonic by design: the Revolutionary disruption dips 1789, the Restoration re-tightens 1815, and German corps enforcement hardens inside a shrinking domain through 1880 before general collapse. The mild oscillation is exogenous - political regime shocks - not an intermittent-reinforcement mechanism. The divergence between the base_properties scalars (operative-life anchor) and the series terminus (near zero) is the contraction itself, the story's subject, not an inconsistency.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience four different arrangements under the same structure. From the established elite seat, the code is the constitution of a world: honor standing is the currency of command and marriage, and refusal is not a risky choice but a self-incomprehensible one - the identity lock is professional and relational at once, the officer's self-concept constituted by the standing the code prices. From the trapped junior's seat, the same structure is enforced extraction: participation compelled by career destruction, risk borne at odds set by others. From the excluded bourgeois seat, it is an illegitimate monopoly on status settlement that codes their ways of life cowardly. From the state's seat, it is a tolerable nuisance outside its effective jurisdiction. The engine computes these divergences from the structural data. Coalition analysis: the trapped payer seats never coalesced because the honor economy priced defection individually - each refusal was punished publicly and locally, converting would-be coalition members into cautionary exhibits; collective abstention would have required simultaneously enforced solidarity across regiments and nations, which the code's gossip infrastructure was built to prevent. Identity-lock dynamics: when the frame broke after the Great War - mass armies diluting the corps, aristocratic authority collapsing, the war itself discrediting martial honor - the arrangement's hold evaporated within roughly a generation, which is the observational signature of identity-fused rather than purely structural maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Established honor elites sit near the beneficiary pole (collect the status rents, identity_locked exit amplifies their investment in the structure's persistence); duel infrastructure professionals collect fees with mobile exit, damping their effective stake; the courts of honor administer the arrangement and their corps collects discipline and cohesion, placing the agenda-setter seat at low-to-moderate directionality. Junior officers, aspirant civilians, dissenters, and bereaved families sit near the target pole - the widows and bereaved families nearest the full-target end, bearing pure cost with no offsetting flow and no exit. The excluded bourgeois classes bear the suppression side of the structure (their rival status order is disparaged and barred) without paying its transfer, giving them elevated directionality through exclusion rather than payment. The state and the reformers sit outside the transfer entirely. No directionality overrides are authored: the derivation from role declarations plus exit atoms reproduces these relationships without correction, and the story's seats differ on exit options and scope enough that the per-power-atom override mechanism would be too coarse to improve on the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - containing feud escalation among armed elites absent credible impartial adjudication - died with the state's judicial monopoly, substantially by the mid-nineteenth century in Western Europe. The arrangement then persisted for roughly a century past functional obsolescence on pure honor logic, and it could not be killed by the means tried: three centuries of statutory prohibition failed because banning a settlement mechanism without dissolving the status currency leaves the demand intact - the honor economy kept pricing insults in blood whether or not the statute book approved. The mandate resolved only when the framework that gave honor its purchasing power exited the possibility space, which is this reading's core claim: mandatrophy resolved by framework death, not by reform. The classification guards against both mislabelings: reading the operative regime as pure extraction erases the feud-containment function that made prohibition fail for three hundred years; reading it as pure coordination erases the coerced dead and the bereaved families with no seat. Tangled rope with terminal cognitive exit captures both, and the R5 interview records the genealogy: founding problem dead, corroborated by state-formation historiography and by the falsified partisan prediction of post-abolition chaos.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the terminal state of the honor-settlement arrangement full cognitive exit (this reading), fringe persistence among residual adherents (drop_reading), or overdetermined multi-mechanism decline (composite_reading)?',
    'Comparative coding of residual practice and stated legitimacy beliefs in 1920-1960 remnants (German corps fencing, Italian ceremonial duels, Iberian cases): count cohorts that still treat the settlement as legitimate versus those performing it as heritage.',
    'If drop_reading is correct, a live low-grade arrangement with real coerced participants persists past this story''s interval and the terminal near-zero series values understate continuing costs; if composite_reading is correct, this story''s single-mechanism attribution overstates cognitive determination and legal and economic mechanisms share causal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Reading choice within the honor_settlement_legitimacy kernel is under-determined by the terminal record.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the force that held gentlemen to the duel-ground structural (courts of honor, dismissal, ostracism) or internalized (honor identity making refusal self-incomprehensible)?',
    'Post-sanction trajectory analysis: after 1918 structural penalties lapsed faster than practice did; biographical evidence of last-cohort officers reporting inability to refuse despite visible enforcement collapse indicates internalized residue. Sources: memoirs, diaries, cohort correspondence.',
    'If largely internalized, the scalar suppression understates the arrangement''s true hold during operation, explains the lag between sanction collapse and practice collapse, and shifts the persistence explanation from enforcement capacity to identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in an identity-fused honor population.').

omega_variable(
    prohibition_vs_framework_causality,
    'Did dueling become unthinkable because cultural framework transformation dissolved the honor premise, or because accumulating legal prohibition finally became effective?',
    'Comparative timing and cross-jurisdiction analysis: prohibition existed for three centuries without effect while collapse tracked belief change within a generation; compare jurisdictions with similar statutes but different honor-economy penetration (Britain versus France versus the German corps); test whether enforcement tolerance correlated with prior belief shift rather than statute severity.',
    'If prohibition-causal, this story''s framework-exit attribution overstates cultural determination and the composite_reading gains weight; if framework-causal, statutory-history accounts misattribute the cause, and prohibition efficacy was derivative of prior belief change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_vs_framework_causality, empirical, 'Causal locus of the legitimacy collapse: normative framework transformation versus legal prohibition.').

omega_variable(
    mensur_fossil_discontinuity,
    'Does German academic fencing (Mensur) continue the honor-settlement arrangement in attenuated form, or is it a transformed sport with the settlement function wholly amputated?',
    'Functional analysis of contemporary practice: presence or absence of dispute-settlement triggers, consent structures, and legitimacy claims; trace continuity of rules, corporations, and justificatory rhetoric from the corps codes.',
    'If continuous, the full-exit terminal claim fails and a live attenuated arrangement persists past the interval; if discontinuous, the fossil confirms framework exit - performance without the normative premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mensur_fossil_discontinuity, empirical, 'Whether surviving German corps fencing is continuous with the historical arrangement.').

omega_variable(
    honor_currency_recurrence,
    'Do modern status economies (credentialing, celebrity, online reputation) re-instantiate the pattern the honor economy carried - status enforced by individually-priced defection punishments with identity-priced exit?',
    'Structural comparison: identify modern settings where refusal of a status ritual carries individually-assessed, publicly-administered ruin and where exit is priced in identity terms; test for coerced-participation dynamics analogous to the challenged gentleman''s.',
    'If recurrent, the contraction is episode-specific rather than permanent and a new family member should be authored for the modern analogue; if not, the exit marks a durable change in the normative possibility space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_currency_recurrence, conceptual, 'Whether the honor-economy pattern recurs in modern status systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1770, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsl_contraction_tr_t1770, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1770, 0.12).
narrative_ontology:measurement(hsl_contraction_tr_t1789, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1789, 0.15).
narrative_ontology:measurement(hsl_contraction_tr_t1815, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1815, 0.22).
narrative_ontology:measurement(hsl_contraction_tr_t1848, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1848, 0.35).
narrative_ontology:measurement(hsl_contraction_tr_t1880, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1880, 0.52).
narrative_ontology:measurement(hsl_contraction_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.68).
narrative_ontology:measurement(hsl_contraction_tr_t1930, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1930, 0.82).

% Extraction over time
narrative_ontology:measurement(hsl_contraction_be_t1770, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1770, 0.78).
narrative_ontology:measurement(hsl_contraction_be_t1789, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1789, 0.74).
narrative_ontology:measurement(hsl_contraction_be_t1815, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1815, 0.8).
narrative_ontology:measurement(hsl_contraction_be_t1848, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1848, 0.66).
narrative_ontology:measurement(hsl_contraction_be_t1880, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1880, 0.48).
narrative_ontology:measurement(hsl_contraction_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(hsl_contraction_be_t1930, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1930, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(hsl_contraction_su_t1770, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1770, 0.85).
narrative_ontology:measurement(hsl_contraction_su_t1789, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1789, 0.78).
narrative_ontology:measurement(hsl_contraction_su_t1815, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1815, 0.83).
narrative_ontology:measurement(hsl_contraction_su_t1848, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1848, 0.7).
narrative_ontology:measurement(hsl_contraction_su_t1880, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1880, 0.55).
narrative_ontology:measurement(hsl_contraction_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.38).
narrative_ontology:measurement(hsl_contraction_su_t1930, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1930, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the decline of dueling' per the epsilon-invariance principle. The single label conflates three structurally distinct claims: (1) this file, contraction_reading - the terminal state is full cognitive exit, the honor framework left the possibility space, epsilon anchored at the operative regime's coerced-and-lethal operation with the series running to extinction; (2) drop_reading - a fringe remnant persists among residual adherents, a persisting-constraint claim with nonzero continuing extraction past 1930; (3) composite_reading - decline overdetermined by multiple reinforcing mechanisms, distributing causal weight across legal, economic, military, and cognitive channels. Upstream/downstream structure: the composite reading cites the contraction mechanism as one of its edges, so this story influences the composite; the drop reading's persisting-remnant premise is incompatible with this story's full-exit premise, an authentic foreclosure pair within the kernel. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
