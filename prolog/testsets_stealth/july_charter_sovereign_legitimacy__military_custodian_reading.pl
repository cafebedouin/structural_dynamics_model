% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Charter-Ratified Permanent Military Guardianship (Military Custodian Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary charter, ratified after a student-led uprising
 *   displaced the previous order, embeds the armed forces as a permanent
 *   institutional guardian charged with ensuring stability. Under the
 *   military-custodian reading — the reading instantiated in this file — the
 *   charter's sovereignty articles confer standing veto authority over
 *   civilian legislation, senior appointments, and defense-budget oversight,
 *   bound political contestation within security statutes, and shelter a
 *   network of military-affiliated enterprises from civilian audit. The
 *   standing arrangement under contest, which is this story's epsilon
 *   referent, is that custodial order itself: elected institutions administer
 *   daily governance while the command holds the levers that define sovereign
 *   office. KEY AGENTS (by structural relationship): military_high_command:
 *   agenda-setter (institutional/arbitrage) — administers the guardian
 *   clauses and collects the arrangement's revenues;
 *   elected_civilian_government: primary payer with a secondary beneficiary
 *   position (organized/trapped); autonomous_political_parties and
 *   student_movement: primary targets (trapped);
 *   military_business_conglomerates: concentrated beneficiary
 *   (powerful/constrained); international_security_patrons: external
 *   beneficiary (institutional/arbitrage); order_dependent_urban_elites:
 *   diffuse beneficiary-payer (organized/constrained); civilian_judiciary:
 *   payer with an identity-fused seat (institutional/identity_locked);
 *   independent_press: target (moderate/constrained);
 *   exiled_opposition_leadership and human_rights_monitoring_missions:
 *   excluded voices; constitutional_scholarship: analytical observer. This
 *   file is one member of a three-story constraint family decomposing the
 *   charter kernel; the claim (tangled_rope) and the metrics below are
 *   authored independently — the engine computes per-seat classifications
 *   from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.72).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Charter-Ratified Permanent Military Guardianship (Military Custodian Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '6ced9043-0bfa-4e6b-ad32-48b0581aa28d').
narrative_ontology:cs_kernel_codification('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', fixed_text).
narrative_ontology:cs_authority_grounding('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', extraction).
narrative_ontology:cs_interpretation_layer_present('6ced9043-0bfa-4e6b-ad32-48b0581aa28d').
narrative_ontology:cs_reading_relation('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', foundational, permanent_military_custody_required_for_stability).
narrative_ontology:cs_axiom_status(permanent_military_custody_required_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', permanent_military_custody_required_for_stability, instrumental).
narrative_ontology:cs_axiom('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', secondary, electoral_contestation_bounded_by_security_prerogatives).
narrative_ontology:cs_axiom_status(electoral_contestation_bounded_by_security_prerogatives, holdable).
narrative_ontology:cs_axiom_grounding('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', electoral_contestation_bounded_by_security_prerogatives, conventional).
narrative_ontology:cs_reference_frame('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', permanent_guardian_stewardship).
narrative_ontology:cs_drift_state('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', contemporary_post_transition_decade, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ced9043-0bfa-4e6b-ad32-48b0581aa28d', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_business_conglomerates).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, international_security_patrons).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, order_dependent_urban_elites).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_judiciary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, elected_civilian_government).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, order_dependent_urban_elites).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, elected_civilian_government).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, guardian_doctrine_of_civilian_incompetence).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_precedence_over_representation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the guardian clauses: holds a standing veto over civilian legislation, senior appointments, and defense-budget oversight; commands the country's only nationwide coercive organization; oversees a network of foundations and companies whose accounts are closed to civilian audit; can dismiss governments and parliaments that challenge security prerogatives. Its way out of any given rule is not leaving the arrangement but rewriting it — the veto applies to its own constraints.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate banks, construction firms, and food and energy holdings chartered under guardian-clause protections. They receive procurement contracts without competitive tender, land allocations, and tax exemptions; their revenue depends on the command's continued political position, and their holdings cannot win open-market competition against private firms on equal terms.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_business_conglomerates, beneficiary,
    powerful, biographical, constrained, national).

% Foreign governments and alliances that fund, arm, and train the force in exchange for basing rights, diplomatic alignment, and a predictable regional order. They can redirect aid and arms flows toward rival partners if returns diminish, so their position inside the arrangement is a portfolio choice rather than a commitment.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_security_patrons, beneficiary,
    institutional, biographical, arbitrage, global).

% Commercial associations, professional guilds, and property-holding families whose continuity depends on enforced order — functioning ports, insured supply chains, contained labor unrest. They fund the arrangement through a tax burden weighted toward defense and accept exclusion from security and foreign-policy decisions as the price of predictability.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, order_dependent_urban_elites, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, order_dependent_urban_elites, payer).

% Wins office through elections the charter permits and administers health, education, and municipal affairs. On security, intelligence, defense budget lines, and senior promotions it executes rather than decides; governments that previously challenged the command's prerogatives were dismissed. It receives stability and international creditworthiness from the arrangement while surrendering the levers that define sovereign office, and it cannot govern against command acquiescence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, elected_civilian_government, payer,
    organized, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, elected_civilian_government, beneficiary).

% Contest elections within boundaries drawn by security statutes: platforms touching the command's budget, amnesty questions, or foreign alignments are screened before registration; parties that crossed the line were dissolved and their leaderships prosecuted. Rebuilding a dissolved party means starting again under surveillance, so organizational memory lives in constant legal jeopardy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    organized, biographical, trapped, national).

% Provided the street force behind the founding uprising. Campuses now host garrison liaison offices, assemblies require prior clearance, and organizers face assignment to remote postings or prosecution under public-order statutes. Individual exit runs through exile or withdrawal into private professional life; collective exit — repeating the mobilization that founded the order — is the activity the clearance system exists to prevent.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, national).

% Investigates procurement irregularities and garrison conduct under licensing regimes controlled by the information ministry. Outlets that published command financial records lost their licenses; editors face defamation and anti-state charges; surviving outlets practice anticipatory omission of security-sector topics to keep printing at all.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_press, payer,
    moderate, biographical, constrained, national).

% Adjudicates commercial, family, and administrative law, but rulings touching security prerogatives are superseded by military tribunals or left unenforced. Senior benches are appointed through channels the command can veto, and a generation of judges has absorbed the guardian doctrine as constitutional common sense — the frame within which 'normal' constitutional law is defined. Leaving that frame would mean ruling against the doctrine that structured their entire careers.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_judiciary, payer,
    institutional, generational, identity_locked, national).

% Former parliamentarians, party secretaries, and student union presidents living abroad under entry bans, banking restrictions, and denial of broadcast access. They would argue the guardian clauses are a usurpation rather than a safeguard; they are kept outside the conversation by the same instruments they would contest.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, exiled_opposition_leadership, excluded,
    moderate, biographical, trapped, continental).

% Regional-court rapporteurs and United Nations special procedures seeking access to detention facilities, casualty records, and court-martial files. Access requests remain pending or denied; their published findings would undercut the stability justification on which the arrangement rests, which is why access is the contested object.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, human_rights_monitoring_missions, excluded,
    organized, biographical, constrained, global).

% Comparative constitutionalists and legal historians who map the charter's competing readings, publish amendment-path analyses, and advise opposition drafters. They hold no enforcement leverage; their influence runs through argument, citation in litigation, and eventual political openings.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_scholarship, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single nationwide coercive and logistical organization capable of territorial defense, border-region counterinsurgency, and disaster logistics; arbitrates among civilian factions at moments when elected institutions deadlock, preventing fragmentation of the state apparatus.
% TRANSFER_FUNCTION: Moves a protected share of national revenue and regulatory immunity from the civilian treasury and courts to the command and its affiliated enterprises; moves decision authority over security, intelligence, and succession from elected offices to the command; moves political initiative from parties and campus organizations into statutorily bounded channels.
% ABSENT_VOICES: Exiled opposition leadership and dissolved-party cadres would contest the guardian clauses' legitimacy but sit outside the country and the broadcast sphere; human-rights monitors hold pending access requests; rank-and-file soldiers have no representative channel separate from the command itself. Unanimity around the charter's stability framing arises partly because these seats were never in the ratifying room.
% DISAPPEARANCE_RATIONALE: Overnight removal of the guardian clauses would immediately expose the defense budget to parliamentary audit, transfer appointment and amnesty powers to elected offices, and legalize unscreened party platforms. The command would then face a compliance choice — acquiesce to civilianization or act extra-legally to restore the veto. Either path rearranges the constitutional order; nothing in the civilian institutional landscape keeps the arrangement alive by itself.
% FOUNDING_PROBLEM: In the immediate post-revolutionary vacuum the state faced armed fragmentation, collapsed administrative chains, and contested borders; the charter was built to guarantee governmental continuity by entrusting a permanent guardian role to the only institution with nationwide organizational reach.
% FOUNDING_PROBLEM_CORROBORATION: Regional mediation archives and United Nations transition-assessment reports from the ratification period corroborate that the founding problem — armed fragmentation and administrative collapse risk — was real and externally documented. On current status, corroboration splits outside the beneficiary set: election-observation missions and development-bank institutional assessments attest that civilian ministries have regained administrative capacity, while independent conflict monitors attest continuing insurgencies in two border regions. No outside source attests that permanent custody remains necessary; the necessity claim is attested only by the command itself.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the veto decouples security prerogatives from civilian accountability: the defense budget line, enterprise revenues, and appointment powers are removed from the ordinary give-and-take of representation, and the series shows steady accumulation from 0.54 to 0.72 as enterprise networks and statute layers thickened over the interval. Suppression is higher still (0.78) because persistence depends on actively criminalizing challenges — party dissolution powers, campus clearance requirements, licensing leverage over press — not on participant preference; the rising suppression_requirement series models the enforcement machinery hardening (new security statutes, expanded tribunal jurisdiction) rather than mere extraction shifting. Theater ratio is moderate-low (0.34): border-region operations, disaster logistics, and territorial defense are real delivered goods, but a growing share of guardianship activity is performative legitimation — stability rhetoric, anniversary ceremonies, doctrine documents — deployed as the founding collapse risk recedes. Accessibility collapse is 0.58: full civilian control remains imaginable and periodically demanded, elections occur within bounds, and exit routes (exile, private life) exist but are costly; alternatives are bounded, not erased. Resistance is 0.63: recurring campus uprisings, party litigation against dissolution decrees, and investigative journalism persist, reflecting latent coalition capacity among parties, students, and labor that historically toppled the predecessor order — the payer seats are individually weak but have demonstrated coalition potential. All three series run on one shared time grid (points 0-12, step 2) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the command's position the arrangement is a coordination structure it built and staffed — the guardian sees stewardship, and its arbitrage-grade exit (it rewrites rather than exits) places it near the subsidy end. From the trapped payer seats — parties, students, press — the same clauses operate as enforced subordination with no workable alternative. The civilian judiciary is the sharpest divergence case: institutionally powerful yet identity_locked, a generation of judges has fused professional identity with the guardian doctrine, experiencing the veto as constitutional common sense rather than imposition; if that identity frame broke, the judicial seat would recompute the arrangement as domination by a parallel legal order. International patrons, holding arbitrage exit, experience the arrangement as a portfolio position and compute minimal burden regardless of what it extracts domestically. The engine computes these per-seat classifications from power, exit, and declared position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the command (d near 0.0 — the arrangement subsidizes it directly), the conglomerates (protected revenue), the patrons (stability and basing returns, with arbitrage damping further), and the urban elites (order as a delivered good, offsetting their tax burden). Declared victims map toward high directionality: parties and students bear the full weight of bounded contestation with trapped exit, pushing their effective burden toward the full-target end; the press sits similarly with constrained exit. The elected government is genuinely dual-positioned — it receives stability and creditworthiness while surrendering sovereign levers — placing it mid-range rather than at either pole. The judiciary's identity_locked exit amplifies its effective burden beyond what its institutional power alone would suggest: it cannot even conceptualize exit, so whatever it bears, it bears fully. Spatial scope is national for nearly all seats, so scope amplification is modest and roughly uniform; the patrons' global scope and arbitrage exit keep their computed burden near zero despite material payments into the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — armed fragmentation and administrative collapse in the immediate post-revolutionary vacuum — was real and externally documented, and the custodial arrangement plausibly solved it. The mandatrophy question is whether that problem still lives. Outside assessors split: development-bank and election-observation reports attest civilian ministries have regained administrative capacity, while conflict monitors attest continuing insurgencies in two border regions. The R5 interview records this as contested, paired with a world_rearranges disappearance verdict — the constitutional order as built depends on the arrangement, which is precisely the signature of a mandate whose function may have receded while its structure self-perpetuates. Classification discipline cuts both ways here: a pure-extraction reading would erase the genuine security coordination that still prevents fragmentation in the periphery; a pure-coordination reading would erase the documented subordination of parties, students, and press. The tangled_rope claim preserves both facts and lets the temporal series — rising extractiveness and hardening suppression against a receding founding problem — carry the drift diagnosis toward capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_clause_reading_indexicality,
    'This constraint is one reading of the kernel july_charter_sovereign_legitimacy — specifically the military_custodian_reading, which treats the charter''s sovereignty articles as ratifying permanent military veto authority. Would the sibling readings (secular_democratic_reading, guided_nationalism_reading) instantiate structurally different constraints from the same text?',
    'Textual analysis of the sovereignty and security articles against drafting history and ratification debate; adoption of a sibling reading would relocate the victim set (under the secular-democratic reading, autonomous parties and the student movement cease to be targets and become rightsholders) and shift the beneficiary set toward electoral majorities.',
    'The disagreement is located in the sovereignty-grounding clause: security-apparatus custody versus civilian electoral mandate versus religious-national identity. Each resolution yields a different epsilon, different victim sets, and a different classification; this file''s values are valid only under the custodian reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_clause_reading_indexicality, conceptual, 'Which reading of the charter''s sovereignty clauses is instantiated, and how sibling readings restructure beneficiaries and victims.').

omega_variable(
    custody_necessity_empirical_status,
    'Is permanent military custody still causally necessary for state stability, or did a transitional necessity ossify into permanence once the founding collapse risk receded?',
    'Comparative analysis of post-revolutionary states that civilianized security sectors at comparable institutional maturity against those that retained custodial arrangements, controlling for insurgency presence and patron dependence.',
    'If custody is no longer necessary, the coordination component shrinks toward zero and the arrangement drifts toward pure extraction sustained by self-protection; if necessary, part of the measured burden is the price of the security good itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custody_necessity_empirical_status, empirical, 'Whether the founding problem still exists or the mandate has outlived its function.').

omega_variable(
    coordination_extraction_separability,
    'Is the security-provision function structurally separable from the veto-authority and enterprise privileges bundled into the same charter clauses?',
    'Budget-transparency audits separating defense-delivery costs from conglomerate revenue streams; jurisdictional comparisons where guard duties were retained while veto powers lapsed.',
    'If separable, the veto and enterprise layers are removable without losing the security function, and the arrangement''s persistence is explained by captured gain rather than functional need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and privilege components of the custodial arrangement can be unbundled.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is structural (statutes, garrison liaison offices, licensing regimes) versus internalized (judicial absorption of the guardian doctrine, press self-censorship norms, elite anticipatory obedience)?',
    'Post-liberalization trajectory in comparable transitions: if prosecutorial and editorial caution persists after formal repeal of security statutes, the internalized component is substantial.',
    'An internalized component means formal repeal alone would not restore bounded contestation; the effective suppression exceeds the statutory surface and outlasts the enforcing institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the suppression load.').

omega_variable(
    guardian_arrangement_reversibility,
    'Can the custodial arrangement be unwound incrementally through the charter''s own amendment channels, or does removal require confronting the coercive monopoly directly?',
    'Amendment-path analysis: whether any historical revision of security prerogatives succeeded through civilian legislative action without command consent, in this state or close comparators.',
    'If no incremental path exists, the cost-to-fix assessment is prohibitive regardless of the benefit, and reform expectations should be modeled as rupture-dependent rather than gradualist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guardian_arrangement_reversibility, conceptual, 'Whether the arrangement admits peaceful unwinding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.17).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t2, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(july_tr_t2, observed).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(july_tr_t4, observed).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(july_tr_t6, observed).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(july_tr_t8, observed).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(july_tr_t10, observed).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(july_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t2, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 2, 0.59).
narrative_ontology:measurement_basis(july_be_t2, observed).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 4, 0.63).
narrative_ontology:measurement_basis(july_be_t4, observed).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement_basis(july_be_t6, observed).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement_basis(july_be_t8, observed).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(july_be_t10, observed).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement_basis(july_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t2, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement_basis(july_su_t2, observed).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement_basis(july_su_t4, observed).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 6, 0.71).
narrative_ontology:measurement_basis(july_su_t6, observed).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement_basis(july_su_t8, observed).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement_basis(july_su_t10, observed).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement_basis(july_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the charter kernel per the epsilon-invariance principle: 'what the July Charter establishes' is a colloquial label covering three structurally distinct constraints. This file authors the military-custodian instantiation (epsilon 0.72, victim set: parties, students, press, judiciary). The secular-democratic sibling authors the civilian-supremacy instantiation (near-zero extraction from its own seat; the custodian arrangement appears in it as the violation to be remedied). The guided-nationalism sibling authors the identity-grounded instantiation, whose victim set centers on religious minorities and secular dissidents rather than parties and students. The upstream/downstream pressure runs from this reading outward: custodial enforcement constrains what the other two readings can realize in practice, since any civilian-supremacist or identity-revisionist program must first survive the veto this reading codifies. Each story carries its own epsilon, beneficiaries, and victims; they are linked here and in their own files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
