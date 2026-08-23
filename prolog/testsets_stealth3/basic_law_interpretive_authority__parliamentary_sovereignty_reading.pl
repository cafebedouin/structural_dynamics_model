% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty over Constitutional Interpretation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Under this arrangement the elected legislature holds final interpretive
 *   authority over the basic law: courts interpret, but their conclusions
 *   yield whenever a sitting majority passes an override, re-enacts a
 *   provision, or amends the text. The arrangement is justified by democratic
 *   mandate and representative accountability — the people's representatives,
 *   not unelected professionals, decide what the constitution requires. The
 *   structure carries a genuine coordination function (terminal resolution of
 *   inter-branch disagreement) alongside asymmetric costs borne by the
 *   judiciary and by rights-minorities whose protections are the recurring
 *   object of override, sustained by active enforcement (override procedure,
 *   appointment politics, procedural control of interpretive disputes). KEY
 *   AGENTS (by structural relationship): national_legislature — primary
 *   beneficiary and agenda-setter (institutional/arbitrage), holds and
 *   administers finality; governing_majority_coalition — secondary
 *   beneficiary (powerful/constrained), exercises finality for policy ends;
 *   constitutional_judiciary — primary target (institutional/trapped),
 *   rulings reversible at a stroke; rights_minorities — primary target
 *   (powerless/constrained), protections contingent on majority restraint;
 *   opposition_parliamentarians — secondary cost-bearers
 *   (organized/mobile-through-office); national_electorate — nominal
 *   principal, dual-positioned (organized/constrained);
 *   civil_society_constitutional_groups — litigating resisters
 *   (organized/constrained); legal_academia — analytical observer
 *   (moderate/analytical). This file instantiates ONE reading of the kernel
 *   basic_law_interpretive_authority; the judicial-supremacy and
 *   popular-constitutionalism readings are separate constraints with their
 *   own epsilon values, beneficiary/victim maps, and classifications. Claim
 *   and metrics are authored independently: the claimed type states what I
 *   judge structurally true; the metric values state what I judge
 *   descriptively true of the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - national_legislature: primary beneficiary and agenda-setter (institutional/arbitrage) — possesses and administers final interpretive authority, controls override procedure
 *   - governing_majority_coalition: secondary beneficiary (powerful/constrained) — converts finality into governing discretion for the duration of its majority
 *   - constitutional_judiciary: primary target (institutional/trapped) — produces interpretations that bind everyone except the body that outranks them; tenure offers no exit from the hierarchy
 *   - rights_minorities: primary target (powerless/constrained) — bear override costs with no vote, no superior forum, and recourse limited to lobbying the deciding majority
 *   - opposition_parliamentarians: secondary cost-bearers (organized/mobile) — lose interpretive contests today and inherit adverse precedents tomorrow; exit runs through winning office
 *   - national_electorate: dual-positioned principal (organized/constrained) — source of the mandate and bearer of instability and minority-harm costs
 *   - civil_society_constitutional_groups: litigating resisters (organized/constrained) — monitoring and re-defense workload multiplied by each override
 *   - legal_academia: analytical observer (moderate/analytical) — supplies the comparative arguments all factions deploy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty over Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '1beb1ba4-7a8d-4e28-9519-83286744f9b5').
narrative_ontology:cs_kernel_codification('1beb1ba4-7a8d-4e28-9519-83286744f9b5', formalized).
narrative_ontology:cs_authority_grounding('1beb1ba4-7a8d-4e28-9519-83286744f9b5', lineage).
narrative_ontology:cs_interpretation_layer_present('1beb1ba4-7a8d-4e28-9519-83286744f9b5').
narrative_ontology:cs_reading_relation('1beb1ba4-7a8d-4e28-9519-83286744f9b5', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1beb1ba4-7a8d-4e28-9519-83286744f9b5', basic_law_interpretive_authority__popular_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('1beb1ba4-7a8d-4e28-9519-83286744f9b5', foundational, interpretive_authority_follows_democratic_mandate).
narrative_ontology:cs_axiom_status(interpretive_authority_follows_democratic_mandate, holdable).
narrative_ontology:cs_axiom_grounding('1beb1ba4-7a8d-4e28-9519-83286744f9b5', interpretive_authority_follows_democratic_mandate, deontological).
narrative_ontology:cs_axiom('1beb1ba4-7a8d-4e28-9519-83286744f9b5', secondary, judicial_veto_is_democratic_deficit).
narrative_ontology:cs_axiom_status(judicial_veto_is_democratic_deficit, holdable).
narrative_ontology:cs_axiom_grounding('1beb1ba4-7a8d-4e28-9519-83286744f9b5', judicial_veto_is_democratic_deficit, conventional).
narrative_ontology:cs_reference_frame('1beb1ba4-7a8d-4e28-9519-83286744f9b5', founding_democratic_mandate_settlement).
narrative_ontology:cs_drift_state('1beb1ba4-7a8d-4e28-9519-83286744f9b5', contemporary_partisan_override_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1beb1ba4-7a8d-4e28-9519-83286744f9b5', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_electorate).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parliamentarians).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, civil_society_constitutional_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_electorate).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, democratic_mandate_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, representative_accountability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The elected assembly that writes, amends, and — under this arrangement — has the last word on what the basic law means. When the constitutional court issues an interpretation the sitting majority dislikes, it can pass an override statute, re-enact the provision, or amend the text, and its version stands. It frames the public justification ('the people's representatives decide'), controls the calendar and procedure of interpretive disputes, and holds the prerogative of finality as an institutional possession. Its alternatives are broad: it can cede authority by statute, expand it, or redraw the division of labor with the courts at will.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature, beneficiary).

% The party or coalition currently holding a working majority. It converts final interpretive authority into governing discretion: programs the constitutional court would block can be enacted and defended by override. Its command of the mechanism lasts exactly as long as its majority; after an election loss it inherits whatever precedents it set, now available to its opponents.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_coalition, beneficiary,
    powerful, immediate, constrained, national).

% Tenured judges of the constitutional court who produce authoritative interpretations of the basic law. Their rulings bind everyone except the institution that outranks them under this arrangement; an override reverses their work at a stroke. They cannot resign their way out of the hierarchy without abandoning their office and lifework, and repeated overrides erode the perceived weight of their judgments regardless of the quality of their reasoning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_judiciary, payer,
    institutional, generational, trapped, national).

% Ethnic, religious, linguistic, and other minorities whose protections against majority action depend on enforceable constitutional limits. When the legislature overrides a ruling that protected them, they have no vote in the decision and no forum above the deciding body; their recourse reduces to lobbying the very majority that acted against them, litigation the override pre-empts, or emigration. Their exposure recurs with each new majority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, generational, constrained, national).

% Legislators out of power. They bear the arrangement twice: their preferred interpretations lose, and the precedents today's majority sets — expanded override procedures, narrowed review — are inherited by whoever wins next. Their realistic path out runs through winning office, at which point they acquire the same authority they criticized; this prospect disciplines how loudly they object.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parliamentarians, payer,
    organized, biographical, mobile, national).

% The voting public in whose name final authority is claimed. It gains a clear chain of accountability: interpretive disputes end in decisions traceable to officials voters can reward or punish. It also absorbs the costs — constitutional whiplash when alternating majorities reverse each other's overrides, and the material and moral costs when majorities turn the mechanism on fellow citizens. Individual voters cannot opt out of either side of the ledger.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_electorate, payer).

% Advocacy organizations, bar associations, and watchdog groups that litigate, monitor, and publicize interpretive conflicts. Each override multiplies their workload: provisions must be re-defended, coalitions rebuilt, campaigns refought. Their access runs through the courts whose conclusions the legislature can discard, and their monitoring is the main early-warning system the arrangement's opponents possess.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, civil_society_constitutional_groups, payer,
    organized, generational, constrained, national).

% Constitutional scholars inside and outside the country who analyze the arrangement, compare it with rival designs, and supply arguments each faction deploys. They bear no direct cost and collect no direct benefit; their standing depends on describing the structure accurately enough that partisans on all sides keep citing them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, legal_academia, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, national_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the terminal-authority problem: when institutions disagree about what the basic law requires, the disagreement ends in a single authoritative decision rather than persisting as inter-branch deadlock. It also attaches constitutional interpretation to the electoral cycle, so interpretive errors are correctable by voters rather than only by the interpreting profession.
% TRANSFER_FUNCTION: Moves final interpretive authority — and the veto power that travels with it — from courts and extra-parliamentary actors to the elected legislature; converts that authority into policy discretion for whichever coalition holds the majority; and places the costs of lost protection on rights-minorities and on the judiciary's institutional standing.
% ABSENT_VOICES: Rights-minorities appear only as petitioners, never as holders of a blocking vote — the people whose protections are the typical object of override have no seat at the override decision. Future generations and future oppositions cannot vote on precedents entrenched today. Courts deliberate but only in a subordinate chamber whose conclusions the legislature may discard. Extra-parliamentary popular-constitutionalist actors stand wholly outside the formal channel.
% DISAPPEARANCE_RATIONALE: If the legislature's final authority vanished overnight, no terminal resolver of constitutional disagreement would remain: either courts would assume finality by default, inverting the beneficiary and cost-bearing map, or interpretation would fragment into open-ended contestation with no stopping rule. Governing majorities would lose the ability to enact programs courts oppose; minorities would gain a protective forum they currently lack; the electoral accountability chain for interpretive disputes would break.
% FOUNDING_PROBLEM: At the settlement, the drafters faced chronic inter-branch deadlock over fundamental law and the living memory of unelected authority binding elected governments. They needed a rule for who decides when institutions disagree, and they wanted ultimate legal authority answerable to voters rather than to unelected professionals.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholarship outside the benefiting parties attests the terminal-authority problem is live: every stable constitutional order resolves it somewhere (courts, legislature, or distributed contest), and orders lacking any resolution rule exhibit chronic inter-branch crisis. Judicial submissions, bar-association positions, and cross-national institutional-design literature corroborate the problem's persistence while disputing this reading's particular solution; no corroborating source outside the beneficiary set claims the problem is dead.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end): the arrangement transfers real, concentrated authority to the legislature and imposes real, recurring costs on identifiable seats, but it also delivers a coordination good every constitutional order needs somewhere, and the reading's own lights credit the accountability chain. Suppression (0.58) is structural, not internalized: rival finality claims are procedurally foreclosed (courts cannot bind the overriding body; extra-parliamentary channels sit outside the formal process), and the mechanism persists through ordinary legislative procedure rather than through belief management — though a growing share of maintenance is rhetorical (see theater). Theater ratio (0.38): the 'will of the people' justification is increasingly performed by party leadership under low-information conditions rather than produced by deliberation, while the terminal-resolution function remains real. Accessibility collapse (0.62): once finality is understood, treating judicial rulings as authoritative collapses as an option, yet contestation channels persist — alternatives narrow but do not vanish. Resistance (0.66): courts assert their conclusions' weight, minorities litigate and mobilize, civil society campaigns, and electoral backlashes occasionally reverse overrides. Identity-lock dynamics bind the judiciary: judges' professional identity is fused with the guardianship role, so override attacks not merely their outputs but their self-conception; if that frame broke (judges reframing as subordinate statutory interpreters), the judiciary's seat would experience the arrangement far less harshly. Electoral micro-cycles ride inside the decadal drift — override activity spikes after elections install fresh majorities and quiets as courts adapt — but the oscillation is a side effect of the electoral calendar, not itself the extraction mechanism. The measurement series run on one shared time grid (t=0..60, decade steps) with every tracked metric authored at every point. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the legislature's chair the arrangement is the constitution working as designed: disputes terminate, voters stay sovereign, and override is accountability in action. From the judiciary's chair it is professional subordination — a lifetime of reasoning that a single vote can erase. From the minorities' chair it is protection contingent on the goodwill of potential adversaries. From the governing majority's chair it is a perishable asset, held only until the next election and inheritable by opponents. The engine computes these per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature sits nearest the beneficiary end (collects the finality prerogative, controls the rules, holds arbitrage-grade restructuring options). The governing majority collects the exercise-value and sits low-d but with constrained exit — its benefit evaporates on losing office. The judiciary sits near the full-target end: victim-declared, trapped exit, and identity fusion amplifying the felt cost. Rights-minorities sit at or near the full-target end: powerless, constrained, recurrently targeted. Opposition parliamentarians are victim-declared but their mobility-through-office damps effective extraction below the trapped seats. The electorate is genuinely dual-positioned — mandate-source benefit against instability and minority-harm costs — landing near symmetric. Civil-society groups bear enforcement-contact costs with constrained exit; academia is analytical and directionally neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — who decides when institutions disagree about fundamental law — remains live: every constitutional order must resolve it somewhere, so this is not a resolved-mandatrophy case and the arrangement is not a vestige. The classification matters in both directions. Reading the arrangement as pure coordination (its proponents' framing: 'this is just democracy') would erase the asymmetric costs the judiciary and minorities demonstrably bear through the same structure that resolves deadlock. Reading it as pure extraction (its critics' framing: 'majoritarian tyranny') would erase the genuine terminal-resolution function whose absence produces observable inter-branch crisis elsewhere. The tangled-rope structure holds both facts: coordination delivered, extraction layered on, enforcement required to sustain the asymmetry. The temporal series show extraction accumulating (0.26 to 0.52) and enforcement hardening (0.34 to 0.58) over the interval — a rent-seeking layer thickening on a live coordination function. If the ratchet dynamic in the omegas resolves toward entrenchment (overrides plus appointment politics becoming self-perpetuating), the trajectory bends toward the snare end; if alternation in government reliably reverses overrides, the structure stabilizes as a durable hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment_contest,
    'Which reading of basic_law_interpretive_authority should govern — is the assignment of final interpretive authority resolvable by comparative institutional performance, or is it a foundational commitment each polity must make?',
    'Comparative natural experiments: jurisdictions that adopted or removed override clauses, jurisdictions that shifted between court-finality and legislature-finality, and measured outcomes on rights protection, deadlock frequency, and accountability. This story is one reading of the kernel; the sibling readings are separate constraints whose adoption would replace this file''s entire structural map.',
    'Adopting the judicial supremacy reading inverts the beneficiary/victim structure (judiciary into the beneficiary set, legislative majorities into constrained cost-bearing); adopting the popular constitutionalism reading dissolves the single-terminal-seat structure into distributed contestation with no agenda-setter seat at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_assignment_contest, conceptual, 'Kernel-level contest over which institution, if any, holds final interpretive authority; this file authors only the parliamentary sovereignty instantiation.').

omega_variable(
    mandate_authenticity,
    'Do override decisions actually track voter mandates, or do they track party-leadership preference formed under low-information, high-discipline conditions?',
    'District-level congruence studies comparing override votes with constituent opinion; audits of deliberation quality in override proceedings; survey evidence on voter awareness of specific interpretive conflicts.',
    'If mandates are largely theatrical, the theater ratio is understated and the coordination justification degrades toward cover-story territory; if override votes genuinely track informed constituent preference, the current theater ratio stands and the accountability chain is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_authenticity, empirical, 'Authenticity of the democratic mandate that grounds the legislature''s interpretive claim.').

omega_variable(
    override_target_distribution,
    'What share of legislative overrides actually strip minority protections versus resolving routine statutory or procedural conflict?',
    'Systematic coding of every override instance across the interval by target class (minority-protective rulings, inter-branch boundary disputes, technical corrections), with severity weighting.',
    'A high minority-target share confirms the victim set as load-bearing and sustains the hybrid classification; a near-zero share would indicate the extraction component is marginal and support movement toward the pure-coordination end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_target_distribution, empirical, 'Empirical distribution of override targets determining whether minority exposure is central or incidental.').

omega_variable(
    ratchet_or_reversibility,
    'Is legislative interpretive supremacy self-correcting — losers reverse overrides by winning elections — or ratcheting, with each override plus aligned appointments making reversal progressively harder?',
    'Survival analysis of override durability across alternations in government; tracking of appointment composition and procedural entrenchment following each override episode.',
    'Ratchet dynamics bend the trajectory toward the extraction-dominant end of the spectrum; reliable reversibility stabilizes the structure as a durable hybrid with bounded asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_or_reversibility, empirical, 'Whether the arrangement''s extraction component compounds or self-corrects through electoral turnover.').

omega_variable(
    gridlock_cost_allocation,
    'Who ultimately bears the coordination costs of maintaining legislative finality — the judiciary (repeated re-litigation and process burden), the electorate (instability across alternations), or minorities (exposure during unresolved episodes)?',
    'Cost-incidence tracing across override episodes: court dockets and delay statistics, electoral-cycle disruption measures, and minority-outcome indicators during contested periods.',
    'Shifts effective extraction across seats without necessarily changing the aggregate — reallocating which seat computes the harshest experience of the same structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_cost_allocation, conceptual, 'Distribution of the arrangement''s coordination costs across the judiciary, electorate, and minority seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the constitution' covers three structurally distinct arrangements, decomposed per the epsilon-invariance principle into three stories sharing the kernel basic_law_interpretive_authority. This file instantiates the parliamentary sovereignty reading: the standing arrangement is legislature-finality, with the legislature in the beneficiary set, the judiciary and rights-minorities bearing override costs, and active enforcement sustaining the hierarchy. The judicial supremacy reading instantiates court-finality (inverted beneficiary and cost-bearing map, expertise-grounded authority); the popular constitutionalism reading instantiates distributed contestation (no terminal seat, no agenda-setter). Each carries its own epsilon, stakeholders, and classification; citation traffic among the readings — each faction invokes the others' failure cases as evidence — runs through these network edges rather than through any single story hedging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
