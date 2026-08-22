% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Hyper-Presidential Reading — President as Direct Sovereign, Minimally Constrained by the Legislature
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The 1958 Fifth Republic Constitution is a contested kernel: one
 *   persistent text, three live readings. This file instantiates the
 *   hyper-presidential reading — the president as direct sovereign embodying
 *   the national will, minimally constrained by the legislature. Under this
 *   reading the operative arrangement concentrates agenda control,
 *   dissolution, appointment, and (via Article 16) emergency authority in the
 *   Élysée, while Article 49.3 lets the government adopt legislation without
 *   a vote unless an absolute majority assembles against it. The reading is
 *   exercised, not merely asserted: force-passage migrated from rare
 *   exception to routine instrument (23 uses under a single government,
 *   2022–2024), and the 2024 cycle — a government censured, budgets carried
 *   by special law, appointments disputed — stressed the reading's embodiment
 *   premise directly. Sibling readings (parliamentary_constraint_reading,
 *   cohabitation_equilibrium_reading) are separate constraint files with
 *   their own victim sets, epsilon values, and classifications; the family is
 *   linked through network edges. Claim and metrics are authored
 *   independently: the claimed type is tangled_rope (a genuine coordination
 *   function joined to an asymmetric burden), while the metrics describe
 *   strongly extractive, actively enforced operation — the engine computes
 *   per-seat types from the structural data and measures any divergence from
 *   the claim. KEY AGENTS (by structural relationship): -
 *   incumbent_president: primary beneficiary and agenda-setter
 *   (institutional/arbitrage) — collects decision authority and sets the
 *   reading through practice - french_presidency_institution: institutional
 *   beneficiary (institutional/identity_locked) — accumulates prerogative
 *   precedent with each exercise - prime_minister: enforcement instrument and
 *   contingent payer (powerful/constrained) — deploys force-passage, loses
 *   office on censure - national_assembly_deputies: primary target
 *   (moderate/constrained) — deliberation bypassed by Article 49.3 -
 *   french_senate: secondary target (moderate/constrained) — delay power only
 *   - presidential_majority_deputies: dual-positioned
 *   (moderate/identity_locked) — collects coalition patronage, pays in
 *   hollowed deliberative role - french_electorate: diffuse target
 *   (organized/constrained) — policy adopted without representative consent
 *   between elections - french_trade_unions: excluded objectors
 *   (organized/constrained) — outside the conversation, recourse is the
 *   street - constitutional_council: analytical observer
 *   (institutional/analytical) — reviews but has historically deferred
 *
 * KEY AGENTS:
 *   - incumbent_president: primary beneficiary and agenda-setter (institutional/arbitrage) — collects decision authority, dissolves chambers, appoints and dismisses, sets the reading through practice
 *   - french_presidency_institution: institutional beneficiary (institutional/identity_locked) — the permanent Élysée apparatus, fused with the strong-presidency function it transmits
 *   - prime_minister: enforcement instrument and contingent payer (powerful/constrained) — administers force-passage on the president's behalf and forfeits the office when censure succeeds
 *   - national_assembly_deputies: primary target (moderate/constrained) — debate and amend under a procedure that can erase their vote
 *   - french_senate: secondary target (moderate/constrained) — indirect mandate, delay without final word
 *   - presidential_majority_deputies: dual-positioned (moderate/identity_locked) — advanced by the coalition while stripped of the deliberative function of their office
 *   - french_electorate: diffuse target (organized/constrained) — elects the president directly; between elections, opposed policy is adopted without their representatives' consent
 *   - french_trade_unions: excluded objectors (organized/constrained) — no seat in the Élysée–Matignon channel where decisive bargains occur
 *   - constitutional_council: analytical observer (institutional/analytical) — reviews referrals, historically deferential to the political branches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.68).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Hyper-Presidential Reading — President as Direct Sovereign, Minimally Constrained by the Legislature").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "political/constitutional").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab').
narrative_ontology:cs_kernel_codification('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', fixed_text).
narrative_ontology:cs_authority_grounding('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', lineage).
narrative_ontology:cs_interpretation_layer_present('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab').
narrative_ontology:cs_reading_relation('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', foundational, direct_suffrage_confers_sovereign_mandate).
narrative_ontology:cs_axiom_status(direct_suffrage_confers_sovereign_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', direct_suffrage_confers_sovereign_mandate, conventional).
narrative_ontology:cs_axiom('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', foundational, president_embodies_unified_national_will).
narrative_ontology:cs_axiom_status(president_embodies_unified_national_will, holdable).
narrative_ontology:cs_axiom_grounding('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', president_embodies_unified_national_will, empirically_contingent).
narrative_ontology:cs_axiom('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', secondary, rationalized_parliamentarism_requires_disciplined_chamber).
narrative_ontology:cs_axiom_status(rationalized_parliamentarism_requires_disciplined_chamber, holdable).
narrative_ontology:cs_axiom_grounding('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', rationalized_parliamentarism_requires_disciplined_chamber, instrumental).
narrative_ontology:cs_reference_frame('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', gaullian_direct_sovereign_presidency).
narrative_ontology:cs_drift_state('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', post_2024_fragmented_chamber, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ee12cc10-c7b2-49e4-b1f7-9c1d08e10bab', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, french_presidency_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidential_majority_deputies).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly_deputies).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, french_senate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, french_trade_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, presidential_majority_deputies).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, rationalized_parliamentarism_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_suffrage_mandate_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected directly for a five-year renewable-once term. Appoints and dismisses the prime minister, presides over the council of ministers, commands the armed forces, holds the nuclear authority, may dissolve the National Assembly, and may invoke Article 16 emergency powers. Shapes the constitution's operative meaning through practice: each force-passage tolerated and each emergency episode weathered thickens the precedent available to successors. Can reshape the game itself — dissolution, referendum, appointment timing — so exit from the arrangement's pressures is effectively unrestricted.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).

% The permanent apparatus — secretariat-general, military staff, diplomatic network, legal cell — that outlasts individual incumbents. Collects prerogative interpretation with every exercise of the strong-presidency reading and transmits it to the next occupant. Its self-concept is constituted by the strong-presidency function; the possibility of reverting to a narrowly ceremonial role is not one it can picture from inside, so no exit exists to take.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, french_presidency_institution, beneficiary,
    institutional, generational, identity_locked, national).

% Appointed by and dismissible at will by the president. Deploys Article 49.3 on the government's behalf, staking the cabinet's survival each time: adoption without vote unless an absolute majority assembles against it. When a censure succeeds the officeholder falls, as in December 2024. Runs the arrangement's daily operation and absorbs its political fallout, holding office strictly at the president's pleasure.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister, agenda_setter,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister, payer).

% 577 elected members who debate, amend, and vote legislation — except when the government forces adoption without a vote under Article 49.3, which happened 23 times under one government between 2022 and 2024. A censure motion requires an absolute majority assembled across mutually hostile factions, and toppling a government without an alternative majority risks dissolution and fresh elections. Recent fragmented chambers have made both the censure route and the arithmetic alternative narrow.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly_deputies, payer,
    moderate, biographical, constrained, national).

% Upper chamber with an indirect mandate and delay power only: it can slow texts and force second readings, but the government may cut off the shuttle and impose a final reading. Bears the arrangement's costs as a permanently outranked partner whose consent is never finally required.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, french_senate, payer,
    moderate, biographical, constrained, national).

% Deputies of the president's coalition. Committee chairs, ministerial pipelines, campaign resources, and electoral protection flow through membership; in exchange they vote the government's texts regardless of chamber arithmetic, and force-passage spares them awkward recorded votes. Crossing the president ends advancement, so careers fuse with the coalition — while the same force-passage that protects them hollows out the deliberative function their office exists to perform.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidential_majority_deputies, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, presidential_majority_deputies, payer).

% Elects the president directly every five years and the Assembly every five years on offsetting calendars. Between elections, policy the plurality opposed — pension age, immigration law, budgets — is adopted without their representatives' consent via force-passage. Recourse within the interval is street mobilization and the next election; the 2024 snap election returned a more fragmented chamber rather than a working majority, deepening the deadlock the arrangement then manages by force-passage.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, french_electorate, payer,
    organized, biographical, constrained, national).

% Organized labor has no seat in the Élysée–Matignon channel where reform bargains are struck. When the 2023 pension reform was forced through despite months of strikes and demonstrations uniting the main federations, their objection channel had already been closed inside the legislature; what remained was the street. Would negotiate settlement terms if admitted to the conversation their exclusion defines.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, french_trade_unions, excluded,
    organized, biographical, constrained, national).

% Reviews laws and, since the 2008 reform, Article 16 measures on referral. Has historically declined to police the political branches' use of emergency and force-passage instruments, dismissing the 1961 and 1988 Article 16 referrals on procedural grounds. Takes testimony, weighs doctrine, and its rulings can alter the arrangement's enforcement — a reviewing seat whose deference is itself part of the arrangement's operating condition.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the Fourth Republic's chronic cabinet instability — roughly twenty-two governments in twelve years, most lasting months — by concentrating agenda-setting, confidence, and crisis command in a single directly elected executive; provides unified foreign-policy and nuclear command and a decisive counterweight to fragmented multiparty chambers.
% TRANSFER_FUNCTION: Moves decision authority from the legislative chamber to the president: budget priorities, reform content, and the timing of adoption flow from Élysée preference; deputies' votes are replaced by government-imposed adoption when arithmetic fails; public attention and accountability concentrate on one office.
% ABSENT_VOICES: Trade unions, civil-society associations, and the parliamentary opposition are outside the conversation when Article 49.3 forces adoption; their objections surface as street mobilization and censure attempts requiring an absolute majority across hostile factions. Regional authorities and the overseas territories are likewise unrepresented in the bilateral Élysée–Matignon channel where the decisive bargains occur.
% DISAPPEARANCE_RATIONALE: Government formation would reorganize around negotiated chamber majorities; amendment bargaining and committee compromise would regain decision weight; the presidency would revert toward the arbiter-and-foreign-policy role its rival readings assign it; budget and reform timelines would lengthen, and coalition maintenance would replace force-passage as the central executive skill.
% FOUNDING_PROBLEM: Chronic governmental instability of the Fourth Republic: cabinets averaging months in office, parliamentary fragmentation blocking decisive action, and executive paralysis exposed during the 1958 Algeria crisis.
% FOUNDING_PROBLEM_CORROBORATION: The instability itself is corroborated by historiography and comparative-politics scholarship wholly outside the benefiting parties — standard accounts of the 1958 transition and the Fourth Republic's collapse record the problem independently. What is contested is liveness: the claim that the problem still requires a minimally constrained president is attested mainly by the presidency's own defenders and allied commentators, while constitutional scholars critical of current practice — noting that the 2024 censure produced a functioning caretaker arrangement rather than collapse — argue the original problem is largely solved. No disinterested source attests the live-status claim; that absence is itself signal.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72 at interval end) because decision authority concentrates in one office while the chamber's consent is dispensable under Article 49.3; the series rises from 0.35 at founding as force-passage migrated from exception to routine. Suppression (0.68) is authored as a raw structural property — dissolution threat held over the chamber, the censure-or-passage dilemma, and a historically deferential constitutional review — and is deliberately left unscaled; the engine owns any context scaling, and extractiveness alone is scaled. Theater ratio (0.55) reflects deliberation that continues procedurally while its outcome is predetermined: committee work and amendments persist, but a growing share of the process performs decisions the Élysée has already settled. Accessibility collapse stays moderate (0.45): alternatives survive — censure succeeded in 1962 and again in 2024, the Council reviews measures, the Senate delays, courts and EU law bind, and presidential elections recur every five years. Resistance is substantial (0.62): mass protest waves, successful censures, and fragmented chambers that deny the president a working majority. All three tracked series share one time grid (seven points spanning 1958–2024) so no metric is ever sampled against another's end-state; the plateau around t=33 records the cohabitation years, when alternating majorities temporarily damped the reading's operation without reversing its long drift.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is a working machine the president operates and profits from — coordination achieved, instability solved, crisis command unified. From the chamber seats the same machine reads as enforced passage: deliberation that cannot lose, opposition priced at governmental risk. Majority deputies occupy a third position: protected and advanced by the coalition while stripped of the deliberative function that defines their office. Identity-lock operates on two seats: majority deputies through career path dependence (leaving the coalition ends advancement), and the presidency institution through institutional fusion (its self-concept is the strong-presidency function, so no exit exists to imagine). If the career-fusion frame broke — through proportional representation or a coalition realignment — majority deputies would recompute as ordinary payers and the chamber's resistance capacity would rise sharply. Suppression here is overwhelmingly structural (procedure, dissolution threat, appointment power) rather than internalized, though the normalized vocabulary of 'presidential legitimacy' among deputies supplies a thin internalized layer.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for the president and the presidency institution: the arrangement subsidizes both, with decision authority and precedent accruing to them. Victim declarations map to high directionality: deputies, senators, electors, and unions bear the arrangement's costs under constrained exit. The prime minister receives an explicit override (powerful → 0.45): the automatic chain would leave this unlisted agent on the power-atom fallback, missing his dual position — he administers the force-passage machinery (pulling toward the beneficiary end) yet pays with his office whenever a censure succeeds (pulling toward the target end); his net structural position is near-symmetric. Presidential-majority deputies are declared on the beneficiary side for the patronage they collect, but their payer side (hollowed deliberative role, career fusion) pulls their effective position toward the middle; the scalar derivation approximates this duality and the residual error is acknowledged rather than overridden, because a power-atom override would also drag the opposition deputies' correctly high directionality downward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic cabinet instability — is substantially solved: five-year stable governments are the norm, and the 2024 censure produced a functioning caretaker arrangement rather than collapse. But the status is contested rather than dead: the arrangement's defenders point to post-2024 fragmentation as proof the problem recurs, so the dead-problem-plus-world-rearranges mismatch does not fire; the story records a live dispute over obsolescence instead. Classification discipline cuts both ways. The genuine coordination achievement (instability ended, unified crisis and nuclear command, a decisive counterweight to fragmented chambers) blocks a pure-extraction reading; the measured asymmetry (chamber bypassed, dissent priced at censure risk, gains concentrating in one office) blocks a pure-coordination reading. The tangled_rope claim encodes both halves, and the engine's per-seat computation is what tests them against each seat's actual position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_classification_delta,
    'How would per-seat classification shift if the parliamentary_constraint_reading of the same kernel were instantiated instead of this hyper-presidential reading?',
    'Generate the sibling reading as its own constraint file and compare computed per-seat types, victim sets, and epsilon values across the family.',
    'Under the parliamentary reading the legislature leaves the victim set, base extractiveness falls toward coordination-cost levels, and the computed type moves toward rope; the divergence between the two files is the measure of how much the reading choice, not the text, determines the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_classification_delta, conceptual, 'Committer-frame uncertainty: this constraint is one reading of the fifth_republic_constitution kernel; sibling readings instantiate different constraints.').

omega_variable(
    embodiment_premise_electoral_stress,
    'Does the president''s mandate still track a unified national will, given record abstention in the 2022 runoff and the fragmented chambers returned in 2022 and 2024?',
    'Abstention and fragmentation time series, mandate-perception polling, and comparison of presidential vote share to subsequent legislative support for the president''s program.',
    'Sustained failure of the embodiment premise overrides this reading''s foundational empirical axiom; the legitimacy cover thins and reclassification pressure toward snare increases, since enforcement would then be holding an arrangement whose justifying claim has failed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodiment_premise_electoral_stress, empirical, 'Whether the reading''s core empirical premise survives contact with contemporary electoral data.').

omega_variable(
    article_16_dormancy_status,
    'Is Article 16 a dormant safeguard of last resort or a usable standing instrument, given its single invocation (1961) and recurring speculation during the 2024 budget deadlock?',
    'Constitutional Council jurisprudence on emergency measures, doctrinal analysis of invocation thresholds, and stress-testing against plausible deadlock scenarios.',
    'Activation would spike suppression and drive accessibility collapse sharply upward, pushing the computed type toward snare; confirmed dormancy supports the tangled_rope reading with the emergency clause as unused backstop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_16_dormancy_status, empirical, 'Persistence question over the emergency-power component of the reading.').

omega_variable(
    force_passage_normalization_trajectory,
    'Is the rising reliance on Article 49.3 an enforcement ratchet (each use lowering the threshold for the next) or arithmetic adaptation to hung chambers that any government would face?',
    'Compare force-passage rates under majority governments versus minority governments across the interval, controlling for legislative workload; cross-case comparison with other rationalized-parliament systems.',
    'The ratchet reading supports the suppression-intensification trend in the measurement series and predicts continued escalation; the adaptation reading caps the trend at chamber-arithmetic necessity and softens the drift interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(force_passage_normalization_trajectory, empirical, 'Whether the enforcement trajectory reflects institutional ratchet or environmental necessity.').

omega_variable(
    kernel_framing_text_vs_practice,
    'Is the contested kernel the written 1958 constitutional text, or the Gaullian practice tradition layered above it that gives the text its operative meaning?',
    'Test whether the readings diverge on the text''s semantic content alone or only on accumulated practice (49.3 usage norms, tolerated Article 16 episodes, Council deference patterns); if practice carries the divergence, the practice tradition is the operative kernel.',
    'Under a practice-kernel framing, kernel_codification shifts from fixed_text toward distributed or implicit, drift migrates from textual interpretation into custom, and the authority structure''s accountability for drift changes correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_text_vs_practice, conceptual, 'CS-framing under-determination: two coherent framings of the same kernel produce different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(fift_tr_t0, observed).
narrative_ontology:measurement(fift_tr_t11, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 11, 0.18).
narrative_ontology:measurement_basis(fift_tr_t11, observed).
narrative_ontology:measurement(fift_tr_t22, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 22, 0.22).
narrative_ontology:measurement_basis(fift_tr_t22, observed).
narrative_ontology:measurement(fift_tr_t33, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 33, 0.26).
narrative_ontology:measurement_basis(fift_tr_t33, observed).
narrative_ontology:measurement(fift_tr_t44, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 44, 0.31).
narrative_ontology:measurement_basis(fift_tr_t44, observed).
narrative_ontology:measurement(fift_tr_t55, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 55, 0.43).
narrative_ontology:measurement_basis(fift_tr_t55, observed).
narrative_ontology:measurement(fift_tr_t66, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 66, 0.55).
narrative_ontology:measurement_basis(fift_tr_t66, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fift_be_t0, observed).
narrative_ontology:measurement(fift_be_t11, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 11, 0.42).
narrative_ontology:measurement_basis(fift_be_t11, observed).
narrative_ontology:measurement(fift_be_t22, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 22, 0.46).
narrative_ontology:measurement_basis(fift_be_t22, observed).
narrative_ontology:measurement(fift_be_t33, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 33, 0.47).
narrative_ontology:measurement_basis(fift_be_t33, observed).
narrative_ontology:measurement(fift_be_t44, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 44, 0.53).
narrative_ontology:measurement_basis(fift_be_t44, observed).
narrative_ontology:measurement(fift_be_t55, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 55, 0.63).
narrative_ontology:measurement_basis(fift_be_t55, observed).
narrative_ontology:measurement(fift_be_t66, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 66, 0.72).
narrative_ontology:measurement_basis(fift_be_t66, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(fift_su_t0, observed).
narrative_ontology:measurement(fift_su_t11, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 11, 0.36).
narrative_ontology:measurement_basis(fift_su_t11, observed).
narrative_ontology:measurement(fift_su_t22, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 22, 0.4).
narrative_ontology:measurement_basis(fift_su_t22, observed).
narrative_ontology:measurement(fift_su_t33, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 33, 0.44).
narrative_ontology:measurement_basis(fift_su_t33, observed).
narrative_ontology:measurement(fift_su_t44, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 44, 0.5).
narrative_ontology:measurement_basis(fift_su_t44, observed).
narrative_ontology:measurement(fift_su_t55, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 55, 0.59).
narrative_ontology:measurement_basis(fift_su_t55, observed).
narrative_ontology:measurement(fift_su_t66, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 66, 0.68).
narrative_ontology:measurement_basis(fift_su_t66, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'the Fifth Republic presidency' is one colloquial label covering three structurally distinct constraints. This file (hyper_presidential_reading) carries the high-extraction variant — the legislature in the victim set, force-passage as a standing instrument. The parliamentary_constraint_reading carries a low-extraction variant (authorization required; chamber as partner) and the cohabitation_equilibrium_reading a conditional variant (authority allocation varies with chamber arithmetic). Each has its own epsilon, beneficiaries, and victims. Upstream, this reading's accumulated precedents (routine 49.3 usage, tolerated Article 16 episodes, Council deference) supply the legitimacy raw material the sibling readings must argue against, so influence edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
