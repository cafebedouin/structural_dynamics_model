% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Fifth Republic Constitution - Hyper-Presidential Reading: President as Direct Sovereign
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the hyper_presidential_reading of the Fifth
 *   Republic constitutional kernel: the president of the Republic as direct
 *   sovereign, elected by universal suffrage, embodying the national will,
 *   governing with minimal legislative constraint. The standing arrangement
 *   this story measures is that practice as it has operated since the 1962
 *   direct-election referendum gave the reading its full form: government
 *   agenda control and bill passage through Article 49.3 (adoption without
 *   vote absent a censure majority), unilateral dissolution of the Assembly
 *   (Article 12), a presidential monopoly on referendums (Article 11), a
 *   reserved domain of foreign policy and defense, and a prime minister
 *   appointed and dismissed at the president's discretion who absorbs censure
 *   motions. Epsilon's referent is this standing arrangement as this reading
 *   holds it - never the parliamentary or cohabitation arrangements the
 *   sibling readings would put in its place. The fixed 1958 text is a single
 *   kernel that decomposes into three structurally distinct constraints -
 *   this reading, the parliamentary_constraint_reading, and the
 *   cohabitation_equilibrium_reading - each authored as its own story with
 *   its own epsilon, its own victim set, and its own type, linked through
 *   network.affects_constraints; conflating the three under the label 'the
 *   Fifth Republic constitution' is the colloquial ambiguity the family
 *   decomposition removes. Claim and metrics are independent authored facts:
 *   I claim tangled_rope because the arrangement genuinely solved the Fourth
 *   Republic's executive-deadlock coordination problem while simultaneously
 *   transferring legislative authority from the Assembly to the presidency
 *   and routing accountability onto a dismissable government; the metrics
 *   describe that dual operation as I assess it, without being tuned toward
 *   any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - incumbent_president: agenda-setter and principal beneficiary (institutional/arbitrage) - holds dissolution, referendum, and no-vote-passage levers; mandate untouched by censure
 *   - presidency_as_institution: structural beneficiary (institutional/arbitrage; generational horizon) - accumulates precedent across incumbents and survived three divided-executive periods
 *   - national_assembly: primary target (institutional/constrained) - deliberation overridable, chamber dissolvable
 *   - parliamentary_opposition: target (organized/constrained) - censure is its only binding lever and it risks its own seats
 *   - french_electorate: dual beneficiary and payer (moderate/mobile) - presidential ballot is the sovereign act; Assembly ballot subordinate
 *   - prime_minister_government: delegated beneficiary and censure absorber (institutional/constrained)
 *   - constitutional_council: analytical observer (institutional/analytical) - historically deferential on presidential prerogatives
 *   - citizen_initiative_advocates: excluded voice (organized/constrained) - no entry point; referendum is a presidential monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.74).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution - Hyper-Presidential Reading: President as Direct Sovereign").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '9320b7b1-59d7-4d7d-bb36-f7404b6b5a93').
narrative_ontology:cs_kernel_codification('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', fixed_text).
narrative_ontology:cs_authority_grounding('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', lineage).
narrative_ontology:cs_interpretation_layer_present('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93').
narrative_ontology:cs_reading_relation('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', fifth_republic_constitution__parliamentary_constraint_reading, influences).
narrative_ontology:cs_reading_relation('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', foundational, president_embodies_national_sovereignty).
narrative_ontology:cs_axiom_status(president_embodies_national_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', president_embodies_national_sovereignty, deontological).
narrative_ontology:cs_axiom('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', foundational, executive_stability_requires_presidential_primacy).
narrative_ontology:cs_axiom_status(executive_stability_requires_presidential_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', executive_stability_requires_presidential_primacy, instrumental).
narrative_ontology:cs_reference_frame('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', gaullist_direct_sovereign_presidency).
narrative_ontology:cs_drift_state('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', post_2022_relative_majority, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9320b7b1-59d7-4d7d-bb36-f7404b6b5a93', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, parliamentary_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_universal_suffrage_legitimacy).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, gaullist_presidential_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, domaine_reserve_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected directly for a five-year term. Sets the government's program, appoints and dismisses the prime minister, can force government bills through the Assembly without a vote, dissolve the Assembly, call referendums, and claims foreign policy and defense as a personal domain. Censure motions reach the government, not the president's mandate.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary).

% The office itself, persisting across incumbents. Each use of the no-vote passage, dissolution, and referendum powers normalizes the next; the office kept its primacy through three periods of divided executive and emerged from each with its prerogatives intact. Its horizon outlasts any one president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, arbitrage, national).

% The 577-seat chamber elected for five years and dissolvable at the president's discretion. Debates government bills whose adoption can be forced without a vote; amendments are frequently set aside. Its one binding lever, the censure motion, requires an absolute majority and, if it fails, leaves its signatories exposed to dissolution.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    institutional, biographical, constrained, national).

% Parties holding Assembly seats outside the presidential camp. Their legislative proposals rarely reach the floor without government assent; their principal instrument is the censure motion, which demands cross-group coordination. The two-round electoral system favors candidates backed by a presidential majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, parliamentary_opposition, payer,
    organized, biographical, constrained, national).

% Votes the president directly every five years; under this arrangement that ballot is the act of national sovereignty. Separately elects the Assembly, but when the government bypasses the chamber the majority the voters chose does not govern. Their next recourse is the following presidential or legislative election.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, french_electorate, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, french_electorate, payer).

% Appointed by and serving at the president's discretion. Carries the president's program through the Assembly, deploys the no-vote passage on the president's behalf, and absorbs censure motions: the government can fall while the president's mandate stands. Tenure is measured in the president's confidence.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government, beneficiary,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government, payer).

% Nine-member council reviewing statutes before promulgation and elections afterward. Has historically declined to police the president's core prerogatives: it validated the 1962 direct-election referendum and did not constrain the 1961 emergency-powers invocation. A third of its members are appointed by the president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Movements and proposals for a citizens' initiative referendum that would let a signature threshold trigger a national vote or legislative consideration. Under the current arrangement the referendum can be called only by the president and legislative initiative sits with the government; the proposals have no institutional entry point.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizen_initiative_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the Fourth Republic's executive deadlock: a fragmented multiparty Assembly could not sustain governments, so the arrangement concentrates agenda-setting, dissolution, and emergency authority in a directly elected president so that a single national decision-maker exists and executive tenure survives Assembly fragmentation.
% TRANSFER_FUNCTION: Moves legislative decision authority from the elected Assembly to the president (agenda control, no-vote passage, ordinance practice); moves political accountability downward onto the prime minister, who absorbs censure motions while the president's mandate remains untouched; reserves the referendum monopoly to the president.
% ABSENT_VOICES: Citizens' initiative referendum advocates have no institutional entry point: the referendum is a presidential monopoly and legislative initiative sits with the government. Voters whose Assembly majority opposes the president see their legislative choice overridden by the no-vote passage without their presidential ballot being at stake. Local and regional authorities are administered through prefects answerable to the executive rather than to elected councils.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading ceased to govern practice overnight - no no-vote passage, dissolution held as a last resort, foreign policy negotiated with parliament - coalition-building would return to the Assembly, governments would fall and form through parliamentary arithmetic, policy speed would drop, and the presidency would shrink toward the cohabitation-equilibrium or parliamentary-constraint configuration. Every seat's situation changes: the Assembly regains binding deliberation, the opposition regains legislative leverage, the electorate's parliamentary vote regains force, and the presidency loses its primacy.
% FOUNDING_PROBLEM: The Fourth Republic's chronic instability: between 1947 and 1958 roughly two dozen governments fell, the Assembly fragmented into shifting coalitions, and the state proved unable to decide decisively - culminating in the 1958 Algerian crisis that brought de Gaulle to power with a mandate to write a new constitution.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real is corroborated outside the benefiting parties by constitutional historians of the Fourth Republic and by the 1958 cross-party consensus that produced the text. That it remains live in its original form is attested only by the presidency and the Gaullist tradition; parliamentary-reading advocates and comparative scholars point to the 2022-2024 fragmented-Assembly record of censure and paralysis as evidence that the arrangement now co-produces the instability it was built to end. No neutral party attests liveness in its 1958 form.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.74 at interval end) because Article 49.3 decouples legislative outcomes from Assembly deliberation: a government bill passes without a vote unless an absolute majority censes - a bar cleared only once in six decades (December 2024), by a vote that risked the signatories' own seats through dissolution. Suppression is high (0.72) because the reading's persistence rests on actively maintained machinery rather than participant preference: the standing dissolution threat, the censure-bar arithmetic, the 2000 electoral synchronization that narrowed the cohabitation exit, and a Constitutional Council historically deferential on presidential prerogatives (the 1961 emergency-powers invocation, the 1962 referendum). Suppression is authored as raw structure and is not scaled; only extractiveness is scaled by the engine's directionality and scope computation. Theater_ratio 0.55: Assembly debate persists and is increasingly non-binding - deliberation staged around outcomes the no-vote passage has already fixed. Accessibility_collapse 0.45: the alternatives remain live - cohabitation was actually practiced three times and the parliamentary reading retains advocates - so the reading does not collapse them, though the quinquennat structurally narrowed one exit. Resistance 0.62: censure motions, mass protest waves (1995, 2006, 2018-19, 2023), and the Sixth Republic movement are real and occasionally effective. The three series share one nine-point grid spanning the reading's consolidation (1962-1982), the cohabitation valleys (1987-2000), and the post-quinquennat restoration (2008-2024). The valley points measure this reading's own enforcement idling under a divided executive - not a different constraint; where the boundary between 'the reading idles' and 'a sibling reading governs' sits is exactly what the operative-reading omega carries. suppression_requirement is authored deliberately because the story tracks enforcement-capacity change: the machinery idles under cohabitation, is structurally reinforced by electoral synchronization, and after 2022 is forced to work harder as the presidential majority disappears - rising no-vote reliance and censure bargaining hold the reading in place.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the Elysee the arrangement is the constitution working as designed: a directly elected sovereign cutting through Assembly fragmentation, with censure routed to a subordinate government so the national mandate is never held hostage. From the Assembly benches the same structure is bypass and nullification: deliberation that does not bind, amendments discarded, a chamber dissolvable at the president's discretion. Two actors of nominally identical constitutional rank (president, Assembly) diverge because of constraint-specific exit asymmetry - the president holds dissolution, referendum, and no-vote-passage levers; the Assembly's single lever, censure, requires an absolute majority across fractious groups and threatens its own existence through dissolution. The prime minister's seat is genuinely dual: delegated executive power flows to him, and censure risk flows onto him, insulating the president. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the incumbent president and the presidency-as-institution near the beneficiary pole (d near 0): the arrangement concentrates agenda control and legislative passage on them, and they hold arbitrage-grade levers inside it. Victim declarations place the national_assembly and parliamentary_opposition near the target pole (d near 1): they bear the transfer of legislative authority with constrained exit. The french_electorate is declared a beneficiary - their direct ballot is the reading's legitimacy source and they receive a decisive, periodically accountable executive - but is structurally near-symmetric: their Assembly votes are nullified under no-vote governance. The directionality override sets their power atom's d to 0.5 because the derivation from the beneficiary declaration alone cannot see the dual position that the stakeholder's secondary payer role records; the electorate is the story's only moderate-power seat, so the override is unambiguous. The prime minister is a delegated beneficiary and censure absorber - benefits flow through him, costs land on him. The Constitutional Council is an observer seat: it neither collects nor pays, though its deference is part of the enforcement machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. First, the mountain mislabel: the reading is habitually presented as constitutional nature - 'the Fifth Republic is presidential; that is what it is' - which would render extraction invisible and immunize it as natural law. The beneficiaries are identifiable and the arrangement requires active enforcement, so the natural-law framing is false-summit cover; the constructed-versus-designed omega carries that ambiguity. Second, the snare mislabel: reading the arrangement as pure extraction would erase the genuine coordination function - the Fourth Republic's deadlock was real, roughly two dozen governments fell in twelve years, and a single decisive executive was a real solution to it. Tangled_rope holds both truths: real coordination, asymmetric transfer, active enforcement. The mandatrophy question is live rather than resolved: the founding problem (executive instability) is contested - the arrangement's own 2022-2024 record of censure and paralysis is cited as evidence that the cure now co-produces the disease. If the founding problem is dead and the extraction persists, the trajectory bends toward snare; the cohabitation-recurrence omega tracks that bend.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_reading_ambiguity,
    'This constraint is one reading of kernel fifth_republic_constitution. Which structural elements do the sibling readings (parliamentary_constraint_reading, cohabitation_equilibrium_reading) relocate, and under what observable conditions does each reading govern practice?',
    'Track reading-governance indicators across presidencies: Article 49.3 invocation rates, censure motion outcomes, dissolution usage, Constitutional Council posture, and cohabitation episodes; the reading that predicts practice is operative in that period.',
    'Under the parliamentary reading the legislature exits the victim set and the presidency''s extraction collapses toward coordination cost; under the cohabitation reading extraction becomes episodic and negotiated. This story''s epsilon holds only while the hyper-presidential reading governs practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_reading_ambiguity, conceptual, 'Kernel-contest location: victim-set membership, mandate primacy, and extraction temporality differ across the three readings of the fixed 1958 text.').

omega_variable(
    constructed_vs_designed_reading,
    'Is the hyper-presidential reading the designed meaning of the 1958 text, as its holders claim via the Founder''s intent and the 1962 ratification, or a constructed accumulation of referendum precedent and practice whose principal beneficiaries are incumbents?',
    'Founding-intent scholarship comparing the 1958 drafting records with the 1962 reform''s ratification path; comparative semi-presidential analysis of whether direct election entails the practiced prerogatives (no-vote reliance, reserved domain, referendum monopoly).',
    'If constructed, the reading''s presentation as constitutional nature is cover for incumbent benefit - false-summit structure inside a tangled_rope; if designed, part of the measured extraction is the intended price of the design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_designed_reading, conceptual, 'Naturalness ambiguity: designed constitution versus accumulated incumbent-serving precedent.').

omega_variable(
    cohabitation_recurrence,
    'Will cohabitation recur under the post-2022 fragmented-Assembly equilibrium, making the reading''s extraction episodic, or has electoral synchronization plus persistent fragmentation produced a new steady state?',
    'Observe the 2027 electoral cycle and subsequent legislative formations: whether any presidential majority remains obtainable, and whether presidents again share power with opposition governments.',
    'Recurring cohabitation keeps the constraint a cyclical tangled_rope with valley-shaped extraction; a permanent no-majority steady state pushes the trajectory toward snare - extraction persisting after the stabilizing function fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_recurrence, empirical, 'Whether the cohabitation discipline on extraction returns or the reading hardens.').

omega_variable(
    article16_latent_suppression,
    'How much standing suppression does the dormant Article 16 emergency-power precedent contribute between invocations - does its credible availability, established by the 1961 use, discipline the legislature continuously?',
    'Comparative analysis of dormant emergency powers; legislative behavior in periods when the executive signals crisis conditions; Constitutional Council doctrine on Article 16 limits.',
    'If the latent threat contributes materially, base suppression understates the reading''s standing coercive force; if inert, suppression is carried almost entirely by the ordinary machinery (dissolution threat, censure arithmetic).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article16_latent_suppression, empirical, 'Latent emergency powers as standing suppression versus inert text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1962, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement_basis(fift_tr_t1962, observed).
narrative_ontology:measurement(fift_tr_t1972, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1972, 0.35).
narrative_ontology:measurement_basis(fift_tr_t1972, observed).
narrative_ontology:measurement(fift_tr_t1982, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1982, 0.4).
narrative_ontology:measurement_basis(fift_tr_t1982, observed).
narrative_ontology:measurement(fift_tr_t1987, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement_basis(fift_tr_t1987, observed).
narrative_ontology:measurement(fift_tr_t1994, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement_basis(fift_tr_t1994, observed).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(fift_tr_t2000, observed).
narrative_ontology:measurement(fift_tr_t2008, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2008, 0.5).
narrative_ontology:measurement_basis(fift_tr_t2008, observed).
narrative_ontology:measurement(fift_tr_t2018, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2018, 0.52).
narrative_ontology:measurement_basis(fift_tr_t2018, observed).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.55).
narrative_ontology:measurement_basis(fift_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t1962, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement_basis(fift_be_t1962, observed).
narrative_ontology:measurement(fift_be_t1972, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1972, 0.68).
narrative_ontology:measurement_basis(fift_be_t1972, observed).
narrative_ontology:measurement(fift_be_t1982, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1982, 0.7).
narrative_ontology:measurement_basis(fift_be_t1982, observed).
narrative_ontology:measurement(fift_be_t1987, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1987, 0.5).
narrative_ontology:measurement_basis(fift_be_t1987, observed).
narrative_ontology:measurement(fift_be_t1994, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1994, 0.48).
narrative_ontology:measurement_basis(fift_be_t1994, observed).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(fift_be_t2000, observed).
narrative_ontology:measurement(fift_be_t2008, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement_basis(fift_be_t2008, observed).
narrative_ontology:measurement(fift_be_t2018, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement_basis(fift_be_t2018, observed).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.74).
narrative_ontology:measurement_basis(fift_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1962, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1962, 0.52).
narrative_ontology:measurement_basis(fift_su_t1962, observed).
narrative_ontology:measurement(fift_su_t1972, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1972, 0.58).
narrative_ontology:measurement_basis(fift_su_t1972, observed).
narrative_ontology:measurement(fift_su_t1982, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1982, 0.6).
narrative_ontology:measurement_basis(fift_su_t1982, observed).
narrative_ontology:measurement(fift_su_t1987, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1987, 0.38).
narrative_ontology:measurement_basis(fift_su_t1987, observed).
narrative_ontology:measurement(fift_su_t1994, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1994, 0.36).
narrative_ontology:measurement_basis(fift_su_t1994, observed).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(fift_su_t2000, observed).
narrative_ontology:measurement(fift_su_t2008, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement_basis(fift_su_t2008, observed).
narrative_ontology:measurement(fift_su_t2018, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2018, 0.66).
narrative_ontology:measurement_basis(fift_su_t2018, observed).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(fift_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The fixed 1958 constitutional text is a single kernel that decomposes into three structurally distinct constraints: this hyper-presidential reading, the parliamentary-constraint reading, and the cohabitation-equilibrium reading. The colloquial label 'the Fifth Republic constitution' conflates them; their epsilon values differ widely - this reading concentrates extraction on the legislature, the parliamentary reading distributes it toward symmetric coordination cost, and the cohabitation reading makes extraction episodic. Each is authored as its own story with its own epsilon per the epsilon-invariance principle, linked here as a constraint family. This reading is upstream in practice: its consolidation (the 1962 direct-election referendum, the 2000 electoral synchronization) restructured the operating environment of both siblings - suppressing the cohabitation exit and degrading the parliamentary-constraint reading's operating conditions - without logically foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
