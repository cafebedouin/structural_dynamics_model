% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_cohabitation_equilibrium, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Cohabitation Equilibrium: Negotiated Dual Executive Authority
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Fifth Republic's Constitution (1958) created an unusual hybrid: a
 *   directly-elected president with broad executive and foreign-policy
 *   powers, combined with a bicameral parliament that can elect its own
 *   government independent of the president's choice. When presidential and
 *   parliamentary majorities align, the president dominates (the
 *   'hyper-presidential' reading holds empirically). When they oppose — a
 *   situation called 'cohabitation' — the Constitution's text does not
 *   clearly resolve authority, and the two branches must negotiate. This
 *   reading instantiates the cohabitation equilibrium: the empirically
 *   observed arrangement where both branches constrain each other, neither
 *   monopolizes power, and authority is divided by domain (president leads
 *   foreign policy, prime minister leads domestic). The constraint is CLAIMED
 *   as tangled_rope (genuine coordination function in preventing
 *   concentration, active enforcement via constitutional court and political
 *   negotiation, asymmetric benefit distribution depending on which actor
 *   controls the legislature) while metrics reveal moderate extractiveness
 *   with cyclical intensification during cohabitation periods and relaxation
 *   during unified-majority periods. The claim/metric divergence is the
 *   measurement the reading takes: is the cohabitation equilibrium truly a
 *   stable constitutional arrangement, or is it a temporary accommodation
 *   under negotiation stress?
 *
 * KEY AGENTS:
 *   - president: directly elected; claims executive sovereignty; trapped in role even during cohabitation
 *   - prime_minister: appointed by president but must command parliamentary confidence; becomes dominant during cohabitation
 *   - ruling_coalition_majority: controls legislative agenda; can force cohabitation; constrains president domestically
 *   - parliamentary_opposition: blocked from policy execution; constrained by minority status
 *   - foreign_policy_establishment: operates in presidential domain; retains continuity across cohabitations
 *   - constitutional_court: interprets boundary disputes; maintains equilibrium via case law
 *   - electorate: votes separately for president and Assembly; can inadvertently create cohabitation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Cohabitation Equilibrium: Negotiated Dual Executive Authority").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'd884e048-6596-4612-9e79-cc65a18d8a80').
narrative_ontology:cs_kernel_codification('d884e048-6596-4612-9e79-cc65a18d8a80', fixed_text).
narrative_ontology:cs_authority_grounding('d884e048-6596-4612-9e79-cc65a18d8a80', lineage).
narrative_ontology:cs_interpretation_layer_present('d884e048-6596-4612-9e79-cc65a18d8a80').
narrative_ontology:cs_reading_relation('d884e048-6596-4612-9e79-cc65a18d8a80', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('d884e048-6596-4612-9e79-cc65a18d8a80', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('d884e048-6596-4612-9e79-cc65a18d8a80', foundational, dual_executive_constraint_legitimate).
narrative_ontology:cs_axiom_status(dual_executive_constraint_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d884e048-6596-4612-9e79-cc65a18d8a80', dual_executive_constraint_legitimate, deontological).
narrative_ontology:cs_axiom('d884e048-6596-4612-9e79-cc65a18d8a80', foundational, domain_split_governance_sustainable).
narrative_ontology:cs_axiom_status(domain_split_governance_sustainable, holdable).
narrative_ontology:cs_axiom_grounding('d884e048-6596-4612-9e79-cc65a18d8a80', domain_split_governance_sustainable, instrumental).
narrative_ontology:cs_reference_frame('d884e048-6596-4612-9e79-cc65a18d8a80', constitutional_dual_executive_equilibrium).
narrative_ontology:cs_drift_state('d884e048-6596-4612-9e79-cc65a18d8a80', contemporary_post_2012, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('d884e048-6596-4612-9e79-cc65a18d8a80', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, ruling_coalition_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional_stability_doctrine).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_opposition).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, foreign_policy_establishment).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, president).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected directly; claims executive sovereignty and foreign policy dominance; must negotiate daily authority over domestic policy, budget, and parliamentary legislative agenda with a prime minister who may represent opposing coalition. Trapped by the presidency itself — cannot resign without constitutional crisis; forced to cohabitate with adversary when the legislature is controlled by opposition. When cohabiting, surrenders primary control over government formation, legislation, and domestic appointments while retaining ceremonial powers and foreign policy voice.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, payer).

% Appointed by president but must command parliamentary confidence; controls day-to-day government operation, legislative agenda, and domestic policy. During cohabitation, becomes the dominant domestic actor but remains symbolically subordinate to the president and depends entirely on legislative majority support. Can be dismissed by parliament via no-confidence vote; constrained by need to maintain coalition.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary).

% Parliamentary majority that can force cohabitation by electing an opposition president or supporting an opposition prime minister. Controls legislative agenda and can block presidential initiatives in domestic policy. Constrained by the president's veto power in foreign affairs and constitutional powers, and by the difficulty of maintaining coalition discipline across multiple election cycles.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, ruling_coalition_majority, beneficiary,
    organized, biographical, constrained, national).

% Minority or excluded-from-majority party; blocked from executing its policy platform by the ruling coalition's control of the prime minister and legislative agenda. Can obstruct via parliamentary procedure and force compromise, but lacks the votes to pass independent legislation or unseat the government without the ruling coalition fragmenting. Exit is via electoral victory, which is difficult and distant.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_opposition, payer,
    moderate, biographical, constrained, national).

% President's constitutional domain; the foreign ministry, military brass, and diplomatic corps operate under presidential direction. During cohabitation, foreign policy becomes a zone of presidentialist authority where the president can act with less parliamentary constraint, making it an arena where a co-habiting president retains real power despite domestic subordination. They benefit from stable, continuous strategic posture even as domestic governments turn over.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, foreign_policy_establishment, beneficiary,
    powerful, generational, mobile, global).

% The meta-good of integrated, forward-consistent government action across foreign and domestic domains. During cohabitation, splits: foreign policy follows presidential direction, domestic policy follows prime minister and coalition; the result is potential incoherence, slow adaptation when one side must accommodate the other, and reduced strategic flexibility. A non-agent placeholder for the institutional cost of shared authority under adversarial conditions.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% Referee between president and prime minister when their authority claims conflict. Interprets constitutional text and custom to resolve boundary disputes. During major cohabitations (1986–88, 1993–95, 1997–2002, 2007–12), issued rulings clarifying zones of exclusive and shared authority. Maintains the equilibrium via case law rather than formal amendment.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Votes for president and Assembly separately under different electoral calendars and logics; can inadvertently create cohabitation by supporting opposition parties at different times. Once cohabitation exists, has limited voice in negotiated authority settlement; the constraint's persistence depends on their continued electoral behavior (split votes), not on their consent to the dual-executive arrangement itself.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate, excluded,
    organized, biographical, trapped, national).

% The supermajority (3/5 of both chambers) needed to formally amend the Constitution. Could, in principle, abolish cohabitation by making the prime minister a presidential appointee without parliamentary confidence requirement, or by merging elections. Constrained by the difficulty of mustering supermajority agreement; the constraint persists because formal amendment is harder than negotiated accommodation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_amendment_coalition, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, ruling_coalition_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents either the president or the parliament from monopolizing executive authority; forces negotiation and mutual constraint, reducing the risk of democratic breakdown by power concentration; maintains separation-of-powers doctrine and protects minority factions from unchecked majority or presidential dominance.
% TRANSFER_FUNCTION: Transfers effective control over domestic policy from the directly-elected president to the legislatively-accountable prime minister during cohabitation; moves veto power over legislation and budgets from presidential hands to parliamentary hands; shifts the benefit of the arrangement from whichever faction holds the presidency to whichever faction controls the Assembly.
% ABSENT_VOICES: Those who would prefer a purely presidential system (executive authority concentrated, no prime minister) are excluded from the constraint's design phase but present in political discourse and press (the hyper-presidential reading). Those who would prefer a purely parliamentary system (prime minister independent of president) are also excluded from the initial constitutional compromise but remain advocates for amendment. Constitutional amendment advocates who would prefer to formally resolve the ambiguity are kept outside the constraint by the 3/5 supermajority requirement.
% DISAPPEARANCE_RATIONALE: If the cohabitation equilibrium vanished and the Constitution were unambiguous about who holds executive power, governments would reorganize immediately: either the president would become supreme executive (hyper-presidential reading), or the president would become ceremonial (parliamentary reading). Policy, personnel, and international commitments would shift. Electoral behavior would change — voters would no longer be able to create split outcomes if the Constitution forbade it.
% FOUNDING_PROBLEM: The 1958 Fifth Republic Constitution created a president with broad (but vaguely delimited) powers and a bicameral parliament that could elect its own prime minister. The text left it ambiguous what happens when the president and parliamentary majority are from opposing factions. The founding problem was to enable a strong, presidentialist executive while preserving republican legitimacy and preventing democratic backsliding during the transition from empire to republic.
% FOUNDING_PROBLEM_CORROBORATION: De Gaulle's supporters and presidentialist scholars attest the founding problem is still live: strong presidency is necessary for stable governance and national coherence. Opposition political parties, parliamentary advocates, and some constitutional scholars attest the founding problem has been superseded: decades of stable cohabitation show the constraint now serves to prevent presidential over-reach and preserve legislative accountability, a function different from the 1958 anti-chaos intent. International observers (UK, Germany constitutional experts) and France's own Constitutional Court rulings support the 'new equilibrium, separated from founding intent' interpretation.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness averages 0.58 across the interval but rises sharply during cohabitation periods (1986→0.62, 1997→0.65) and relaxes during unified periods (1973→0.48, 2007–12→0.58). This cyclical pattern reflects the core structural fact: cohabitation intensifies the negotiation cost and extractive load on both branches — they must invest more effort in compromise, delay increases, and policy coordination becomes difficult. Suppression (0.42 average) is moderate because neither branch can fully suppress the other — the president cannot dissolve parliament at will (attempt made in 1962, rejected in 1986), and parliament cannot easily remove the president (requires majority in both chambers, never achieved). Theater ratio rises during cohabitation (0.52–0.58 at 1986, 1997) because both actors engage in public positioning and constitutional rhetoric while actual decisions emerge from backstage negotiation. Accessibility collapse is moderate (0.61): the Constitution can be amended, but the 3/5 supermajority requirement makes formal resolution difficult; the cohabitation equilibrium persists because negotiated accommodation is easier than amendment. Resistance is high (0.68) because the opposition constantly resists and the constraint's own logic depends on both branches defending their turf — the equilibrium is maintained by mutual resistance, not consent.
 *
 * PERSPECTIVAL GAP:
 *   From the president's seat during unified government, this is a rope (genuine coordination, manageable enforcement, net benefit to governing). From the prime minister's seat during unified government, it is a constraint that subordinates domestic authority to presidential direction — extractive and subordinating. During cohabitation, the seats reverse: the president sees a snare (forced into powerlessness), while the prime minister sees a rope (legitimate domestic authority, constitutional support). The constitutional court sees this entire dynamic as tangled_rope across all periods — genuine coordination function (preventing concentration) coupled with asymmetric distribution (whoever holds more offices extracts more). The opposition always sees a snare: structurally excluded from both seats despite having electoral support. The engine will compute these per-seat types from the structural data; this reading claims the territory-split is the stable equilibrium across all phases.
 *
 * DIRECTIONALITY LOGIC:
 *   The president's directionality shifts with the political cycle: during unified government (same party controls presidency and Assembly), the president is a full beneficiary (d→0.0); during cohabitation, the president becomes partially targeted (d→0.5–0.7) because the prime minister and coalition can block presidential initiatives. The prime minister's directionality is inverse: weak during unified government (d→0.6–0.8, constrained by presidential will), strong during cohabitation (d→0.1–0.3, controls domestic agenda). The ruling coalition majority's directionality approaches 0.0 during periods when it controls both offices; during opposition presidency it approaches 0.5. The opposition's directionality is consistently high (d→0.8+) because it is structurally excluded from the constraint's benefits. This reading's claim of tangled_rope captures the fact that BOTH coordination and extraction are simultaneously present: coordination in preventing concentration (real benefit to the republic), extraction in the form of policy delay and reduced coherence (real cost), with the balance shifting based on who holds which office.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem in 1958 was to create a strong presidency capable of governing France coherently and preventing return to multiparty paralysis (the Fourth Republic problem). By the 1990s, the problem the cohabitation equilibrium actually SOLVES has shifted: it prevents presidential over-reach and preserves legislative accountability — a different mandate. However, the constraint persists because both branches have learned to operate within it, the Constitutional Court has clarified its boundaries, and formal amendment remains harder than negotiated accommodation. Mandatrophy is partial: the original rationale (strong executive + republican legitimacy) is dead/contested, but a new rationale (prevent concentration + preserve separation of powers) has emerged in its place. The constraint has not been abandoned because the new rationale is institutionally stronger than the amendment process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_stability,
    'Is the cohabitation equilibrium a stable constitutional arrangement, or an unstable accommodation under negotiation stress that will eventually resolve toward either hyper-presidential or parliamentary dominance?',
    'Long-run constitutional practice: if cohabitation remains frequent (recurs every 7–10 years), stable, and bounded by the same authority-split pattern, then it is stable. If it gradually drifts toward one branch consistently dominating, then it is unstable. Post-2012 electoral reform (synchronized presidential/parliamentary elections in some proposed models) provides a natural experiment.',
    'If stable: this reading''s tangled_rope claim is vindicated; the Constitution successfully balances two branches. If unstable and drifting hyper-presidential: the hyper_presidential_reading becomes empirically dominant. If unstable and drifting parliamentary: the parliamentary_constraint_reading becomes dominant. Stability vs. drift is the core structural question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_stability, empirical, 'Whether cohabitation equilibrium is a stable constitutional pattern or transitional accommodation.').

omega_variable(
    constitutional_text_ambiguity,
    'Does the Fifth Republic Constitution genuinely leave executive authority ambiguous between president and prime minister, or does the text actually privilege one branch — and is ambiguity itself a feature or a defect?',
    'Comparative constitutional law analysis: legal scholars from outside the French constitutional tradition (German, UK, Canadian) provide independent close-reading of the French text. Courts in other jurisdictions applying similar constitutional text offer interpretive precedent. Historical record of Constituent Assembly intent (what did the drafters intend the text to mean?).',
    'If ambiguity is textual: this reading treats ambiguity as constitutional design allowing negotiation. If the text actually privileges one branch but is misread: this reading is built on a false premise, and the constraint drifts toward the privileged reading. If ambiguity is a feature: it supports institutional flexibility and prevents premature lockdown of power. If ambiguity is a defect: formal amendment becomes more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, conceptual, 'Whether textual ambiguity about executive authority is genuine or interpretive artifact.').

omega_variable(
    electoral_calendar_contingency,
    'How much of the cohabitation equilibrium depends on the accidents of electoral timing (presidential and parliamentary elections on different schedules), versus being a stable constitutional choice?',
    'Natural experiment: if France synchronizes presidential and parliamentary elections (a proposal periodically raised), does cohabitation cease and hyper-presidential dominance resume? Or does the equilibrium persist via institutional culture and court interpretation regardless of electoral timing?',
    'If timing is contingent: cohabitation is not a stable constitutional equilibrium but an artifact of electoral design. This reading becomes historical rather than structural. If equilibrium persists after synchronization: it reflects genuine constitutional negotiation, not just electoral chance. Structural consequence: a timing-dependent constraint is fragile; a culturally-embedded constraint is resilient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_calendar_contingency, empirical, 'Role of electoral timing in maintaining or preventing cohabitation.').

omega_variable(
    kernel_reading_contest,
    'How should the interpreter choose between this reading (cohabitation equilibrium as valid stable arrangement), the hyper-presidential reading (president as supreme), and the parliamentary-constraint reading (legislature as supreme) when the Constitution itself does not definitively settle the matter?',
    'Committer axis: the choice depends on which interpretive tradition (Gaullist, republican, parliamentary) the interpreter privileges and which version of the Constitution''s purpose (strong executive, democratic accountability, separation of powers) the interpreter endorses. No textual resolution exists; the different readings reflect different legitimate constitutional frameworks.',
    'This is the irreducible omega of commitment-system ambiguity. The constraint''s type and extractiveness depend on which reading is adopted. A Gaullist interpreter sees hyper-presidential reading and low extractiveness on presidential seat, high on opposition. A republican interpreter sees cohabitation equilibrium and moderate extractiveness on both seats. A parliamentary advocate sees parliamentary-constraint reading and high extractiveness on presidential seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Irreducible interpretive indeterminacy: which reading of the Constitution is correct when multiple are textually defensible?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2012).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.35).
narrative_ontology:measurement(fift_tr_t1973, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1973, 0.38).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.52).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.58).
narrative_ontology:measurement(fift_tr_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2007, 0.48).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2012, 0.48).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement(fift_be_t1973, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1973, 0.48).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.62).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.65).
narrative_ontology:measurement(fift_be_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2012, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.38).
narrative_ontology:measurement(fift_su_t1973, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.45).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.48).
narrative_ontology:measurement(fift_su_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2007, 0.42).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2012, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.18).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fifth_republic_constitution kernel. The other readings are hyper_presidential_reading (president as supreme executive) and parliamentary_constraint_reading (legislature as supreme executive). All three are live positions held by different French political factions and Constitutional Court interpretations over the decades. This reading (cohabitation_equilibrium) claims both branches constrain each other; it coexists_with the other readings (neither is logically foreclosed, but they prioritize different constitutional values and apply different interpretive traditions). The three readings share the same referent (the 1958 Constitution), but differ in ε (extractiveness) because they assess the Constitution under different axioms about separation of powers, executive accountability, and republican legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
