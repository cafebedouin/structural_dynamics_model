% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Popular-Sovereignty Legitimation Regime (Delegated Consent Reading)
 *   domain: political philosophy/constitutional theory/legitimacy studies
 *
 * SUMMARY:
 *   A republic's claim to legitimacy is that political authority is real only
 *   as a standing delegation: the people authorize, officeholders execute,
 *   elections renew or revoke the mandate, and a constitutional frame polices
 *   what majorities may lawfully do. This story authors that arrangement as
 *   it actually operates — franchise lines as drawn, safeguards as built,
 *   campaign finance as it flows — assessed by this reading's own standards:
 *   the epsilon referent is the operative republican legitimation regime
 *   under contest, never the reading's idealized alternative and never a
 *   sibling reading's arrangement. Its coordination half is load-bearing: it
 *   converts grievance into replacement rather than revolt and synchronizes
 *   peaceful transfers of power. Its extraction half is equally structural:
 *   persons governed at full intensity with no consent channel, minorities
 *   bound cycle after cycle by durable majorities, and generations bound by
 *   commitments they never made. The colloquial label 'legitimate authority'
 *   decomposes into three readings of the sovereign_legitimacy kernel; this
 *   file is the republican one and hedges nothing across siblings. Claim and
 *   metrics are independent: tangled_rope is asserted from structure, and the
 *   metric scores describe observed operation without being tuned to confirm
 *   the claim. KEY AGENTS (by structural relationship): -
 *   enfranchised_voting_citizenry: primary beneficiary
 *   (organized/constrained) — source of delegated consent, bears episodic
 *   mobilization costs - elected_officeholders: agenda-setting beneficiary
 *   (institutional/constrained) — receives delegated authority, writes
 *   selection rules, removable via the mechanism itself -
 *   majority_coalitions: cycling beneficiary (organized/mobile) — collects
 *   policy victories while ascendant, dissolves on defeat -
 *   disenfranchised_residents: primary target (powerless/trapped) — fully
 *   bound by the arrangement, no authorized consent channel -
 *   persistent_minority_groups: recurring target (organized/trapped) —
 *   participates but systematically loses decisive aggregations -
 *   future_generations: silent target (powerless/trapped, civilizational
 *   horizon) — bound by present commitments without present consent -
 *   constitutional_high_courts: analytical observer (institutional) —
 *   adjudicates the boundary between popular will and constitutional limit -
 *   rival_authorization_claimants: excluded alternative (organized/regional)
 *   — hereditary, theocratic, and secessionist legitimacy claims kept outside
 *   the authorization conversation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.48).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.5).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Popular-Sovereignty Legitimation Regime (Delegated Consent Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political philosophy/constitutional theory/legitimacy studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, 'f691006d-7ec0-45d0-afcc-67ab19ea3505').
narrative_ontology:cs_kernel_codification('f691006d-7ec0-45d0-afcc-67ab19ea3505', formalized).
narrative_ontology:cs_authority_grounding('f691006d-7ec0-45d0-afcc-67ab19ea3505', practice).
narrative_ontology:cs_interpretation_layer_present('f691006d-7ec0-45d0-afcc-67ab19ea3505').
narrative_ontology:cs_reading_relation('f691006d-7ec0-45d0-afcc-67ab19ea3505', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('f691006d-7ec0-45d0-afcc-67ab19ea3505', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f691006d-7ec0-45d0-afcc-67ab19ea3505', foundational, all_legitimate_authority_derives_from_popular_consent).
narrative_ontology:cs_axiom_status(all_legitimate_authority_derives_from_popular_consent, holdable).
narrative_ontology:cs_axiom_grounding('f691006d-7ec0-45d0-afcc-67ab19ea3505', all_legitimate_authority_derives_from_popular_consent, deontological).
narrative_ontology:cs_axiom('f691006d-7ec0-45d0-afcc-67ab19ea3505', secondary, delegated_authority_conditionally_revocable_through_electoral_cycles).
narrative_ontology:cs_axiom_status(delegated_authority_conditionally_revocable_through_electoral_cycles, holdable).
narrative_ontology:cs_axiom_grounding('f691006d-7ec0-45d0-afcc-67ab19ea3505', delegated_authority_conditionally_revocable_through_electoral_cycles, conventional).
narrative_ontology:cs_reference_frame('f691006d-7ec0-45d0-afcc-67ab19ea3505', popular_sovereignty_consent_framework).
narrative_ontology:cs_drift_state('f691006d-7ec0-45d0-afcc-67ab19ea3505', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f691006d-7ec0-45d0-afcc-67ab19ea3505', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_voting_citizenry).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officeholders).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, majority_coalitions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, persistent_minority_groups).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, future_generations).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, consent_of_the_governed_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authorization power the whole arrangement runs on: periodically casts ballots that confer, renew, or terminate officeholders' mandates, and between elections petitions, organizes, and litigates. Carries the costs of civic attention and periodic mobilization and absorbs policy outcomes as a diffuse mass. Exit means emigrating to another polity with its own authorization regime — leaving does not release anyone from being governed somewhere else.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_voting_citizenry, beneficiary,
    organized, generational, constrained, national).

% Competes for delegated mandates and then wields the administrative, legislative, and coercive powers conferred by election results. Also writes the rules of its own selection — franchise law, district boundaries, ballot access — and staffs the machinery that keeps elections running. Tenure ends at scheduled intervals unless renewed; defeat removes them peacefully. Careers, patronage networks, and post-office prospects all depend on the continuation of the selection cycle, and every term in office is lived under threat of removal by the electorate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officeholders, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, elected_officeholders, beneficiary).

% Temporary alliances of voters, parties, and interest blocs that win enough aggregation events to set policy. While ascendant, their preferences are enacted and their opponents' overridden; on defeat they dissolve and re-form around new issues. Nothing pins them to a fixed position — their defining feature is that membership rotates with each electoral cycle, and today's losing bloc is a candidate for tomorrow's winning one.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, majority_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Lives under the full weight of the laws, taxes, policing, and long-term commitments the arrangement produces, but holds no authorized channel to grant or withhold consent: ballots, candidacy, and referendum access are closed by status — citizenship category, criminal record, territory of residence, or age. Recourse is petitioning those already enfranchised, litigation, or emigration; none of these converts presence into counted consent. Many are born into the exclusion and die in it, and the rules of their inclusion are written exclusively by the enfranchised.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, disenfranchised_residents, excluded).

% Participates fully in the authorization cycle — votes, organizes, runs candidates, lobbies — yet loses the decisive aggregations cycle after cycle, because numbers or geographic dispersion guarantee being outvoted whenever stakes concentrate. Counter-majoritarian safeguards sometimes blunt the worst outcomes; the underlying arithmetic recurs regardless. Internal relocation does not escape a national majority, and emigration means abandoning home, livelihood, and community.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, persistent_minority_groups, payer,
    organized, generational, trapped, national).

% Will inherit the debts, environmental commitments, constitutional amendments, and institutional precedents contracted by today's majorities. Cannot vote, object, or renegotiate now; by the time each cohort can act, the commitments are sunk. Every cohort hands the next a pre-negotiated world it never agreed to, and no mechanism exists by which the not-yet-present register consent or dissent.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, future_generations, payer,
    powerless, civilizational, trapped, national).

% Reviews legislation and executive action against the constitutional text that frames the arrangement, striking down what fails. Decides what counts as a valid exercise of the popular will and where majorities must stop — questions the majorities themselves would often answer differently. Judges are selected through the very processes they referee, and their rulings reshape the franchise and the consent mechanisms over generational timescales.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_high_courts, observer,
    institutional, generational, analytical, national).

% Hereditary-restorationist circles, theocratic movements, and secessionist campaigns hold complete theories of legitimate rule that the arrangement does not merely outvote but defines as out of bounds — ineligible for ballots, barred from official recognition, occasionally prosecuted when acted upon. They operate at the margins of tolerated politics; their claims survive as argument and underground organization rather than as access to the authorization conversation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, rival_authorization_claimants, excluded,
    organized, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates millions of dispersed political judgments into single binding authorization events on a fixed schedule; confers revocable mandates on officeholders; converts grievances about rule into competitive replacement of rulers rather than violent removal; and synchronizes the peaceful transfer of governing power across an entire polity on a predictable calendar.
% TRANSFER_FUNCTION: Transfers decision authority and command of lawful coercion from the citizenry at large to elected officeholders for fixed renewable terms; transfers tax revenue and compliance from everyone governed — consenting or not — into institutions those officeholders direct; and during campaigns transfers money, media attention, and volunteer labor from donors, broadcasters, and activists toward contenders for office.
% ABSENT_VOICES: The disenfranchised would object first: they are governed at full intensity with no consent channel (see disenfranchised_residents). Rival authorization traditions — hereditary, theocratic, secessionist — would object that their legitimacy claims are ruled inadmissible rather than defeated on merit (see rival_authorization_claimants). Future generations cannot appear at all yet absorb the longest-lived commitments. None of these seats sits in the authorization conversation whose reach they contest.
% DISAPPEARANCE_RATIONALE: Every office's title would evaporate overnight: mandates derive their validity from the consent cycle, and with it gone, succession would fall to raw possession — military commands, wealthy families, foreign patrons, and leftover hereditary claimants each asserting rule. Courts would lose the constitutional frame they adjudicate within, and the removal mechanism that converts grievance into turnover would vanish with it. Nearly every modern institution chains its authority back through this arrangement, so the rearrangement would be extensive and violent.
% FOUNDING_PROBLEM: After divine-right and bloodline justifications for rule collapsed under religious schism, dynastic war, and Enlightenment criticism, large-scale societies faced the problem of what could make obedience legitimate: why should the governed obey any particular set of governors, and how should governors be replaced without war? The answer built here: locate the source of authority in the governed themselves, and make its renewal a repeating public act.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set on both flanks. Authoritarian regimes corroborate it involuntarily: governments that permit no real contest nonetheless stage elaborate elections, spending real resources to counterfeit the consent signal — behavior that makes sense only if the legitimation problem remains live even for the arrangement's enemies. Classical corroboration predates the beneficiary classes: contract theorists wrote before mass electorates existed, addressing skeptics and rulers generally, and Edmund Burke, attacking the proposed solution from a traditionalist seat, conceded the problem it answered was real. Contemporary legitimacy research measuring trust decay corroborates that the problem recurs each generation rather than having been solved once.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48 is moderate by construction of the arrangement: a working consent mechanism holds extraction below snare range, but full-intensity governance without a consent channel (status-based disenfranchisement), systematic minority outvoting, and intergenerational binding hold it well above rope range. The value is reading-indexed over the fixed referent (the operative arrangement), not averaged across sibling readings. Suppression 0.50 is a RAW structural property, unscaled by power or scope (only extractiveness is scaled in the engine's computation): rival authorization channels are legally barred and delegitimized, occasionally prosecuted when enacted, but ordinary political contestation is institutionalized and routine — what is suppressed is the authorization channel itself, not participants' welfare. Theater 0.27: elections genuinely confer and remove power, but campaign ritual, symbolic voting, and plebiscitary performance carry a substantial and rising performative share. Accessibility_collapse 0.38: alternatives remain visible and live — sibling readings, technocratic proposals, sortition experiments, direct-democracy instruments — so understanding the arrangement does not collapse its rivals. Resistance 0.55: sustained and recurrent, from suffrage movements historically through secession attempts, abstention waves, court-curbing bills, and anti-constitutional populism; notably, much resistance attacks the exclusion half while affirming the coordination half.
 *   
 *   CYCLICAL PATTERN: the measurement series shows a full cycle — accumulation (t0-t12: demographic change outpacing franchise adjustment, donor concentration, district entrenchment drive epsilon up), incident (t12: legitimacy crisis peak, epsilon 0.56, theater 0.33 as performative response swells), reform (t15: franchise expansion and representation repair pull epsilon down sharply), honeymoon/relaxation (t18-t21), then renewed accumulation (t24-t30). Unlike interpersonal intermittent reinforcement, the oscillation here is partly the self-correction mechanism working: crisis opens reform windows that reset extraction. But each cycle leaves ratchet residue — the net trend across one full cycle is upward (0.40 to 0.48), and the theater floor creeps up with each repetition. Scalars are authored at t30, late-accumulation phase, so base_properties reflects that phase, not the cycle average. All tracked metrics run on one shared time grid (step 3 across the full interval); suppression_requirement is deliberately untracked because enforcement capacity is static in this story's narrative — the flat enforcement picture is carried by the base_properties scalar alone.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the officeholder seat the arrangement is simultaneously the source of its authority, the discipline over it, and the rulebook it writes — a mixed position no pure category captures from inside. From the enfranchised voter seat it is near-pure subsidy: the machine exists to validate their sovereignty. From the disenfranchised seat the identical legal order presents as unconsented imposition — same laws, same taxes, zero channel. The sharpest same-power contrast is majority_coalitions versus persistent_minority_groups: both hold the organized power atom, yet one sits near the beneficiary extreme and the other near the target extreme; the differentiator is positional durability and exit (mobile re-formation versus trapped recurrence), not resources or organization. The courts see the full structure from an analytical seat and are themselves products of the processes they referee. The engine computes these per-seat classifications from the authored structural data; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map cleanly onto the directionality axis. Enfranchised voters derive near the full-beneficiary end (d ~0.05): the arrangement subsidizes them with validated sovereignty. Majority coalitions derive similarly low with mobility damping (arbitrage-adjacent exit). Disenfranchised residents derive near the full-target end (d ~0.95): powerless, trapped, bearing every governance cost with no consent input. Future generations sit at or beyond full-target (d ~0.90+): no channel exists in any period available to them. Persistent minority groups derive high but below full-target (d ~0.80-0.85): they hold a real participation channel that simply loses — participation moderates directionality even when outcomes do not. One override is authored: elected_officeholders (institutional) at d 0.25, above the ~0.10 the uniform treatment of the beneficiaries array would produce, because officeholders are not passive collectors — they administer enforcement, write the selection rules that define everyone else's position, and live under removal discipline and career lock to the mechanism. The derivation chain reads the beneficiaries array uniformly and cannot distinguish the voter's subsidy from the officeholder's administered, accountable position. Scope note: the national scope raises verification difficulty modestly, amplifying effective extraction for target seats; suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — every generation re-legitimates or forfeits legitimacy, and even the arrangement's enemies counterfeit its signal — so the R5 mismatch consumer reads status=live x verdict=world_rearranges: coherent, no zombie flag. The tangled_rope claim guards against the two symmetrical mislabelings. Calling this a rope would zero out the excluded: the disenfranchised, the recurrently outvoted, and the unborn bear real costs that a pure-coordination reading renders invisible. Calling it a snare would erase the accountability function that genuinely and peacefully removes rulers — a function with no cover-story character whatsoever. Scaffold is structurally unavailable: the arrangement declares itself permanent by design, renewal rather than transition. Residual decay risk is tracked by the theater series: if electoral performance detaches from the conferment/removal function (sustained theater above 0.5), the arrangement slides toward piton — the measurements watch exactly that threshold, peaking at 0.33 in-cycle and resetting after each reform window.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the republican reading of the sovereign_legitimacy kernel. Would instantiating a sibling reading — monarchical (authority flows downward through inherited right) or constitutional_hybrid (dual-sourced, constitutionally mediated) — change the beneficiary/victim structure and epsilon of the legitimation arrangement?',
    'Comparative classification across the three sibling files: align each reading''s beneficiary/victim arrays and epsilon over the same polity, and isolate the disagreement element — the direction of legitimacy flow, the identity of the authorizing principal, and the legitimacy weight assigned to inheritance.',
    'Under the monarchical reading the beneficiary/victim sets invert (court and nobility collect; the populace pays without a consent channel); under the hybrid reading beneficiaries split between a ceremonial principal and an elective principal. Epsilon and per-seat classifications are reading-indexed, so cross-reading comparison is valid only over the shared referent. Note on scoping: the republican-hybrid pair avoids foreclosure only because the hybrid''s inherited component is confined to ceremonial/symbolic functions outside this reading''s axiom scope (political-coercive authority); if that scoping judgment were rejected, the hybrid would join the monarchical reading in the foreclosure relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the sovereign_legitimacy kernel; each sibling instantiates a different constraint with its own epsilon and party structure.').

omega_variable(
    demos_boundary_circularity,
    'Who is ''the people'' whose consent authorizes? The enrolled electorate defines franchise boundaries, yet the reading''s moral principal may be all the governed — the boundary that determines the victim set is drawn by the mechanism it characterizes.',
    'Track franchise jurisprudence and expansion episodes against demographic reality: if the enrolled demos converges on all permanently governed persons the circularity closes benignly; if exclusions persist or grow, the gap is structural rather than transitional.',
    'If the demos is all the governed, current exclusions are core victims and epsilon rises materially; if the demos is the enrolled citizenry, exclusions fall outside the arrangement''s own accounting and epsilon drops — swinging the classification between tangled_rope and near-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_boundary_circularity, conceptual, 'Demos-definition circularity: the mechanism defines the population whose consent is supposed to define the mechanism.').

omega_variable(
    majoritarian_binding_status,
    'Are losses suffered by persistent minorities under durable majorities extraction, or the legitimate price of collective decision adequately buffered by counter-majoritarian safeguards?',
    'Measure safeguard strength and outcomes: judicial protection, supermajority thresholds, minority-weighted institutions, and longitudinal gaps between minority outcomes and majority-preference curves.',
    'Strong effective safeguards push the minority seat toward symmetric directionality and the arrangement toward rope; weak safeguards leave the minority seat near full target and the arrangement at tangled_rope trending snare-for-that-seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_binding_status, conceptual, 'Whether systematic minority loss counts as extraction depends on safeguard strength and normative framing.').

omega_variable(
    consent_authenticity,
    'Is the periodic consent signal authentic delegation, or manufactured acquiescence shaped by restricted choice sets, donor-filtered agendas, and information asymmetry?',
    'Compare expressed preferences under expanded-choice conditions (ranked ballots, open primaries, publicly financed campaigns) against baselines; measure congruence between policy output and median-voter preference over time.',
    'If consent is substantially manufactured, the validation mechanism the reading rests on degrades toward performance, epsilon rises, and the coordination claim weakens; if the signal is robust, current metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity, empirical, 'Authenticity of the electoral consent signal versus manufactured acquiescence.').

omega_variable(
    extraction_seat_rotation,
    'Does electoral turnover genuinely rotate the seat receiving the arrangement''s gains, or has capture congealed into a durable donor-incumbent class?',
    'Longitudinal analysis of policy responsiveness by income bracket, incumbent persistence under neutralized spending conditions, and coalition-composition turnover across cycles.',
    'Genuine rotation supports the diffuse receipt reading and blocks drift toward snare; congealed capture relocates the gains to a named seat and pushes classification toward snare. This omega is the load-bearing assumption behind authoring gain_flow as diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_seat_rotation, empirical, 'Whether the gains-receiving seat rotates with elections or has congealed into durable capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t3, sovereign_legitimacy__republican_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement(sove_tr_t6, sovereign_legitimacy__republican_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(sove_tr_t9, sovereign_legitimacy__republican_reading, theater_ratio, 9, 0.29).
narrative_ontology:measurement(sove_tr_t12, sovereign_legitimacy__republican_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(sove_tr_t15, sovereign_legitimacy__republican_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(sove_tr_t18, sovereign_legitimacy__republican_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(sove_tr_t21, sovereign_legitimacy__republican_reading, theater_ratio, 21, 0.2).
narrative_ontology:measurement(sove_tr_t24, sovereign_legitimacy__republican_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(sove_tr_t27, sovereign_legitimacy__republican_reading, theater_ratio, 27, 0.26).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__republican_reading, theater_ratio, 30, 0.27).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sove_be_t3, sovereign_legitimacy__republican_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(sove_be_t6, sovereign_legitimacy__republican_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(sove_be_t9, sovereign_legitimacy__republican_reading, base_extractiveness, 9, 0.51).
narrative_ontology:measurement(sove_be_t12, sovereign_legitimacy__republican_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(sove_be_t15, sovereign_legitimacy__republican_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(sove_be_t18, sovereign_legitimacy__republican_reading, base_extractiveness, 18, 0.41).
narrative_ontology:measurement(sove_be_t21, sovereign_legitimacy__republican_reading, base_extractiveness, 21, 0.4).
narrative_ontology:measurement(sove_be_t24, sovereign_legitimacy__republican_reading, base_extractiveness, 24, 0.43).
narrative_ontology:measurement(sove_be_t27, sovereign_legitimacy__republican_reading, base_extractiveness, 27, 0.46).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__republican_reading, base_extractiveness, 30, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sovereign_legitimacy__republican_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, resource_allocation).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate authority' conflates three structurally distinct legitimation arrangements and is decomposed per the epsilon-invariance principle into a three-file constraint family sharing the sovereign_legitimacy kernel: monarchical_reading (historically upstream — its collapse supplied the founding problem this reading answers, and it is cited as the rejected alternative in republican founding documents), constitutional_hybrid_reading (the mediating form, under continuous structural pressure from this reading toward fuller popular authorization — parliamentary monarchies progressively absorbing elective elements), and this republican_reading. Each file carries its own epsilon, beneficiary/victim structure, and claimed type; family links run through affects_constraints in all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
