% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: Mandatory Interpretive Discretion over the Palestine Mandate Instruments
 *   domain: international law/colonial administration/state formation
 *
 * SUMMARY:
 *   The League of Nations confirmed the Palestine Mandate in 1923 as a fixed
 *   instrument containing materially inconsistent commitments: facilitation
 *   of a 'national home' for the Jewish people alongside preservation of the
 *   'civil and religious rights' of the existing non-Jewish communities and
 *   the eventual development of self-governing institutions. This story
 *   instantiates the procedural reading of that contested kernel: the
 *   operative constraint on the governed parties was never the text's
 *   direction but the mandatory power's unreviewable authority to decide,
 *   period by period, what the text meant. The Churchill White Paper (1922),
 *   Passfield White Paper (1930), MacDonald letter (1931), Peel report
 *   (1937), and 1939 White Paper each reset the operative meaning -
 *   immigration gates, land-transfer zones, constitutional prospects - by
 *   administrative act, with no forum in which either community could obtain
 *   a binding ruling. FAMILY DECOMPOSITION: the colloquial label 'the
 *   Mandate' conflates three constraints instantiated from one kernel.
 *   jewish_national_home_primacy reads the instruments as directive toward
 *   Jewish sovereignty (epsilon concentrated on the Arab community);
 *   dual_obligation_indigenous_rights reads them as protective obligations
 *   subordinating the national-home clause (epsilon concentrated on the
 *   Zionist program); this file's mandatory_interpretive_discretion reads the
 *   adjudication procedure itself as the constraint (moderate epsilon ~0.62
 *   borne by both communities in alternation, accruing to the interpreter).
 *   Each substantive reading supplies the material this reading administers;
 *   family links run through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - british_mandatory_administration:
 *   agenda-setter and receipt-seat (institutional/arbitrage) - holds and
 *   exercises interpretive supremacy; collects the flexibility premium -
 *   palestinian_arab_community: target (organized/trapped) - bears
 *   uncertainty, influence costs, and irreversible land loss under adverse
 *   swings - jewish_yishuv_community: target (organized/trapped) - bears gate
 *   dependence, revocable gains, and closed gates after 1939 -
 *   league_permanent_mandates_commission: excluded reviewer
 *   (institutional/analytical) - hears everything, binds nothing -
 *   un_special_committee_on_palestine: terminal observer
 *   (institutional/analytical) - certifies unworkability as the regime ends
 *
 * KEY AGENTS:
 *   - british_mandatory_administration: agenda-setter and receipt-seat (institutional/arbitrage) - drafts ordinances, issues white papers, grants or withholds certificates, collects flexibility and cheap governance
 *   - palestinian_arab_community: primary target (organized/trapped) - bears strategic uncertainty, influence costs, and irreversible land loss
 *   - jewish_yishuv_community: primary target (organized/trapped) - bears administrative gate dependence and revocable gains
 *   - league_permanent_mandates_commission: structurally excluded reviewer (institutional/analytical) - receives petitions and reports with no binding power
 *   - un_special_committee_on_palestine: terminal observer (institutional/analytical) - documents unworkability at the constraint's endpoint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.6).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "Mandatory Interpretive Discretion over the Palestine Mandate Instruments").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international law/colonial administration/state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '233de671-c6a1-447f-abf0-a5f6f96508ba').
narrative_ontology:cs_kernel_codification('233de671-c6a1-447f-abf0-a5f6f96508ba', fixed_text).
narrative_ontology:cs_authority_grounding('233de671-c6a1-447f-abf0-a5f6f96508ba', extraction).
narrative_ontology:cs_interpretation_layer_present('233de671-c6a1-447f-abf0-a5f6f96508ba').
narrative_ontology:cs_reading_relation('233de671-c6a1-447f-abf0-a5f6f96508ba', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('233de671-c6a1-447f-abf0-a5f6f96508ba', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('233de671-c6a1-447f-abf0-a5f6f96508ba', foundational, mandatory_interpretive_supremacy).
narrative_ontology:cs_axiom_status(mandatory_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('233de671-c6a1-447f-abf0-a5f6f96508ba', mandatory_interpretive_supremacy, conventional).
narrative_ontology:cs_axiom('233de671-c6a1-447f-abf0-a5f6f96508ba', foundational, administrative_revision_over_textual_fixity).
narrative_ontology:cs_axiom_status(administrative_revision_over_textual_fixity, holdable).
narrative_ontology:cs_axiom_grounding('233de671-c6a1-447f-abf0-a5f6f96508ba', administrative_revision_over_textual_fixity, instrumental).
narrative_ontology:cs_reference_frame('233de671-c6a1-447f-abf0-a5f6f96508ba', discretionary_tutelage_prerogative).
narrative_ontology:cs_drift_state('233de671-c6a1-447f-abf0-a5f6f96508ba', terminal_mandate_years, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('233de671-c6a1-447f-abf0-a5f6f96508ba', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_administration).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_yishuv_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts ordinances, issues white papers, grants or withholds immigration certificates, opens and closes land-transfer zones, appoints commissions of inquiry, and answers or declines League inquiries. Collects the arrangement's principal returns: freedom to re-price commitments, governance without negotiated consent, and the ability to defer crises by shifting baselines. Officials rotate on short cycles while the communities' stakes are generational, so time consistently favors the interpreting seat. Exit is real and ultimately exercised: withdraw and refer the problem onward (1947), leaving successors the wreckage.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_administration, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_administration, beneficiary).

% Majority population whose land tenure, villages, and political claims are tied to the territory. Strategy reduces to influencing the interpreter: delegations to London, petitions to Geneva, strikes, and finally revolt. Gains when policy swings protective (1930, 1939) are administrative grants, revocable by the next paper; land sold in permissive years cannot be unsold in restrictive ones. No forum exists where its reading of 'civil and religious rights' binds against the mandatory's interest.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community, payer,
    organized, generational, trapped, national).

% Immigrant-building national movement whose entire program runs through administrative gates: immigration certificates, land-purchase approval, recognition of its institutions. Favored swings (the 1922 clarification, the 1931 letter) deliver real demographic and territorial gains, but each gain sits on administrative sufferance and is repriced by the next reversal (1930, 1939). After 1939 the gates close; relocating elsewhere contradicts the movement's constitutive purpose, so strategy again collapses into pressuring the interpreter, then resisting it.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_yishuv_community, payer,
    organized, generational, trapped, national).

% Nominally supervises the mandate: receives annual reports and hundreds of petitions, questions the accredited representative, records observations. Holds no binding power - its recommendations require Council action that great-power politics never supplies. Its objections are preserved in minutes and dismissed in practice; it sits inside the procedure and outside the decision.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_permanent_mandates_commission, excluded,
    institutional, generational, analytical, global).

% Arrives at the terminal moment (1947) after the mandatory refers the dispute outward; takes testimony from all seats, finds the mandate unworkable, recommends termination and partition. Its observation is the constraint's death certificate rather than a check on its operation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, un_special_committee_on_palestine, observer,
    institutional, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_administration).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates in one administering power the decisions a divided territory requires daily - ordinance-making, land-title registration, immigration quota allocation, public order - avoiding both inter-communal veto gridlock and a multi-power condominium, and giving the two communities a single address for claims neither could settle with the other.
% TRANSFER_FUNCTION: Moves interpretive authority and strategic certainty from both communities to the administering power: each community's planning horizon and legal security become functions of administrative decision rather than fixed rules. Substantively, it moves land (transfer zones opened 1920-1939, restricted 1940) and demographic futures (certificate grants and closures) between the communities by administrative act, and moves both communities' political resources into influence-seeking directed at London and Geneva.
% ABSENT_VOICES: The Permanent Mandates Commission is the paradigmatic absent-present voice: seated inside the procedure, it receives reports and hundreds of petitions and holds no binding power; its criticisms are minuted and dismissible. Independent arbitration is absent as a category - no forum exists where either community's reading could be ruled upon against the mandatory's interest. Individual petitioners appear in the record by the hundred and alter policy essentially never.
% DISAPPEARANCE_RATIONALE: If unreviewable interpretive discretion vanished overnight - replaced by binding arbitration or fixed textual meaning - both communities' entire political strategies (decades of investment in influencing the interpreter) would be void at a stroke; the 1939 White Paper's closures would be challengeable rather than sovereign; land transfers made under permissive regimes would face adjudication rather than standing as faits accomplis. This is not hypothetical: when the interpreter withdrew in 1947-48, the whole settlement architecture rearranged within months - the United Nations assumed adjudication, partition was planned, and both communities repositioned from lobbying London to contending with each other.
% FOUNDING_PROBLEM: Administering former Ottoman territories claimed by rival nationalist movements without either annexing them outright or granting immediate self-determination: Article 22 of the Covenant framed a 'sacred trust' of tutelage, with a single mandatory exercising authority on the League's behalf until the population could stand alone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Permanent Mandates Commission's own minutes record repeated criticism of the gap between tutelage rhetoric and practice; the Anglo-American Committee of Inquiry (1946) found the mandate had developed no self-governing institutions and commanded the confidence of neither community; UNSCOP (1947) reported the mandate unworkable and recommended termination. No source outside the administration attests that the founding problem remained live at interval end; the mandatory's own 1947 referral of the dispute to the United Nations concedes it.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.62 (matching the interval-end measurement) because the extraction is real but non-monetary and unevenly phased: what the discretion regime takes from both communities is strategic certainty, bargaining position, and the resources consumed courting the interpreter - permanent delegations in London, annual petition cycles in Geneva, and the political capital spent anticipating each swing. The oscillation is the mechanism, not noise: each white paper demonstrated that no concession or understanding was bankable, and each community's rational response - invest in influencing the next swing - is itself the transfer being extracted. Intermittent reinforcement (gains granted then withdrawn: 1930 reversed in 1931; partition proposed in 1937, effectively shelved, then reversed in 1939) keeps both payers paying. Suppression is authored at 0.60 as a raw structural property (unscaled by the engine; only extractiveness is scaled): the absence of any binding forum is the standing coercive fact, reinforced episodically by extraordinary force during the 1936-39 revolt. Theater_ratio reaches 0.58 because much of the regime's visible activity was performative accountability: commissions of inquiry whose findings were adopted or discarded at will, a petitions process that altered essentially nothing, and 'preparation for self-government' rhetoric unchanged from 1922 (when the proposed legislative council was abandoned after the Arab boycott) to 1947 (when self-government had still not arrived). Accessibility_collapse at 0.62: once both communities understood the interpreter was unbound, the alternative of relying on fixed textual meaning collapsed completely, leaving only within-system strategies - lobbying, leverage, force - which are real channels but not exits from the discretion itself. Resistance at 0.75 reflects the highest-intensity response anywhere in the mandate system: the 1936 general strike and three-year Arab revolt, followed by Jewish insurgency once the 1939 White Paper closed the gates. The three measurement series share one eight-point grid (years elapsed from 1920: T0=1920 ... T28=1948); the aggregate series integrate the policy-level oscillation, whose poles (1930/31, 1937/39) appear as slope changes rather than separate cycles. Fixing_cost is prohibitive for the seat that could fix it: submitting to external arbitration or fixing textual meaning would convert flexible positions into committed ones, betray one community catastrophically, and invite great-power friction - the option value of discretion exceeded the repair cost throughout. Boltzmann note: enforcement_mechanism carries a 0.10 floor that is generous for a regime whose coordination content for the governed is thin; excess extraction above the floor is expected to flag.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the administration's seat the arrangement is faithful stewardship of an internally contradictory instrument: no interpretation could honor both communities' readings simultaneously, so discretion was not theft but necessity, and each white paper a good-faith rebalancing. From either community's seat the same structure is unaccountable rule: a counterparty that writes, interprets, enforces, and revises its own commitments, whose every concession arrives revocable. From the Commission's seat the salient fact is its own impotence - a supervision system whose minutes constitute the best evidence against it. The divergence is structural (role and exit), not informational: all seats saw the same white papers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: the administration sits near the beneficiary pole (collects flexibility, divides opposition, governs without negotiated consent); both communities derive near the target pole. One override is authored: derived d for the organized victim seats would read as full target (~1.0), but each community collected real transfers during favorable swings - land and immigrants for the yishuv through 1939, protective zoning and immigration closure for the Arab community thereafter - making them targets with episodic, revocable subsidies rather than pure targets; d = 0.87 encodes that episodic relief. The alternation is the point: the same discretion that subsidizes one community in one phase extracts from it in the next, which is why both remain structurally trapped in the interpreter's orbit rather than exiting into opposition to the arrangement as such. Suppression remains a raw structural input throughout; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The tutelage framing ('sacred trust... until able to stand alone') invites a scaffold misclassification: a transitional arrangement carrying its own sunset. No enforceable sunset existed - the Covenant's transitional language carried no mechanism, no date, and no review with teeth, and the arrangement's actual justification became the flexibility itself, not the transition. Reading the discretion layer as the constraint (rather than the administrative services beneath it) prevents the opposite error too: courts, registries, and municipal order were real coordination, but this story's constraint is the unreviewable interpretive supremacy layered above them, whose coordination content for the governed is nil - which is what makes snare, not tangled_rope, the honest claim despite real administration occurring underneath. mandatrophy_resolved is declared true: the founding problem (tutelage toward self-government) died unfulfilled - the mandate terminated in 1948 with self-government never instituted - while the arrangement persisted to the last day; the R5 mismatch (dead founding problem x world_rearranges) is the expected capture/zombie signature for this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the balfour_mandate_instruments kernel constitutes the operative constraint - a substantive direction (Jewish national home primacy, or dual obligation to the existing population) or the mandatory''s unilateral interpretive discretion itself?',
    'Counterfactual and comparative test: if the instrument''s text had been self-executing under any reading, policy would have tracked the text regardless of who administered; observed policy instead tracks the administering power''s successive choices, supporting the discretion reading - but the contest is conceptual, resolvable only by agreement on what counts as ''the constraint''.',
    'Under either substantive sibling, victims are one-sided (the Arab community under primacy; the Zionist program under dual-obligation) and epsilon concentrates accordingly; under this reading both communities are victims in alternation and the beneficiary is the interpreter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel membership: this story instantiates the procedural-discretion reading; sibling stories instantiate substantive-direction readings of the same instruments.').

omega_variable(
    oscillation_incidence_asymmetry,
    'Is the harm of interpretive oscillation actually symmetric-in-turn, or does cumulative incidence fall predominantly on one community (irreversible Arab land loss versus reversible Jewish immigration caps)?',
    'Longitudinal land-title registry analysis and demographic accounting across the interval: compare the irreversibility profiles of losses suffered under adverse swings for each community.',
    'A decisively asymmetric ledger would shift the story toward the sibling readings'' victim structures (a single systematic victim) and raise effective epsilon for the disadvantaged seat; a genuinely alternating ledger supports this reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_incidence_asymmetry, empirical, 'Whether alternating discretion harmed both communities equally in cumulative, irreversible terms.').

omega_variable(
    coalition_against_interpreter_feasibility,
    'Why did two communities both harmed by unbound discretion never combine against the interpreter - was joint action infeasible because their claims were mutually exclusive, or was it prevented?',
    'Examine the record of Arab-Jewish negotiation contacts and joint proposals across the interval; distinguish identity-level incompatibility of claims from enforcement-driven isolation of moderates.',
    'If coalition was feasible but prevented, suppression is understated and the regime''s stability owes more to active enforcement; if infeasible, the discretion regime''s persistence is subsidized by the victims'' own incompatibility - extraction sustained by victim structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_against_interpreter_feasibility, empirical, 'Whether divide-and-rule exploited pre-existing incompatibility between the communities or manufactured it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.3).
narrative_ontology:measurement(balf_tr_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 4, 0.33).
narrative_ontology:measurement(balf_tr_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 8, 0.37).
narrative_ontology:measurement(balf_tr_t12, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 12, 0.44).
narrative_ontology:measurement(balf_tr_t16, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 16, 0.51).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 20, 0.55).
narrative_ontology:measurement(balf_tr_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 24, 0.57).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.58).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(balf_be_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(balf_be_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(balf_be_t12, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(balf_be_t16, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(balf_be_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(balf_su_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(balf_su_t8, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(balf_su_t12, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(balf_su_t16, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(balf_su_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% Colloquial references to 'the Mandate' or 'the Balfour commitment' conflate three structurally distinct constraints instantiated from one kernel (balfour_mandate_instruments): jewish_national_home_primacy (directive toward Jewish sovereignty; epsilon concentrated on the Arab community), dual_obligation_indigenous_rights (protective obligations superior to the national-home clause; epsilon concentrated on the Zionist program), and this file's mandatory_interpretive_discretion (procedural: unreviewable interpretive authority; moderate epsilon borne by both communities in alternation, captured by the administering power). This story authors only the procedural reading; the epsilon differences across the family are the decomposition's point. The upstream substantive texts feed this reading: each substantive claim supplies material for the interpreter to adjudicate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, organized, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
