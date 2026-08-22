% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Strict Neutrality Secular Settlement (State Equidistance Reading)
 *   domain: constitutional/political/religious_governance
 *
 * SUMMARY:
 *   A constitutional settlement bars the state from preferring, funding,
 *   endorsing, or interfering with any religion: one uniform legal order,
 *   equidistant from every faith. The arrangement solves a real
 *   collective-action problem — in a plural polity, open competition for
 *   state preference escalates into persecution — and it is actively
 *   enforced: courts strike down preferential measures and refuse invitations
 *   to police communal interiors. The same wall that protects every community
 *   from the state protects every community's internal hierarchy from its own
 *   members: women, dissenters, and hereditary underclasses inside the
 *   communities cannot carry their grievances to the state, and reform
 *   movements must win unassisted against entrenched leadership. Meanwhile
 *   the majority's cultural defaults seep into facially neutral law as the
 *   unmarked baseline. This file instantiates ONE reading of the contested
 *   constitutional_secularism kernel — the strict_neutrality_reading — and
 *   authors epsilon for the standing strict-neutrality arrangement as this
 *   reading's own lights assess it; the sibling readings are separate
 *   constraints linked in network.affects_constraints. Claim/metric
 *   independence: the settlement is CLAIMED as tangled_rope (genuine
 *   coordination plus asymmetric extraction through one structure) and the
 *   metrics are authored to describe its actual operation, independently. KEY
 *   AGENTS (by structural relationship): - constitutional_courts: Agenda
 *   setter (institutional/identity_locked) — administers the settlement,
 *   strikes down preference, declines to reach inside communities -
 *   established_religious_leaderships: Primary beneficiary
 *   (organized/constrained) — hold unreviewable internal authority -
 *   vulnerable_members_within_communities: Primary target (powerless/trapped)
 *   — bear the insulation of internal hierarchy - religious_minorities:
 *   Dual-positioned (organized/constrained) — protected from majority
 *   capture, exposed to majority-norm defaults - dominant_cultural_majority:
 *   Indirect beneficiary (powerful/mobile) — its defaults become the neutral
 *   baseline - intra_community_reform_movements: Secondary target
 *   (moderate/constrained) — denied state backing for internal reform -
 *   national_legislature: Constrained agenda setter (powerful/constrained) —
 *   writes uniform law with religious instruments removed
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.52).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.58).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Strict Neutrality Secular Settlement (State Equidistance Reading)").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional/political/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'f8e1a492-dda6-4a6f-b59e-3f1149b932c2').
narrative_ontology:cs_kernel_codification('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', fixed_text).
narrative_ontology:cs_authority_grounding('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', lineage).
narrative_ontology:cs_interpretation_layer_present('f8e1a492-dda6-4a6f-b59e-3f1149b932c2').
narrative_ontology:cs_reading_relation('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', constitutional_secularism__principled_intervention_reading, forecloses).
narrative_ontology:cs_reading_relation('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', constitutional_secularism__reformist_reading, forecloses).
narrative_ontology:cs_axiom('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', foundational, categorical_state_abstention_from_religious_affairs).
narrative_ontology:cs_axiom_status(categorical_state_abstention_from_religious_affairs, holdable).
narrative_ontology:cs_axiom_grounding('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', categorical_state_abstention_from_religious_affairs, deontological).
narrative_ontology:cs_axiom('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', secondary, uniform_law_without_communal_carveouts).
narrative_ontology:cs_axiom_status(uniform_law_without_communal_carveouts, holdable).
narrative_ontology:cs_axiom_grounding('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', uniform_law_without_communal_carveouts, conventional).
narrative_ontology:cs_reference_frame('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', strict_equidistance_noninterference).
narrative_ontology:cs_drift_state('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', contemporary_pluralist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8e1a492-dda6-4a6f-b59e-3f1149b932c2', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, established_religious_leaderships).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, dominant_cultural_majority).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, vulnerable_members_within_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_reform_movements).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, national_legislature).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, established_religious_leaderships).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, dominant_cultural_majority).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, non_establishment_principle).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, equal_citizenship_regardless_of_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which laws survive scrutiny under the non-establishment and free-exercise provisions, striking down measures that confer preference on any faith and declining invitations to police conduct inside religious communities. Their docket is dominated by boundary cases — funding, symbols, schooling, personal-law questions — and their institutional self-understanding is bound up with guarding the settlement they administer; stepping outside it would mean renouncing the role the text assigns them.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).

% Writes the facially uniform statutes — tax codes, education rules, calendars, charity law — through which the settlement touches daily life, and periodically faces majoritarian pressure to fund, honor, or accommodate the majority faith. Every such proposal fails against the neutrality rule, so the chamber operates with one whole category of policy instrument removed from its kit while still setting the terms of ordinary law.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, national_legislature, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, national_legislature, payer).

% Keep final say over doctrine, membership, family law, and internal discipline without external review, because the state stands equidistant and does not reach inside. What they forgo is access to state patronage, endorsement, and subsidy that an aligned regime would channel to them; what they keep is an unchallengeable interior.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, established_religious_leaderships, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, established_religious_leaderships, payer).

% Gain assurance that the state will not be turned against them by majority vote — no established church to fund rivals, no tests for office. The same uniformity cuts the other way: statutes drafted around majority habits (holidays, dress, food, weekly rest timing) land on them as burdens they cannot ask to have adjusted, since adjustment is precisely what the settlement forbids the state to grant. Emigration or assimilation are the only exits, and both are costly.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, religious_minorities, payer).

% Women, dissenters, and hereditary lower-status members inside religious communities live under authorities the state has promised not to touch. When marriage, divorce, inheritance, or discipline turn coercive, the courthouse door that opens for everyone else stays shut for them: the rule that shields their community from persecution also shields its internal hierarchy from their challenge. Leaving the community means losing family, livelihood, and social world at once.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, vulnerable_members_within_communities, payer,
    powerless, biographical, trapped, national).

% Organize to change practices from within — reforming personal law, opening clergy ranks, revising communal schooling — and must win entirely against entrenched leadership, because the state will neither tip the scales for them nor hear appeals against communal authorities. Their campaigns run longer and succeed less often than they would under a state permitted to back reform.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_reform_movements, payer,
    moderate, generational, constrained, national).

% Loses the formal establishment its numbers could command — no tax stream, no legal primacy, no oath architecture favoring its rites. In exchange, its inherited defaults quietly become the unmarked baseline: the statutory calendar, the common-law family forms, the public-school rhythm all rhyme with its customs without anyone legislating a preference. Its culture permeates the neutral order regardless of where its members worship.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, dominant_cultural_majority, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, dominant_cultural_majority, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, established_religious_leaderships).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the sectarian-competition problem: in any religiously plural polity, every faith has reason to seek state preference, and mutual capture efforts escalate into persecution and civil conflict. The settlement removes the prize — the state may not distribute advantage along religious lines — and supplies one uniform legal order that applies identically across communities.
% TRANSFER_FUNCTION: Moves immunity from state interference to every religious community equally; moves decision authority over doctrine, family law, and internal discipline wholly inside each community; and, as the price of that interior sovereignty, moves the cost of intra-communal coercion onto the members least able to resist it, who lose the state as a forum of appeal. It also disqualifies confessional alignment as a basis for distributing public legitimacy.
% ABSENT_VOICES: Vulnerable members inside the communities — women, dissenters, hereditary underclasses — are absent as distinct voices: the settlement recognizes communities, represented by their existing leaderships, as the units of religious freedom, so those with the most at stake inside speak only through those the arrangement insulates. They would testify that equal distance between communities is not equal protection within them.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, every community would immediately petition the state for preference and protection against rivals; parties would reorganize along confessional lines to capture the newly available instrument; minorities would fortify or emigrate; and the informal majority-default accommodations now tolerated under neutrality would harden into explicit legal primacy for whichever faith held office.
% FOUNDING_PROBLEM: Confessional states: the historical pattern in which rulers aligned with one faith taxed, tested, and persecuted the others, culminating in wars of religion and cycles of retaliatory persecution that no sect could win outright.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the wars of religion and confessional persecution attests the founding problem and its severity; comparative politics of contemporary sectarian conflict — states that slid into confessional capture where no settlement held — corroborates that the problem remains live wherever religious pluralism meets state capacity. Advocates for vulnerable members corroborate the problem's reality from outside the benefiting parties while disputing that this settlement answers it: corroboration of the problem, contestation of the remedy.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits mid-range (0.52) and climbs across the interval: the settlement's coordination core — removing sectarian preference from state power — is real and load-bearing, but two extraction channels grow steadily. First, facially neutral law accretes majority-norm content (calendars, family-law forms, schooling rhythms), taxing minorities without any statute naming them. Second, informal accommodations to the majority faith accumulate faster than doctrine forbids them, so practiced neutrality lags professed neutrality. Suppression (0.58) is a raw structural property, unscaled by power or scope: the binding force falls hardest on the most powerful party — the state itself is barred from a whole class of action — while for trapped members inside communities the operative suppression is the closed courthouse door. Theater ratio (0.30) tracks the widening gap between ceremonial neutrality declarations and accommodated practice. The suppression_requirement series rises because enforcement machinery genuinely matured over the interval — doctrine elaborated, litigation professionalized, review routines hardened — not as a proxy for other movement. All three series share one seven-point time grid; no oscillation is modeled because the drift is monotonic rather than cyclical. Receipt surface: the settlement's gains accrue concentrately to the leaderships (interior sovereignty collected directly), diffusely to the majority (unmarked baseline-setting); fixing cost is prohibitive for whoever could fix it — entrenched text, supermajority amendment gates, and a reopened sectarian-competition problem await any remover.
 *
 * PERSPECTIVAL GAP:
 *   From the courts' and leaderships' seats the settlement computes as coordination: a peace treaty among faiths, administered even-handedly, worth its costs. From the trapped member's seat the identical wall reads as extraction: the state that would hear any other grievance has contractually deafened itself to hers. The engine computes these per-seat classifications from the structural data — same epsilon referent, different directionalities — and the divergence between the agenda-setter seats and the payer seats is the measurement this story exists to take. The authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Established leaderships sit near the beneficiary pole: the settlement subsidizes their interior sovereignty, and their constrained exit (they operate inside the constitutional order they benefit from) keeps them invested in it. The dominant cultural majority collects indirectly — its defaults become the unmarked baseline — placing it below symmetric despite forfeiting formal establishment. Religious minorities straddle: autonomy gained pulls directionality down, majority-default burdens pull it up, netting slightly below symmetric; they are declared in both the beneficiary and victim arrays because both flows are structural, not rhetorical. Vulnerable members and reform movements sit near the full-target pole: they receive the settlement's protections only at the community boundary and pay inside it, with trapped and constrained exits amplifying exposure. The state's own seats (courts, legislature) are bound administrators rather than collectors — the settlement constrains them without enriching them, so their directionality reflects administration, not capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — confessional capture and sectarian persecution — remains live wherever religious pluralism meets state capacity, so this is not a piton maintained by inertia nor a scaffold awaiting sunset: the mandate has not outlived its function, and mandatrophy is unresolved. Classifying the settlement as tangled_rope rather than snare preserves the fact that removing it would not simply liberate — it would reopen the sectarian-competition problem the settlement solved — while the victim declarations block the opposite error of reading the coordination story as the whole truth. The arrangement still does what it was built to do, at a price its founders did not itemize; the temporal series showing accumulating extraction is the record of that unitemized price compounding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_displacement,
    'Which reading of the constitutional_secularism kernel governs — strict equidistance, principled intervention, or reformist duty?',
    'Track landmark constitutional adjudication and amendment: a court or constituent process adopting intervention authority for reform purposes displaces this reading''s constraint wholesale.',
    'Under the intervention readings the victim set changes — vulnerable members gain a state forum, leaderships lose insulation — and epsilon for the governing arrangement shifts: extraction on the currently-shielded institutions surfaces while the insulation cost borne by trapped members converts into coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_displacement, conceptual, 'Kernel-level contest: this constraint is one reading of constitutional_secularism; sibling readings instantiate different constraints with different victim sets and epsilon values.').

omega_variable(
    neutrality_as_majority_default_embedding,
    'Are the facially neutral statutes genuinely community-blind, or do they systematically encode the majority''s religious-cultural defaults?',
    'Comparative incidence analysis of neutral-law burdens (statutory calendars, dress and food regulation, family-law forms, weekly rest timing) across communities within the same jurisdiction.',
    'If burdens track community lines, the uniform-application framing understates minority-seat extraction and the arrangement drifts toward extraction-dominant for minority seats; if burdens are roughly symmetric, the coordination reading strengthens and epsilon should fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_as_majority_default_embedding, empirical, 'Whether formal neutrality conceals systematic majority-norm content in ostensibly uniform law.').

omega_variable(
    insulation_cost_attribution,
    'Are the harms borne by vulnerable members inside communities attributable to this settlement''s non-interference rule, or to the communities'' internal structures that would persist under any state posture?',
    'Cross-jurisdiction comparison of outcomes for equivalent populations under strict-separation versus intervention-authorized regimes, holding community internals constant where possible.',
    'If attribution to the settlement is strong, epsilon is understated by treating those harms as external to this constraint; if weak, part of the measured extraction belongs to separate intra-communal constraints and this story''s epsilon should fall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insulation_cost_attribution, conceptual, 'Attribution of intra-communal harm between this settlement''s non-interference rule and the communities'' own internal structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__strict_neutrality_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__strict_neutrality_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__strict_neutrality_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__strict_neutrality_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__strict_neutrality_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(cons_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 25, 0.51).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(cons_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(cons_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% 'Constitutional secularism' is a contested kernel, decomposed per the epsilon-invariance principle into three stories. This file instantiates the strict_neutrality_reading (categorical abstention; victims are the insulated communities' weaker members and reform movements). principled_intervention_reading licenses reform-motivated intervention (different victim set: communal hierarchies lose immunity). reformist_reading imposes an affirmative eliminative duty superseding autonomy claims (strongest intervention posture). Each carries its own epsilon, beneficiaries, and victims. Upstream/downstream pressure runs from this reading's settled doctrine toward the intervention readings' legitimacy conditions — the strict reading's entrenchment raises the cost of adopting either sibling — which is why the influence edges point outward from this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
