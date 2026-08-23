% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation Kernel — Principle Reading (Structural Non-Entanglement Test)
 *   domain: religious/technological/commitment-systems
 *
 * SUMMARY:
 *   An Old Order Anabaptist community governs technology adoption through its
 *   Ordnung, read here through the PRINCIPLE reading of the
 *   gelassenheit_separation kernel: separation means avoiding structural
 *   entanglement in worldly systems, and a technology is acceptable when it
 *   functions in isolation from external infrastructure. Solar arrays
 *   charging battery banks, pneumatic tools fed by shop compressors, and
 *   off-grid diesel generation pass the test; grid electricity, commercial
 *   insurance, and the internet fail it categorically, because their use is
 *   itself the connection — no isolated implementation of a pooling contract
 *   or a global network exists in the reading's eyes. Enforcement runs
 *   through the ordained ministry: semiannual Ordnung affirmation,
 *   case-by-case adjudication of new devices, confession for violations, and
 *   shunning for persistence. The epsilon referent is the standing Ordnung
 *   arrangement under contest, assessed by this reading's own lights — the
 *   reading endorses the non-entanglement principle, but the story authors
 *   epsilon for the arrangement as it actually binds members, not for the
 *   arrangement the reading would praise. This story is one member of a
 *   three-story constraint family: the artifact_reading (visible-distinction
 *   test) and the consequence_reading (community-practice-effects test) are
 *   separate files with their own epsilon, beneficiaries, and
 *   classifications, linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ordained_ministry: Agenda setter (institutional/identity_locked) — administers the Ordnung, adjudicates each technology against the separation standard, imposes confession or shunning; the office, vocation, and social world stand or fall together
 *   - church_community_collective: Primary beneficiary (organized/constrained) — the baptized body that receives cohesion, mutual-aid solvency, and generational continuity from the arrangement
 *   - technology_restricted_members: Primary target (moderate/identity_locked) — baptized adults who bear the forgone channels: no online sales, no commercial policies, no grid power; exit costs family, faith, and trade simultaneously
 *   - catastrophic_loss_households: Residual-risk bearers (powerless/trapped) — households whose losses exceed routine mutual aid, bound tightest at the moment the arrangement serves them least
 *   - compliant_off_grid_adopters: Permitted-tech beneficiary (moderate/constrained) — members whose solar and pneumatic setups prove the reading's test livable and who defend the rule accordingly
 *   - rumspringa_youth: Pre-commitment excluded voice (powerless/mobile) — unbaptized adolescents who sample the outside world, sit outside the councils writing the rules they will inherit, and can still walk away cheaply
 *   - english_market_partners: Outside observer (moderate/arbitrage) — customers, wholesalers, and county officials who transact around the arrangement's edges and can redirect patronage at will
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.48).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.61).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation Kernel — Principle Reading (Structural Non-Entanglement Test)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technological/commitment-systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, 'b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086').
narrative_ontology:cs_kernel_codification('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', fixed_text).
narrative_ontology:cs_authority_grounding('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', lineage).
narrative_ontology:cs_interpretation_layer_present('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086').
narrative_ontology:cs_reading_relation('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', foundational, separation_is_structural_nonentanglement).
narrative_ontology:cs_axiom_status(separation_is_structural_nonentanglement, holdable).
narrative_ontology:cs_axiom_grounding('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', separation_is_structural_nonentanglement, theological).
narrative_ontology:cs_axiom('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', secondary, functional_isolation_suffices_for_tool_use).
narrative_ontology:cs_axiom_status(functional_isolation_suffices_for_tool_use, holdable).
narrative_ontology:cs_axiom_grounding('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', functional_isolation_suffices_for_tool_use, instrumental).
narrative_ontology:cs_reference_frame('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', schleitheim_gathered_church_separation).
narrative_ontology:cs_drift_state('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', contemporary_smartphone_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b3d22fc6-ce2e-41f0-baa1-9b8d20dbc086', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, church_community_collective).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ordained_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, compliant_off_grid_adopters).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, technology_restricted_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, catastrophic_loss_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% District bishops and ministers maintain the Ordnung: they convene the semiannual affirmation, adjudicate each proposed device against the separation standard, impose confession or shunning on violations, and set the rules of the mutual-aid fund. Their office, vocation, and entire social world exist inside the community they govern; leaving would dissolve all three at once.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordained_ministry, agenda_setter,
    institutional, generational, identity_locked, regional).

% The baptized membership as a body pools risk through mutual aid, staffs barn raisings and parochial schools, and reproduces itself across generations. It receives the arrangement's cohesion and autonomy benefits; individual dissent leaves the body rather than changing it, so the collective's continuity is insulated from minority preference.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, church_community_collective, beneficiary,
    organized, generational, constrained, regional).

% Baptized adults whose trades or circumstances would benefit from the forbidden channels: woodshops that cannot list inventory online, farms that cannot hedge prices, households that cannot hold commercial policies. Open violation brings confession, and persistence brings shunning, which cuts the business and kinship ties their livelihood runs on. Departing the community altogether would cost family, faith community, and trade network in a single motion.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, technology_restricted_members, payer,
    moderate, biographical, identity_locked, regional).

% Households struck by losses beyond what mutual aid routinely covers — a burned workshop, a catastrophic medical bill, a liability judgment. At the moment of loss they meet the residual risk the insurance prohibition leaves with them; district aid arrives unevenly and there is no policy to draw on. Their binding to the arrangement is tightest exactly when the arrangement serves them least.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, catastrophic_loss_households, payer,
    powerless, immediate, trapped, local).

% Members running permitted equipment: solar arrays charging battery banks, pneumatic tools fed by shop compressors, diesel generators kept off the utility lines. The arrangement's own test works in their favor — modern output without grid connection — and they defend the rule as evidence that the standard is livable.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, compliant_off_grid_adopters, beneficiary,
    moderate, biographical, constrained, local).

% Adolescents not yet baptized: they sample cars, phones, and city work without being bound by the Ordnung, then face baptism into all its restrictions or departure. They sit outside the councils that write the rules they will inherit; their objections surface only as parental anxiety or as departures.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, rumspringa_youth, excluded,
    powerless, immediate, mobile, local).

% Non-community customers, wholesalers, and county officials who buy Amish goods, hire crews, and regulate the roads and schools around the settlements. They transact around the arrangement's edges — faxed orders, scheduled pickups — and can redirect their patronage elsewhere at will; they hold no seat in the community's deliberation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, english_market_partners, observer,
    moderate, biographical, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, church_community_collective).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains communal autonomy from external infrastructures and institutions: energy, risk-pooling, information flow, and market access are kept inside the congregation so that mutual aid, common worship, and communal discipline remain operable without external dependencies.
% TRANSFER_FUNCTION: Moves decision authority over technology adoption from individual households to the ordained ministry; moves risk-bearing from commercial insurers to congregational mutual aid; moves market access from direct digital channels to intermediary and physical channels.
% ABSENT_VOICES: Unbaptized youth would object to inheriting rules they had no hand in setting — they are present in the community but absent from its councils. Former members who left under discipline cannot speak in any forum the arrangement recognizes. Women, who hold no ordained office, register preferences only through household and informal channels. Catastrophic-loss households discover their objection only after the loss, when the relevant decision is long past.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, members would connect to the grid, buy policies, and open online sales channels within months; mutual-aid structures would thin as risk migrated to commercial pools; the community's distinct economy would merge with the surrounding market, and the ministry's adjudicating role would lose its object. The settlement's whole configuration — workshops, schools, aid funds, travel patterns — is arranged around the separation standard.
% FOUNDING_PROBLEM: Keeping the gathered church practicably separate from worldly systems: first under persecution, when survival required distance from state and established-church structures, and later against industrialization, which pulled members into wage labor, consumer markets, and externally administered risk pools that would dissolve mutual aid and communal discipline.
% FOUNDING_PROBLEM_CORROBORATION: Academic sociologists and historians of Anabaptism attest from outside the benefiting parties that assimilation pressure is real and ongoing — employment surveys, settlement studies, and technology-adoption research document the continuing pull of external labor markets and consumer infrastructure. Former-member memoirs corroborate the lived force of the same pressure. Neither source collects rents from the arrangement's persistence.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.48 — moderate, and deliberately lower than the artifact reading's profile would be: the principle test tracks a genuine harm (infrastructural dependency does corrode communal self-sufficiency), and it legitimizes workarounds, so fewer members are blocked from useful tools than under an appearance-based ban. But the costs are real: forgone e-commerce channels, unpriced catastrophic tail risk shifted onto the least powerful seat, and an information environment bounded at the district line. Suppression is 0.61 as a raw, unscaled structural property — shunning severs the economic and kinship graph a member's livelihood runs on, and the engine scales only extractiveness, never suppression. Theater is 0.22: enforcement is substantively functional, though the visible markers of compliance (no utility lines to the house) carry growing symbolic weight as boundary signals. Accessibility collapse is 0.45 — the arrangement does not collapse alternatives so much as replace them: grid power yields to solar, insurance yields to mutual aid, and the substitutes are internal by design, so the alternative set narrows without vanishing. Resistance is 0.45: covert phone use, youth pressure during the pre-baptism years, and recurring district-level disputes, including historical schisms when liberalization was attempted. The measurement series run on one shared time grid (all three metrics at t=0,8,16,24,32,40). The trajectories show a mild U-shape rather than a cycle: extraction and suppression decline through the middle of the interval as the isolation test clarifies and workarounds are legitimized, then turn upward as smartphones and e-commerce arrive — technologies whose entanglement is harder to fence — and enforcement must intensify against covert use. This is technology-wave-driven ratcheting, not intermittent reinforcement; the oscillation is a side effect of exogenous innovation, not an extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the ministry seat the arrangement is faithful stewardship: each ruling is an application of a shared standard the community affirms twice a year. From the restricted-member seat the same structure operates as a ceiling on livelihood — the craftsman watching competitors sell online experiences the ban as a tax he did not consent to in his current circumstances. The off-grid adopter seat experiences the reading as liberating, which is precisely the reading's design: it converts potential dissenters into defenders by permitting everything that passes the test. Same-level lateral divergence matters here: two baptized farmers hold identical power atoms, identical exit conditions, and identical formal standing, yet the one whose commodity ships fine through intermediaries and the one whose craft business dies without a web presence inhabit computationally different constraints — differentiated by occupation-specific exposure, not by rank. Across affiliated districts the same divergence appears institutionally: districts that permit shop phones have already relaxed the internet ban's edge, so the identical kernel reading computes with different enforcement intensity one county line away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The church_community_collective and ordained_ministry sit near the beneficiary end (low d): the arrangement subsidizes them with cohesion, solvency, and adjudicating authority, and neither pays the extraction. Compliant_off_grid_adopters sit low-to-moderate: they receive permitted-tool benefits and pay only the residual restrictions. Technology_restricted_members sit near the full-target end, and their identity_locked exit amplifies effective extraction — trapped or identity-locked targets sit nearer the full-target end than mobile ones, and here exit means simultaneous loss of family, faith community, and trade network, a triple fusion of relational, ideological, and professional identity. Catastrophic_loss_households carry the highest directional load: powerless, trapped at the moment of loss, bearing the residual risk the insurance prohibition created. Rumspringa_youth are excluded rather than coordinated — pre-baptism, they fall outside the derivation's beneficiary/victim structure and take the canonical fallback. English_market_partners hold the analytical seat: they observe and adapt but neither collect nor pay. No directionality overrides were needed: the declared structure produces the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Calling this a rope would erase the asymmetric extraction: identifiable victims exist, enforcement is active, and the costs land unevenly on members whose livelihoods the forbidden channels would most help. Calling it a snare would erase the genuine coordination: mutual aid genuinely pools risk internally, the community genuinely maintains autonomy and continuity, and the reading's own test distinguishes entangling from isolatable technology rather than suppressing all alternatives indiscriminately. The mandatrophy interview confirms the arrangement is not a zombie: the founding problem (assimilation pressure dissolving the gathered church into wage labor, consumer markets, and external risk pools) is live, corroborated from outside the benefiting parties, and matched by a world_rearranges disappearance verdict — no dead-mandate-plus-persistence mismatch fires. The constraint carries no sunset clause and is not transitional; it is a steady-state hybrid. The forward risk is drift, not obsolescence: if the inherent-entanglement omega resolves toward 'isolation-capable implementations exist and are banned anyway,' the coordination story thins and the computed type slides toward snare; the temporal series' late-interval uptick in both extraction and suppression is the early signature of exactly that pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This constraint is one reading of the gelassenheit_separation kernel: which structural test actually defines separation for a given community — artifact resemblance (artifact_reading), community-practice consequences (consequence_reading), or structural non-entanglement (this reading)?',
    'Comparative adjudication records across affiliated districts: which test districts cite when ruling on novel technologies, and whether rulings migrate between tests over time.',
    'If the artifact reading governs, epsilon rises sharply (function becomes irrelevant to permission) and the off-grid adopter seat empties; if the consequence reading governs, the victim set shifts toward relational harms (declining visiting, weakened mutual aid) and the internet/insurance bans become contingent rather than categorical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: this story instantiates the principle_reading; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    inherent_entanglement_category_boundary,
    'Are internet and commercial insurance genuinely incapable of functional isolation (their use IS the entanglement), or does the categorical ban exceed the reading''s own isolation test and function as boundary maintenance riding on principle language?',
    'Examine whether isolation-capable implementations exist and what happens where they are trialed: shared filtered terminals, proxy purchasing, association health plans. If communities admitting isolated implementations retain cohesion and autonomy, the categories were boundary markers, not entanglement necessities.',
    'If isolation-capable implementations are feasible and still banned, the reading''s authored epsilon understates extraction — the categorical bans are the identity-coordination cover story the FNL gaming risk warns about, and the computed type should drift toward snare. If genuinely inseparable, the bans are the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_entanglement_category_boundary, conceptual, 'Whether the internet/insurance prohibitions track the reading''s own test or exceed it.').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression structural (shunning sanctions, economic interdependence, geographic concentration) or internalized (Gelassenheit formation such that members experience compliance as devotion and the question of exit barely arises)?',
    'Post-exit suppression trajectory of leavers: if former members report durable fear, guilt, and severed-relation grief years after exit, a substantial internalized component is present; if adjustment tracks material reintegration alone, suppression was mostly structural.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them after exit, raising computed extraction for the identity_locked seats; if structural, remedies that open external options would release the pressure directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs. internalized composition of the constraint''s suppressive force.').

omega_variable(
    mutual_aid_tail_risk_coverage,
    'Does congregational mutual aid actually cover the catastrophic tail risks that commercial insurance would have pooled, or does the insurance prohibition leave catastrophic_loss_households bearing uncompensated ruin?',
    'Compare district aid-fund disbursement records against actuarial benchmarks for fire, medical catastrophe, and liability events; survey households experiencing above-threshold losses for recovery completeness.',
    'Inadequate tail coverage raises effective extraction concentrated on the powerless trapped seat and pushes the computed type toward snare; adequate coverage supports the coordination half of the tangled-rope reading and lowers epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_aid_tail_risk_coverage, empirical, 'Whether the insurance ban''s risk burden is genuinely absorbed by mutual aid.').

omega_variable(
    exit_cost_heterogeneity,
    'How uniform is the identity_locked exit condition across members — do age, gender, and occupation produce materially different exit costs, such that the average directionality of the payer seat misrepresents its distribution?',
    'Leaver cohort analysis: compare exit rates and post-exit outcomes for unmarried young men, married household heads, and widowed members; measure how much of the exit cost is kinship severance versus trade-network loss versus doctrine.',
    'High heterogeneity means the payer seat''s computed extraction is a blend of near-mobile and deeply trapped subpopulations; classification of the seat is sensitive to weighting, and coalition or subgroup remedies would target different subpopulations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_heterogeneity, empirical, 'Variability of exit costs within the nominally uniform member population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__principle_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__principle_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__principle_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__principle_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__principle_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__principle_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__principle_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__principle_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__principle_reading, base_extractiveness, 32, 0.47).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__principle_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__principle_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__principle_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__principle_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__principle_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__principle_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, amish_social_security_self_employment_exemption).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Amish separation from technology' decomposes into three structurally distinct constraints sharing one kernel (gelassenheit_separation). The artifact_reading bans by resemblance regardless of function — highest epsilon, victims include users of functionally harmless tools. The consequence_reading bans by community-practice effect — epsilon tracks relational degradation, victims identified retrospectively. This principle_reading bans by structural entanglement — lowest epsilon, permits isolatable tools, but categorically forbids internet and insurance on the claim that their use is inseparable from the entanglement. The upstream/downstream structure runs from this reading outward: consequence-leaning districts cite the non-entanglement rationale as justification for their rulings, and the artifact reading survives as the traditional baseline this reading refines. Each file carries its own epsilon, beneficiaries, victims, and classification; the family link enables contamination propagation analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
