% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority as Territorial Sovereignty: the State's Legitimate Right to Exclude (Sovereignty Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   A state claims authority to exclude non-members from its territory as a
 *   constitutive attribute of territorial sovereignty, and operates an
 *   enforcement apparatus — legislated entry categories, patrols, detention,
 *   removal, visa and carrier regimes, and increasingly externalized and
 *   biometric processing — that makes the claim effective. The standing
 *   arrangement under contest, and the epsilon referent for this story, is
 *   that exclusion regime as the sovereignty reading itself sees it: the
 *   reading holds exclusion legitimate (a verdict about justification), while
 *   the authored metrics describe the arrangement's actual operation —
 *   severe, concentrated, non-consented costs borne by identifiable persons,
 *   sustained by active coercion. The claim (tangled_rope: a genuine
 *   bounded-demos coordination function carrying asymmetric extraction) and
 *   the metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data, and where the computed type
 *   diverges from the reading's self-presentation, that divergence is the
 *   measurement. Excluded migrants and deportable residents are the declared
 *   victims; citizens and the enforcement apparatus are the declared
 *   beneficiaries; the arrangement's operation also vindicates the
 *   territorial-sovereignty and bounded-self-determination doctrines, which
 *   collect no rents and are recorded as vindicated propositions, not
 *   beneficiaries.
 *
 * KEY AGENTS:
 *   - territorial_state_government: agenda-setter (institutional/arbitrage) — claims, legislates, and administers the exclusion power; can restructure or dismantle the regime by ordinary law
 *   - citizens_of_the_state: primary beneficiary (organized/constrained) — holds the membership goods the border protects; funds enforcement; its electoral consent is the legitimacy source the reading invokes
 *   - excluded_migrants: primary target (powerless/trapped) — bears refusal, detention, deportation, and route mortality; no vote and no standing in the process that rules them
 *   - deportable_resident_migrants: secondary target (powerless/trapped) — residence converted to permanent precarity inside the boundary
 *   - immigration_enforcement_agencies: enforcement administrator (institutional/identity_locked) — budgets and institutional purpose constituted by enforcement intensity
 *   - migrant_sending_communities: diffuse cost-bearers (powerless/trapped) — lose members to removal and to dangerous routes; object without standing
 *   - domestic_courts: analytical observer (institutional/analytical) — adjudicate the exclusion power's limits, absorbing drift through interpretation
 *   - international_human_rights_bodies: excluded voice (organized/constrained) — monitor and condemn enforcement practice with persuasive rather than binding force under the sovereignty framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.75).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.82).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority as Territorial Sovereignty: the State's Legitimate Right to Exclude (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '5aab82a6-0f76-4661-847b-9ed0ba4b2e6a').
narrative_ontology:cs_kernel_codification('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', formalized).
narrative_ontology:cs_authority_grounding('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a').
narrative_ontology:cs_reading_relation('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', foundational, territorial_sovereignty_confers_exclusion_prerogative).
narrative_ontology:cs_axiom_status(territorial_sovereignty_confers_exclusion_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', territorial_sovereignty_confers_exclusion_prerogative, conventional).
narrative_ontology:cs_axiom('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', foundational, legitimate_association_right_includes_exclusion).
narrative_ontology:cs_axiom_status(legitimate_association_right_includes_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', legitimate_association_right_includes_exclusion, deontological).
narrative_ontology:cs_reference_frame('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5aab82a6-0f76-4661-847b-9ed0ba4b2e6a', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizens_of_the_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, immigration_enforcement_agencies).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, deportable_resident_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, migrant_sending_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, citizens_of_the_state).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, bounded_demos_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates entry categories, operates the border, and claims the authority to refuse admission as an attribute of its sovereignty. Funds and directs enforcement, absorbs the diplomatic friction and enforcement costs, and can amend, tighten, or dismantle the whole arrangement by ordinary legislation. No external actor compels its membership rules.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, territorial_state_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold membership in the political community the border defines: preferential access to the labor market, welfare eligibility, and the vote. Pay for enforcement through taxation and supply the electoral consent the arrangement's legitimacy rests on. Emigration is possible but bound up with family, language, and livelihood, so leaving is costly. Some bear diffuse costs — enforcement budgets, contested labor-market effects, and the moral and diplomatic weight of enforcement practices.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizens_of_the_state, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, citizens_of_the_state, payer).

% Seek entry for work, family reunification, or safety and are refused under the entry rules. The legal channels that remain mostly do not apply to them; the alternative is irregular routes policed by the same enforcement apparatus — desert crossings, sea routes, smugglers — where thousands die annually. They are detained and deported when caught. They have no vote and no standing in the political process that writes the rules they are governed by.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Live and work inside the territory without secure status. Their residence persists at the state's discretion: they can be detained and removed, which makes them exploitable as labor and keeps their families under separation risk. Many have spent decades in the community — working, raising children, paying taxes — without a path into membership.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, deportable_resident_migrants, payer,
    powerless, biographical, trapped, national).

% Are the villages, cities, and states people leave or are returned to. They lose members to removal and to dangerous routes, depend on remittances that their members' precarity both generates and destabilizes, and object through diplomatic channels while holding no seat in destination-state politics. Most maintain their own border controls, attesting the general structure while contesting its application to their nationals.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, migrant_sending_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, migrant_sending_communities, excluded).

% Run patrols, detention, removal, surveillance, and increasingly externalized and biometric processing. Their budgets, headcount, and institutional purpose grow with enforcement intensity; operational practice — externalization agreements, screening technology, pushback methods — is shaped inside the agency as much as by legislation. Their mission, identity, and careers are constituted by enforcement work.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, immigration_enforcement_agencies, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, immigration_enforcement_agencies, beneficiary).

% Adjudicate the boundary between the sovereign power to exclude and individual rights: detention conditions, removal procedure, asylum access. Their rulings qualify enforcement practice without revisiting the sovereignty premise itself; case law absorbs change incrementally.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, domestic_courts, observer,
    institutional, generational, analytical, national).

% Monitor border deaths, pushbacks, and detention conditions; issue findings, recommendations, and judgments under human rights treaties. They hold no power to compel the territorial state; under the sovereignty framework their objections carry persuasive rather than binding force.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, citizens_of_the_state).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constitutes and maintains the boundary of a political community: it makes 'the people' a definite unit capable of self-government, and organizes membership-dependent allocation — labor-market access, welfare eligibility, political voice — around that boundary.
% TRANSFER_FUNCTION: Moves freedom of movement and secure settlement away from would-be entrants and from residents without secure status, converting them into protected membership goods — preferential labor access, welfare eligibility, political voice, and the state's sovereignty claim itself — for citizens of the state.
% ABSENT_VOICES: The excluded themselves: would-be migrants have no vote, no representation, and no standing in the political process that defines the rules governing them; sending communities object through diplomacy without a seat; international human rights bodies issue findings that bind no territorial state under the sovereignty framework. The arrangement's domestic unanimity is partly an artifact of these seats never having been in the room.
% DISAPPEARANCE_RATIONALE: If the exclusion power vanished overnight, the bounded demos would dissolve as a definite allocation unit: welfare states, labor markets, and electoral constituencies would reorganize around new membership principles; the hundreds of millions of people living outside their state of birth would face transformed option sets immediately; and the state's sovereignty claim would lose its territorial-exclusion core. The arrangement is load-bearing for the modern state system — the world rearranges, it does not stay put.
% FOUNDING_PROBLEM: How a 'people' becomes a definite, self-governing unit with the authority to allocate membership and resources — the problem of constituting bounded political community in a world of mobile populations and rival jurisdictional claims.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: migrant-sending states enforce their own borders and maintain sovereignty doctrine in international law while contesting this state's application of it — attesting the general structure from a non-beneficiary seat; political theorists who reject the exclusion conclusion still accept that bounded political communities face a real membership-allocation problem; and the near-universal recognition of territorial jurisdiction attests the arrangement's reality. No source outside the benefiting parties attests that the founding problem justifies the current enforcement intensity — that attestation comes only from the state and its citizens.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.75) because the arrangement imposes severe, asymmetric, non-consented costs on identifiable persons — foreclosed movement and life prospects indexed to birthplace, detention, family separation, and documented route mortality — while the good it produces accrues to members. The sovereignty reading's endorsement addresses justification, not the magnitude of the cost: by the reading's own lights the costs are real and borne by the excluded, held to be outweighed by the self-determination good; epsilon measures the extraction, the reading's axiom supplies its defense. Suppression is authored high (0.82) and is predominantly structural — physical barriers, patrols, detention, removal, carrier sanctions, externalized processing — not internalized; a minor internalized component operates on the citizen side (the border naturalized as simply how countries work), which does not carry the scalar. Theater is moderate-low (0.31): enforcement functionally excludes people, but a growing share of activity is performative — barrier construction that routes rather than stops flows, sovereignty signaling — while remote biometric processing is comparatively functional, hence the small late-series dip. Accessibility_collapse (0.62): legal channels have collapsed for most would-be entrants, but irregular routes persist at scale — they are the enforcement surface of the arrangement itself, not alternatives to it. Resistance (0.58): irregular crossing, sanctuary practices, litigation, and abolitionist advocacy are persistent; they do not aggregate into power because the arrangement's first move is to strip its targets of standing — coalition potential among the powerless is structurally suppressed. All three tracked series run on one shared time grid (1914, 1945, 1970, 1990, 2005, 2015, 2025) with every metric authored at every point. The suppression_requirement series is authored because this story specifically traces a century of enforcement-capacity build-up — from wartime passport regimes through securitization to externalized digital enforcement — not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the state and citizen seats the arrangement is a coordination mechanism they author, fund, and consent to: bounded self-determination with costs they accept. From the excluded migrant's seat the same structure is enforced extraction with no exit and no voice — trapped targets bearing full costs administered by a process they cannot touch. Citizens and migrants differ in global standing, but the operative differentiation is membership status: citizens are organized with constrained exit (identity-bound emigration), migrants are powerless with trapped exit (the alternative routes are the enforcement surface). Enforcement agencies add a third seat: institutional identity fusion — the agency's budget, mission, and personnel are constituted by enforcement intensity, so its exit is identity_locked and it experiences constraint revision as existential threat rather than policy change; if that identity frame broke (mandate conversion to migration management or humanitarian processing), the agency seat would recompute from beneficiary to neutral administrator. Domestic courts and international human rights bodies observe the same arrangement from interpretive and persuasive seats respectively, but neither sets membership policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end: the state government (d near 0.0 — the arrangement subsidizes its sovereignty claim and membership-allocation power, and it holds arbitrage-grade exit since it writes the rules), citizens (low d — membership goods accrue to them; their tax contribution is real but their net position is beneficiary), enforcement agencies (low d, with identity-lock amplifying stability — budgets and purpose flow from enforcement intensity). Targets sit near the target end: excluded migrants (d near 1.0 — full costs, trapped exit, no standing), deportable residents (near 1.0 — precarity is the arrangement operating on them from inside the boundary), sending communities (high d, diffuse — they lose members and remittance stability with no compensating good). The beneficiary/victim declarations map onto real structural relationships: the same fence that constitutes the citizen's political community is the instrument that kills the migrant at the border — one structure, opposite directionalities. Scope: enforcement authority is national but effects are global, and the human cost is incurred at remote, hard-to-verify sites; the engine's scope amplification registers on the extraction side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constituting a bounded, self-governing political community capable of allocating membership and resources — is live, so this is not a mandatrophy case: the arrangement has not outlived its function even by its critics' concession that some bounded allocation problem exists. The classification work is in preventing two mislabelings. Reading the arrangement as pure rope (the sovereignty reading's own public framing — 'every state controls its borders') would erase the asymmetric extraction: the costs are borne by people who never consented and cannot exit. Reading it as pure snare would erase the genuine coordination function: bounded-demos constitution is a real problem the arrangement does solve for members. Tangled rope holds both: coordination for the bounded community, extraction from the excluded, active enforcement required to sustain the asymmetry. The live drift risk is snare-ward: if the humanitarian exception is absorbed into enforcement (see omega asylum_exception_absorption) or the citizen-side benefit proves largely attributable to other policy instruments (see omega membership_benefit_attribution), the coordination side thins and the structure approaches pure extraction with the enforcement apparatus as concentrated beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the sovereignty_reading of the border_legitimacy kernel; what would change structurally under the sibling readings, and where exactly is the disagreement located?',
    'Authoring the sibling readings as separate stories in the same family: the freedom_of_movement_reading would move excluded persons from the victim set to the rights-holder set and re-author epsilon over the same standing arrangement near the top of the scale; the humanitarian_obligation_reading would partition the victim set by cause of flight, admitting refugee claims and retaining only non-fleeing migrants as cost-bearers.',
    'The disagreement is located in the foundational premise: whether authority over entry is a constitutive state prerogative or a presumptively-rebutable restriction on a human liberty. Epsilon, victim set, and classification are all reading-indexed, so cross-reading comparison must join on the kernel, not the constraint id; no single constraint-level epsilon exists for ''border legitimacy'' as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure of the border_legitimacy kernel: one kernel, three constraints.').

omega_variable(
    consent_scope_of_bounded_demos,
    'Does the self-determination justification require the consent of those the boundary excludes, or only the consent of members?',
    'Political-theoretical analysis plus revealed practice: if exclusion is defensible only with the excluded''s consent (which the arrangement makes impossible to obtain), the legitimacy claim reduces to member-preference and the justificatory cover for the measured extraction thins; if member-consent suffices, the arrangement''s legitimacy stands on the reading''s own terms.',
    'If member-consent suffices, the tangled_rope''s coordination side is stable at the reading''s own lights; if the excluded''s consent is required, the justificatory cover fails and the arrangement drifts toward snare even within this reading''s framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_scope_of_bounded_demos, conceptual, 'Scope of the consent that grounds the claimed right to exclude.').

omega_variable(
    externalized_cost_visibility,
    'Does externalized and digitalized enforcement reduce the human cost of exclusion, or only displace it beyond the reading''s observational reach?',
    'Comparative mortality and detention data at externalized sites (third-country processing, remote routes, offshore facilities) versus domestic enforcement sites, tracked over time.',
    'If costs merely displace, effective extraction is rising while visible enforcement intensity falls — the arrangement drifts toward snare with the enforcement apparatus as concentrated beneficiary; if costs genuinely fall, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_cost_visibility, empirical, 'Whether enforcement externalization displaces rather than reduces the costs borne by the excluded.').

omega_variable(
    asylum_exception_absorption,
    'Does the humanitarian exception (non-refoulement, asylum procedure) remain a genuine internal limit on the exclusion power, or is it being absorbed into enforcement machinery (externalized processing, safe-third-country chains, pushbacks)?',
    'Track recognition rates, procedural access, and documented pushback incidents against the legal text of the exception across the interval.',
    'If absorbed, the victim set expands to include refugees and the arrangement loses its principal internal limit — classification drifts toward snare; if the exception holds, the tangled_rope structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asylum_exception_absorption, empirical, 'Stability of the humanitarian exception as a limit on sovereign exclusion.').

omega_variable(
    membership_benefit_attribution,
    'How much of the citizens'' measured benefit from exclusion (labor-market and fiscal effects) is attributable to the border regime itself versus other policy instruments, given contested labor economics?',
    'Quasi-experimental studies of enforcement variation, wage-panel analyses of low-skill labor markets, and fiscal incidence studies of membership-dependent allocation.',
    'If the attributable citizen-side benefit is small, the coordination function''s beneficiary side thins and the arrangement drifts toward pure extraction with the enforcement apparatus as residual beneficiary; if substantial, the tangled_rope reading is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(membership_benefit_attribution, empirical, 'Size and attribution of the citizen-side benefit anchoring the coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1914, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1914, border_legitimacy__sovereignty_reading, theater_ratio, 1914, 0.12).
narrative_ontology:measurement_basis(bord_tr_t1914, observed).
narrative_ontology:measurement(bord_tr_t1945, border_legitimacy__sovereignty_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement_basis(bord_tr_t1945, observed).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__sovereignty_reading, theater_ratio, 1970, 0.19).
narrative_ontology:measurement_basis(bord_tr_t1970, observed).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__sovereignty_reading, theater_ratio, 1990, 0.23).
narrative_ontology:measurement_basis(bord_tr_t1990, observed).
narrative_ontology:measurement(bord_tr_t2005, border_legitimacy__sovereignty_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(bord_tr_t2005, observed).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__sovereignty_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__sovereignty_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(bord_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1914, border_legitimacy__sovereignty_reading, base_extractiveness, 1914, 0.45).
narrative_ontology:measurement_basis(bord_be_t1914, observed).
narrative_ontology:measurement(bord_be_t1945, border_legitimacy__sovereignty_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement_basis(bord_be_t1945, observed).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__sovereignty_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement_basis(bord_be_t1970, observed).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__sovereignty_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement_basis(bord_be_t1990, observed).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__sovereignty_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement_basis(bord_be_t2005, observed).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__sovereignty_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__sovereignty_reading, base_extractiveness, 2025, 0.75).
narrative_ontology:measurement_basis(bord_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1914, border_legitimacy__sovereignty_reading, suppression_requirement, 1914, 0.45).
narrative_ontology:measurement_basis(bord_su_t1914, observed).
narrative_ontology:measurement(bord_su_t1945, border_legitimacy__sovereignty_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement_basis(bord_su_t1945, observed).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__sovereignty_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement_basis(bord_su_t1970, observed).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__sovereignty_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement_basis(bord_su_t1990, observed).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__sovereignty_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement_basis(bord_su_t2005, observed).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__sovereignty_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__sovereignty_reading, suppression_requirement, 2025, 0.82).
narrative_ontology:measurement_basis(bord_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% 'Border legitimacy' is a colloquial label covering three structurally distinct claims; per the epsilon-invariance principle each is a separate story in one constraint family. This story instantiates the sovereignty reading — the upstream institutional reading, since states act on it and it supplies the enforcement infrastructure the other readings contest. The freedom_of_movement_reading re-authors epsilon over the same standing arrangement from a rights-based seat (the excluded move from victim set to rights-holder set, epsilon near the top of the scale); the humanitarian_obligation_reading partitions the victim set by cause of flight. The readings differ in epsilon, victim set, and classification; they are linked here as family members, never averaged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
