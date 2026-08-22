% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order — Liberal-Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the liberal-institutional reading —
 *   of the contested kernel rbio_practice_norm_complex. The standing
 *   arrangement under contest is the post-1945 rules-based international
 *   order: Charter collective security, the treaty corpus, sanctions
 *   machinery, and conditionality practice. Assessed BY THIS READING'S OWN
 *   LIGHTS, the arrangement is a genuine coordination achievement whose
 *   enforcement layer carries real, acknowledged asymmetric costs: authorized
 *   interventions return strategic gains to intervening states and contract
 *   revenue to their contractors, while targeted states and their civilians
 *   absorb freezes, scarcity, and exclusion. The reading defends the
 *   arrangement as universal, consent-based, and revisable, and reads
 *   enforcement selectivity as a capacity deficit rather than a legitimacy
 *   defect — while its own structural delta concedes who benefits and who
 *   pays. Claim and metrics are authored independently: the claimed type
 *   (tangled_rope) states what this reading holds structurally true —
 *   coordination plus conceded asymmetry under active enforcement — and the
 *   metrics state what this reading descriptively observes; the engine
 *   computes per-seat types from the structural data and owns any divergence.
 *   Sibling readings (hegemonic_extraction, sovereignty_maximalist) are
 *   separate constraint files with their own epsilon over the same referent;
 *   they are not averaged into this one.
 *
 * KEY AGENTS:
 *   - unsc_permanent_members: Agenda setter (institutional/arbitrage) — administers authorization and sanctions machinery; veto-immune from the enforcement it runs
 *   - authorized_intervening_states: Primary beneficiary (powerful/mobile) — collects strategic returns of authorized action; participation chosen per operation
 *   - compliance_and_reconstruction_contractors: Beneficiary (organized/arbitrage) — collects contracted fees; market expands with every new regime
 *   - consenting_non_permanent_states: Net beneficiary with episodic payer exposure (organized/constrained) — buys public goods with membership, pays when enforcement selects them
 *   - targeted_states_governments: Primary target (moderate/trapped) — bears freezes, conditionality, and exclusion; exit forfeits the financial commons
 *   - targeted_states_civilian_populations: Deepest target (powerless/trapped) — absorbs sanctions incidence with no seat in design
 *   - humanitarian_access_organizations: Excluded voice (organized/constrained) — documents civilian harm, advises after design is fixed
 *   - international_law_scholars: Analytical observer (analytical/analytical) — supplies the doctrinal vocabulary all seats invoke
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.56).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.64).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order — Liberal-Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, 'e3db3081-4da5-4d44-835e-a1680807fa43').
narrative_ontology:cs_kernel_codification('e3db3081-4da5-4d44-835e-a1680807fa43', fixed_text).
narrative_ontology:cs_authority_grounding('e3db3081-4da5-4d44-835e-a1680807fa43', lineage).
narrative_ontology:cs_interpretation_layer_present('e3db3081-4da5-4d44-835e-a1680807fa43').
narrative_ontology:cs_reading_relation('e3db3081-4da5-4d44-835e-a1680807fa43', rbio_practice_norm_complex__hegemonic_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('e3db3081-4da5-4d44-835e-a1680807fa43', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('e3db3081-4da5-4d44-835e-a1680807fa43', foundational, multilateral_authorization_confers_legitimacy).
narrative_ontology:cs_axiom_status(multilateral_authorization_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e3db3081-4da5-4d44-835e-a1680807fa43', multilateral_authorization_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('e3db3081-4da5-4d44-835e-a1680807fa43', foundational, enforcement_selectivity_is_capacity_deficit).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('e3db3081-4da5-4d44-835e-a1680807fa43', enforcement_selectivity_is_capacity_deficit, empirically_contingent).
narrative_ontology:cs_axiom('e3db3081-4da5-4d44-835e-a1680807fa43', secondary, conditionality_is_valid_contract_terms).
narrative_ontology:cs_axiom_status(conditionality_is_valid_contract_terms, holdable).
narrative_ontology:cs_axiom_grounding('e3db3081-4da5-4d44-835e-a1680807fa43', conditionality_is_valid_contract_terms, instrumental).
narrative_ontology:cs_reference_frame('e3db3081-4da5-4d44-835e-a1680807fa43', universal_consent_based_revision_order).
narrative_ontology:cs_drift_state('e3db3081-4da5-4d44-835e-a1680807fa43', contemporary_multipolar_drift, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e3db3081-4da5-4d44-835e-a1680807fa43', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, authorized_intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, compliance_and_reconstruction_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, consenting_non_permanent_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states_civilian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, consenting_non_permanent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over Council action, draft and negotiate resolutions, and sit on the sanctions committees that decide listings, delistings, and exemption design. Their own conduct is structurally shielded: no Council mandate binds them against their veto, and they can disregard specific resolutions while keeping the framework that grants the shield. Leaving the arrangement would mean surrendering privileged position, not escaping obligations.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Contribute forces, funding, and diplomatic capital to Council-authorized operations and to coalitions framed in the order's vocabulary. They receive strategic returns: basing access, post-conflict contracting influence, precedent-setting authority, and legitimation of operations they might otherwise pursue alone. Participation is chosen operation by operation; they join where interests align and stand aside where they do not.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, authorized_intervening_states, beneficiary,
    powerful, biographical, mobile, global).

% Sell sanctions-compliance services — screening, due diligence, escrow, monitoring — and post-intervention reconstruction, logistics, and training contracts. Each new sanctions regime or authorized mission enlarges their addressable market. Their attachment is contractual rather than territorial, so they follow demand across jurisdictions and clients at negligible switching cost.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, compliance_and_reconstruction_contractors, beneficiary,
    organized, immediate, arbitrage, global).

% The broad membership that ratified the Charter and the treaty corpus. They receive public goods — dispute settlement, recognized borders, common trade rules, collective screening of aggression — and vote in the General Assembly. They pay when enforcement selects their region or their partners, and when implementation and reporting costs land without matching agenda power; they hold no veto and cannot cheaply replicate the system's network effects.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, consenting_non_permanent_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, consenting_non_permanent_states, payer).

% Bear asset freezes, trade restrictions, and conditionality negotiated under asymmetric leverage. They can contest listings in committee, lobby for humanitarian carve-outs, and retain counsel, but exiting the financial and institutional infrastructure would forfeit trade, currency access, and diplomatic standing at once. Some retain real regional power yet meet the enforcement machinery isolated.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states_governments, payer,
    moderate, biographical, trapped, national).

% Absorb the incidence of comprehensive sanctions: scarcity of medicines and parts, inflation, degraded public services, and job loss in restricted sectors. They hold no seat in the committees that design the measures; humanitarian exemptions arrive late and narrow. Individual exit means emigration, rationed by destination states.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states_civilian_populations, payer,
    powerless, biographical, trapped, national).

% Operate inside sanctioned and post-intervention environments and document civilian incidence of enforcement. They hold advisory channels into exemption negotiations but no vote on listing, scope, or termination; their objections enter the record after design decisions are made. Standing down would abandon the populations they serve.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_access_organizations, excluded,
    organized, immediate, constrained, global).

% Track the distance between declared universality and enforcement practice, publish on the legality of specific operations and sanctions designs, and supply the doctrinal vocabulary every party invokes. They collect no rents and bear no enforcement costs; their stake is interpretive authority over what the order's rules mean.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, authorized_intervening_states).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces unilateral balancing with collective machinery: Security Council authorization screens interstate force, standardized dispute settlement lowers bilateral bargaining costs, common commercial and financial rules reduce transaction friction, and coordinated sanctions prevent defectors from undercutting collective pressure.
% TRANSFER_FUNCTION: Moves enforcement costs, asset control, and policy concessions from targeted states and their civilian populations toward intervening-state coalitions; moves contract revenue to compliance and reconstruction industries; concentrates agenda-control rents in the permanent members.
% ABSENT_VOICES: Targeted-state civilians have no seat in sanctions-committee design; non-permanent states have no veto over measures applied to them; populations of intervened territories are consulted after mandates are set. Humanitarian organizations are present but advisory-only — heard after design choices, not during them.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, interstate force would lose its screening institution, trade and finance would lose their common rulebook, and dozens of dispute-settlement and treaty-monitoring bodies would lose their anchoring — states would rebuild substitutes within years, and the interim would be governed by raw capability differentials.
% FOUNDING_PROBLEM: Prevent a third great-power war and end unilateral territorial conquest, after two world wars and the 1930s collapse of open trade; provide stable commercial rules and a collective substitute for alliance-war politics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: small and middle powers continue to fund and litigate through the institutions, attesting by budget line and courtroom practice that the founding problem persists; historical scholarship independent of any party documents the 1939–45 failure the arrangement answered; and the sovereignty-maximalist camp — adversarial to this reading — attests the founding problem was real while disputing the current form of the solution. No corroborating source claims the problem is dead.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.56 because this reading's own lights register substantial but bounded extraction: sanctions incidence on civilians and conditionality extracted under leverage are real and conceded, while much of the order's operation is public-goods provision. Suppression (0.64) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic; only extractiveness is scaled. It reflects the enforcement machinery's coercive surface: asset freezes, secondary-sanctions reach into third-country markets, and compliance mandates enforced by market exclusion rather than participant preference. Theater_ratio (0.42) tracks the widening gap between declared universality and selective practice — legitimacy language increasingly performs a consensus that practice no longer exhibits — while core functions (dispute settlement, treaty monitoring) remain real, keeping theater below the piton-signaling range. Accessibility_collapse is low (0.35): alternatives demonstrably persist (regional bodies, bilateral deals, parallel institutions, non-participation), because the arrangement's rules do not foreclose rival arrangements the way a natural limit would. Resistance is substantial (0.58): vetoed resolutions, parallel institution-building, de-dollarization efforts, and sustained legal contestation. The temporal series run on ONE shared eight-point grid (1991–2025) so every tracked metric is authored at every examined time point. Base extractiveness climbs as sanctions architectures matured and conditionality deepened faster than consent-side renewal; theater climbs as mandate controversies (2003 Iraq strain, 2011 Libya reinterpretation, subsequent veto paralysis) decoupled declared principle from practice; suppression_requirement climbs as enforcement infrastructure hardened — counterterror-financing controls after 2001, extraterritorial secondary sanctions through the 2010s — an enforcement ratchet, not decay. The trajectory is monotonic, not cyclical: no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the permanent-members seat, the arrangement is a managed coordination they administer and are personally shielded from — near-beneficiary experience with arbitrage-grade exit. From the intervening-states seat it is a voluntary coalition tool that pays out when joined. From the consenting-majority seat it is a net-positive membership with episodic, unvetoable exposure — the classic hybrid experience. From the targeted-government seat the same machinery operates as isolation enforced by market structure; from the civilian seat it operates as unchosen deprivation with no procedural address. The engine derives these divergent classifications from role, power, and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: intervening states (mobile exit, per-operation opt-in) sit near the beneficiary pole; contractors (arbitrage exit, contractual attachment) sit nearest it; the consenting majority (constrained exit, net-positive goods) sits low-moderate with episodic upward pull from its payer secondary role. Victim declarations drive high directionality: targeted governments (trapped — exit forfeits the financial commons) sit far toward the target pole; targeted civilians (powerless and trapped, national scope concentrating incidence) sit nearest the full-target end. The permanent members, as agenda setters with veto immunity and arbitrage exit, derive near the beneficiary pole — the arrangement subsidizes them structurally. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options reproduces every seat's true relationship, so an override would only duplicate structural data already declared.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — great-power war prevention and stable commerce — remains live, corroborated by parties outside the beneficiary set, so the mandate has NOT outlived its function and mandatrophy_resolved is authored false. The classification prevents two symmetrical errors: reading the conceded asymmetry (sanctions incidence, contractor revenue) as pure extraction would erase the coordination function that targeted states themselves invoke when seeking dispute settlement; reading the arrangement as pure coordination would erase the documented transfer the reading's own delta concedes. Tangled_rope holds both. The warning sign this story flags is partial and located: theater_ratio's climb toward 0.5 indicates Goodhart drift specifically in the legitimacy-declaration layer — universality language performing consensus practice no longer exhibits — even as the enforcement layer stays functional. If the declaration layer fully decouples (theater > 0.5 sustained) while enforcement persists, the arrangement drifts toward theatrical maintenance of a legitimacy claim its practice contradicts — the piton signature arriving through the front door the hegemonic sibling predicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_epsilon_delta,
    'How would epsilon, beneficiary structure, and computed type shift if the same standing arrangement were authored under the hegemonic_extraction_reading or the sovereignty_maximalist_reading rather than this liberal-institutional reading?',
    'Generate the two sibling stories over the identical referent and interval, then compare authored epsilon, victim sets, and per-seat classifications across the family.',
    'The hegemonic sibling is expected to author materially higher epsilon (selectivity as intent, revision as frozen) and compute nearer snare at payer seats; the sovereignty sibling is expected to concentrate epsilon on the interference dimension and widen the victim set to any externally-measured state. Divergence magnitude across the family is itself the measurement of how much of this constraint''s character is reading-indexed versus referent-fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_delta, conceptual, 'Committer-frame indexicality: epsilon is a property of the reading over a fixed referent; sibling readings instantiate different constraints from one kernel.').

omega_variable(
    capacity_vs_interest_selectivity,
    'Is enforcement selectivity actually explained by capacity constraints (funding, geography, force availability, information) or by the interest-alignment of the enforcing states?',
    'Comparative enforcement studies regressing enforcement initiation and intensity against violator characteristics, controlling for severity and enforcer capacity; natural experiments where capacity and interest diverge.',
    'If interest dominates after controlling for capacity, this reading''s foundational axiom enforcement_selectivity_is_capacity_deficit fails on its own empirically_contingent grounding, the legitimacy defense collapses, and the story drifts toward the hegemonic sibling''s account — with foreclosure consequences computed by the engine rather than asserted here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_interest_selectivity, empirical, 'The empirical crux separating this reading from the hegemonic sibling: capacity story versus intent story for the same selectivity pattern.').

omega_variable(
    ratification_consent_depth,
    'Is the consent that grounds this reading''s legitimacy claim meaningful — informed, revocable, negotiated — or nominal ratification under systemic pressure by states with no realistic alternative?',
    'Examine reservation patterns, withdrawal episodes, and archived negotiation records for small-state agency; compare consent quality across differently-positioned ratifiers.',
    'Shallow consent would undermine the consent-based axiom, raise effective suppression above the authored 0.64, and push payer-seat classifications toward harder extraction; robust consent variance would confirm the reading''s legitimacy story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_consent_depth, empirical, 'Depth of the ratification consent on which this reading''s entire legitimacy claim rests.').

omega_variable(
    sanctions_civilian_harm_attribution,
    'Are civilian harms inside targeted states attributable to the sanctions architecture itself, or primarily to target-government allocation choices under sanctions?',
    'Humanitarian-access data and mortality studies comparing exemption regimes, monitored corridors, and differently-designed sanctions on comparable targets.',
    'Attribution to the architecture raises the victim-directionality of the civilian seat and lifts epsilon; attribution to target governance shifts the payer structure and partially vindicates the conditionality-as-contract axiom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctions_civilian_harm_attribution, empirical, 'Causal location of the civilian incidence this reading concedes but explains away as regrettable cost.').

omega_variable(
    kernel_codification_framing,
    'Is the kernel best framed as a fixed text (the Charter anchoring the norm complex, with drift absorbed by Council practice and doctrine) or as a distributed practice-complex with no single adjudicating authority?',
    'Test both framings against the order''s actual adjudication pattern: if legitimacy disputes are settled by textual interpretation channeled through authorized interpreters, fixed_text holds; if competing institutions and scholarly doctrine produce equally authoritative articulations with no designated interpreter, distributed holds.',
    'Under the distributed framing, kernel_codification becomes distributed, authority_grounding shifts away from lineage, and interpretation_layer_present becomes invalid — changing the cs_pattern classification of this reading while leaving the structural stakeholder data untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'CS-framing under-determination: the obvious framing (Charter as fixed kernel) versus the less obvious one (practice-complex with distributed authority).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_liberal_tr_t1991, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1991, 0.2).
narrative_ontology:measurement(rbio_liberal_tr_t1996, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1996, 0.24).
narrative_ontology:measurement(rbio_liberal_tr_t2001, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(rbio_liberal_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(rbio_liberal_tr_t2011, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2011, 0.37).
narrative_ontology:measurement(rbio_liberal_tr_t2016, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(rbio_liberal_tr_t2020, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(rbio_liberal_tr_t2025, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(rbio_liberal_be_t1991, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1991, 0.34).
narrative_ontology:measurement(rbio_liberal_be_t1996, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1996, 0.39).
narrative_ontology:measurement(rbio_liberal_be_t2001, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2001, 0.44).
narrative_ontology:measurement(rbio_liberal_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement(rbio_liberal_be_t2011, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2011, 0.51).
narrative_ontology:measurement(rbio_liberal_be_t2016, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2016, 0.54).
narrative_ontology:measurement(rbio_liberal_be_t2020, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(rbio_liberal_be_t2025, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2025, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(rbio_liberal_su_t1991, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1991, 0.38).
narrative_ontology:measurement(rbio_liberal_su_t1996, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1996, 0.43).
narrative_ontology:measurement(rbio_liberal_su_t2001, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(rbio_liberal_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(rbio_liberal_su_t2011, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(rbio_liberal_su_t2016, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2016, 0.59).
narrative_ontology:measurement(rbio_liberal_su_t2020, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(rbio_liberal_su_t2025, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2025, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the rules-based international order' decomposes into three readings of one kernel (rbio_practice_norm_complex), each with its own stable epsilon over the same referent — the standing post-1945 arrangement. This liberal-institutional story authors epsilon ~0.56 (real but bounded extraction, rationalized as capacity and contract); the hegemonic_extraction sibling authors materially higher epsilon (frozen project, selectivity as intent) and would classify nearer snare; the sovereignty_maximalist sibling authors high epsilon on the interference dimension. Upstream/downstream: this reading is the order's official self-description, and the hegemonic reading treats this story's existence as its own evidence (the cover thesis needs the sincere cover); the sovereignty reading responds to this reading's practiced outputs (authorized interventions, conditionality). All three files link each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
