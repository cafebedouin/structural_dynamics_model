% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Vaccination Mandate Legitimacy — Public-Health-Primary Reading
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the
 *   public_health_primary reading of mandate_legitimacy_scope, whose claim is
 *   that state compulsion of vaccination is legitimate when necessary to
 *   protect vulnerable populations from serious harm. The eps referent is the
 *   standing arrangement under contest — the permissive default in which
 *   mandates are absent, hollowed out by exemptions, or politically blocked —
 *   assessed by this reading's own lights. In that arrangement the costs of
 *   refusal are externalized onto people who cannot vaccinate and cannot exit
 *   shared air, which is why eps is high FROM MANDATE ABSENCE. KEY AGENTS (by
 *   structural relationship): immunocompromised_patients and
 *   pre_vaccination_age_infants bear the arrangement's costs
 *   (powerless/trapped); unvaccinated_refusers and
 *   anti_mandate_advocacy_networks gain from it (moderate/constrained,
 *   organized/identity_locked); public_health_agencies administer what
 *   remains (institutional/constrained); constitutional_courts adjudicate
 *   legitimacy (institutional/analytical); congregate_care_residents would
 *   object but have no seat (powerless/trapped). The claim/metric gap is
 *   deliberate and is the datum: claimed_type states what I believe is
 *   structurally true of the PRINCIPLE (a necessity-conditioned public-good
 *   coordination solution — rope-shaped when operative), while the metrics
 *   describe the degraded standing arrangement the story is about. The engine
 *   computes per-seat types from the structural data; the divergence between
 *   claim and computed type is the measurement, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - immunocompromised_patients: principal bearer of the standing arrangement's costs (powerless/trapped) — cannot be vaccinated, cannot exit shared air
 *   - pre_vaccination_age_infants: principal bearer (powerless/trapped) — protection entirely external to them
 *   - unvaccinated_refusers: principal gainers under the permissive default (moderate/constrained) — externalize outbreak risk while retaining public life
 *   - anti_mandate_advocacy_networks: organized gainer (organized/identity_locked) — donations, audience, and identity ride on keeping the fight alive
 *   - public_health_agencies: administering seat (institutional/constrained) — runs what mandates remain, proposes more, lacks political room
 *   - constitutional_courts: adjudicating seat (institutional/analytical) — Jacobson lineage affirms police power; recent rulings narrow the mandate form
 *   - congregate_care_residents: absent voice (powerless/trapped) — safety decided by staff coverage, no seat in the debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.66).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.3).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.66).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Vaccination Mandate Legitimacy — Public-Health-Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'c6341ae4-eec2-425b-a751-4a7b3f956cf6').
narrative_ontology:cs_kernel_codification('c6341ae4-eec2-425b-a751-4a7b3f956cf6', formalized).
narrative_ontology:cs_authority_grounding('c6341ae4-eec2-425b-a751-4a7b3f956cf6', lineage).
narrative_ontology:cs_interpretation_layer_present('c6341ae4-eec2-425b-a751-4a7b3f956cf6').
narrative_ontology:cs_reading_relation('c6341ae4-eec2-425b-a751-4a7b3f956cf6', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('c6341ae4-eec2-425b-a751-4a7b3f956cf6', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('c6341ae4-eec2-425b-a751-4a7b3f956cf6', foundational, necessity_conditioned_compulsion_legitimate).
narrative_ontology:cs_axiom_status(necessity_conditioned_compulsion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c6341ae4-eec2-425b-a751-4a7b3f956cf6', necessity_conditioned_compulsion_legitimate, deontological).
narrative_ontology:cs_axiom('c6341ae4-eec2-425b-a751-4a7b3f956cf6', secondary, community_immunity_requires_near_universal_uptake).
narrative_ontology:cs_axiom_status(community_immunity_requires_near_universal_uptake, holdable).
narrative_ontology:cs_axiom_grounding('c6341ae4-eec2-425b-a751-4a7b3f956cf6', community_immunity_requires_near_universal_uptake, empirically_contingent).
narrative_ontology:cs_reference_frame('c6341ae4-eec2-425b-a751-4a7b3f956cf6', jacobson_police_power_baseline).
narrative_ontology:cs_drift_state('c6341ae4-eec2-425b-a751-4a7b3f956cf6', contemporary_exemption_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6341ae4-eec2-425b-a751-4a7b3f956cf6', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, unvaccinated_refusers).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, anti_mandate_advocacy_networks).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, pre_vaccination_age_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_refusers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Undergoing chemotherapy, transplant immunosuppression, or living with immune-defining conditions that make vaccination ineffective or unsafe. Their protection consists entirely of the vaccination status of the people around them, because their own bodies cannot mount a response. When local coverage slips they absorb the resulting outbreak risk in full: they cannot move through public life without breathing shared air, and shielding at home means giving up work, school, and medical care.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_patients, payer,
    powerless, biographical, trapped, national).

% Too young for the recommended vaccine schedule. Their protection consists of maternal antibodies and the vaccination status of everyone they come into contact with. In an outbreak they are typically the first hospitalized and the most likely to die, and they have made no choice about any of it.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, pre_vaccination_age_infants, payer,
    powerless, immediate, trapped, national).

% Decline vaccination for reasons of conscience, religion, distrust, or calculated risk preference. Under the current permissive arrangement they face no legal consequence in most jurisdictions, continue to move through public spaces, and are shielded by the vaccinated around them whenever coverage stays high. Their own elevated illness risk is the main cost they personally carry.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_refusers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, unvaccinated_refusers, payer).

% Organizations, media channels, and political entrepreneurs that fundraise, broadcast, and mobilize around resistance to compulsory vaccination. Donations, audiences, and electoral relevance flow to them in proportion to how salient the mandate fight remains, which depends on the permissive default staying contested rather than settled. Their organizational identity is built on the fight itself.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, anti_mandate_advocacy_networks, beneficiary,
    organized, biographical, identity_locked, national).

% Administer whatever mandates exist — school-entry checks, emergency workplace rules — track coverage, and recommend schedules. They can propose extending compulsion but lack the political room to do so; after each outbreak they absorb public blame while the exemption statutes that produced the coverage gap remain untouched.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate challenges to mandate laws. Since Jacobson v. Massachusetts (1905) the doctrinal line affirms that compulsion to prevent disease falls within the state police power; recent religious-liberty rulings press toward requiring individualized exemptions, narrowing what a mandate can demand.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Live in nursing homes, group homes, and long-term facilities where a single introduced infection spreads through the whole building. Staff vaccination rates decide their safety. They have no seat in exemption debates, limited ability to relocate, and no way to audit the immunity of the people who bathe and feed them.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, congregate_care_residents, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, unvaccinated_refusers).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides community immunity as a public good: converts each person's vaccination decision from a private gamble into a shared pool of protection tall enough to interrupt transmission, solving the free-rider problem that voluntary uptake cannot close at the coverage threshold vulnerable people depend on.
% TRANSFER_FUNCTION: Moves disease risk — and the small burden of receiving the vaccine — away from those who cannot bear it (the immunocompromised, infants too young for the schedule) and onto those who can, by making refusal a regulated act with consequences instead of a costless private choice.
% ABSENT_VOICES: Congregate-care residents and the not-yet-diagnosed immunosuppressed would object but have no seat; infants appear only through proxies. The visible debate is conducted between refuser communities asserting bodily sovereignty and agencies asserting expertise, while those who cannot be vaccinated speak mainly through hospital admission statistics and the obituaries that follow outbreaks.
% DISAPPEARANCE_RATIONALE: If the principle vanished overnight — if no state could ever cite vulnerable-population protection to justify compulsion — school-entry laws would lose their constitutional footing, coverage would drift below outbreak thresholds in jurisdiction after jurisdiction, endemic measles, pertussis, and influenza cycles would resume, and the immunocompromised and infants would either retreat into permanent shielding or absorb repeated infection; congregate facilities would become periodic epicenters.
% FOUNDING_PROBLEM: Late-nineteenth-century cities suffered recurrent smallpox epidemics that voluntary inoculation could not stop, because coverage below the protection threshold let outbreaks chain through the susceptible. The founding problem was whether a self-governing community may compel a minority to accept a small medical burden to avert a large shared catastrophe — answered affirmatively in Jacobson v. Massachusetts (1905).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: state and federal court dockets from 1905 forward, CDC and WHO surveillance linking coverage dips to resurgence, pediatric-society testimony, and pre-vaccine mortality statistics. The immunocompromised attest the problem from the harmed side, not the benefited side. No party disputes that outbreaks follow coverage collapse; the live dispute is over whether compulsion is a permissible answer.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.66) because the standing arrangement transfers serious disease risk onto people who did not choose it and cannot purchase protection individually — herd immunity cannot be bought privately, so the vulnerable's alternatives collapse severely once the epidemiology is understood (accessibility_collapse 0.68, high for a non-mountain because collapse is seat-specific, not universal). Suppression is low and FALLING (0.30, matched by the suppression_requirement series) because the arrangement's history is one of enforcement abandonment: active coercive machinery (fines, exclusion, forcible measures) decayed monotonically across the interval. Suppression is a raw structural property and is NOT scaled by power or scope; the vulnerable's bind registers not as active coercion against them but as trapped exit options and collapsed alternatives. Resistance is high (0.72): organized litigation, exemption campaigns, and legislative repeal waves meet the principle wherever it advances. Theater_ratio rises from 0.12 to 0.32 as functional enforcement gives way to performative health-freedom politics and ritual school-entry checks in high-exemption districts — a Goodhart-drift marker. The three series share one ten-point grid (1905-2026). Cyclical note: the fine-grained record oscillates — outbreak, emergency tightening, relaxation, accumulation — and the relaxation phase is where exemption stock accumulates; episodic outbreaks periodically vindicate the principle and then fade, letting opposition rebuild, so the oscillation is load-bearing for the permissive equilibrium rather than noise. The coarse grid captures one full arc (compliance era to erosion era); the ratchet appears as the asymmetric endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the immunocompromised and infant seats the standing arrangement is experienced as a trap with no exit and no purchased alternative — functionally indistinguishable from pure extraction borne by a class that cannot strike, boycott, or move away from shared air; the coalition power that normally rescues powerless classes is unavailable to people whose vulnerability is medical and dispersed. From the refuser seat the same arrangement is ordinary freedom plus a residual self-risk they accepted knowingly. From the agency seat it is responsibility without authority — blame flows in after every outbreak while the levers stay locked. From the advocacy-network seat it is a revenue and identity engine that depends on the fight never being settled. The engine derives these divergences from power, exit, and directionality; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (unvaccinated_refusers, anti_mandate_advocacy_networks) drive low directionality for those seats; victim declarations (immunocompromised_patients, pre_vaccination_age_infants) drive high directionality near the full-target end, pushed further by trapped exits. Two overrides correct derivations the structural data alone would get wrong: (1) moderate -> 0.15 — refusers carry a secondary payer position (their own elevated illness risk), which would drag the derived d upward, but their structural position in the standing arrangement is beneficiary: they retain public life while externalizing the risk their refusal creates; (2) institutional -> 0.30 — public_health_agencies sit between administration and blame: mildly subsidized (budget, jurisdiction, mandate) yet politically captive, so a mid-low d rather than the fallback. Constitutional_courts share the institutional atom but are near-analytical and barely feed the computation. Scope is national throughout, so the engine's scope amplification applies uniformly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrent epidemic catastrophe in dense populations when coverage slips — is LIVE, corroborated by every post-2015 outbreak linked to exemption clusters, so the principle itself carries no mandatrophy declaration and the status x verdict pair (live x world_rearranges) raises no zombie flag. The mandatrophy-relevant drift sits one layer down: the enforcement machinery (school-entry checks, exclusion rules) increasingly persists as ritual in high-exemption districts — theater_ratio rising past 0.3 — while the guarantee it symbolizes erodes; if enforcement decay proves irreversible (see omega enforcement_decay_reversibility), that layer drifts toward piton. The classification prevents two mislabelings: reading the coerced refuser as the constraint's victim (the bodily-autonomy move) would erase the actual victim set this reading identifies; reading paper mandates as functioning coordination (a naive rope verdict on the standing arrangement) would miss the extraction the vulnerable currently bear. Claiming rope for the principle while authoring extractive metrics for the arrangement keeps both errors visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_of_mandate_legitimacy_scope,
    'This constraint is the public_health_primary reading of the kernel mandate_legitimacy_scope — what structurally changes if a sibling reading is instantiated instead?',
    'Compare victim sets and eps referents across the three sibling stories: bodily_autonomy_primary moves unvaccinated refusers into the victim set and authors eps over the mandate-imposed arrangement; proportionality_reading makes both victim sets conditional on severity, safety, and less-restrictive-alternative findings.',
    'Instantiating a sibling flips the eps referent to the opposite arrangement and swaps the victim set; cross-reading comparison is valid only at the kernel level, never by averaging sibling eps values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_of_mandate_legitimacy_scope, conceptual, 'Committer structure: this is one of three readings; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    disagreement_location_bodily_integrity_vs_imposed_harm,
    'Where exactly do the readings disagree — on facts, on weights, or on who counts as harmed?',
    'Locate the divergence: all readings accept the same epidemiology (coverage thresholds, outbreak mechanics); they disagree on the moral weighting of nonconsensual bodily intrusion versus harm imposed on third parties who cannot consent and cannot protect themselves.',
    'If the disagreement is purely axiological, no empirical finding resolves it and the kernel stays permanently contested; to the extent any reading rests on disputed facts such as vaccine safety profiles, targeted evidence could shrink the contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_bodily_integrity_vs_imposed_harm, conceptual, 'The contest''s locus: value weighting over a shared factual base, not the epidemiology itself.').

omega_variable(
    necessity_standard_operationalization,
    'What coverage level, disease severity, and outbreak proximity make compulsion ''necessary'' rather than merely precautionary?',
    'Judicial articulation of the necessity trigger combined with epidemiological threshold studies (e.g., roughly 95 percent coverage for measles interruption); watch whether courts adopt quantitative triggers or defer to agency discretion.',
    'A demanding necessity standard shrinks mandate scope and enlarges the exposed victim set; a lax standard expands compulsion and strengthens the bodily-autonomy challenge against it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_standard_operationalization, empirical, 'Operational content of the ''necessary'' condition on which this reading''s legitimacy claim turns.').

omega_variable(
    enforcement_decay_reversibility,
    'Is the post-2015 enforcement decay a permanent ratchet, with exemption stock accumulating across each outbreak-relaxation cycle, or does coverage recover after each crisis?',
    'Track kindergarten coverage and exemption rates over the coming decade against outbreak timing; persistent failure to recover post-outbreak baselines indicates a ratchet.',
    'A permanent ratchet drives theater_ratio upward and pushes school-entry enforcement toward piton drift; recoverable decay keeps the rope classification of the operative principle stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_reversibility, empirical, 'Whether enforcement erosion compounds or self-corrects across outbreak cycles.').

omega_variable(
    refuser_identity_fusion_depth,
    'How deeply are refuser communities fused with refusal as identity, and does that make their exit identity_locked rather than merely constrained?',
    'Post-mandate attitude tracking in jurisdictions that removed exemption categories (e.g., California after SB 277): if opposition hardens rather than dissipates once the legal cost lands, fusion is deep.',
    'Deep fusion raises the coercive price of any mandate, feeds the resistance metric, and predicts conversion of refusers from incidental gainers into durable organized opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuser_identity_fusion_depth, empirical, 'Identity-lock depth of the refuser population and its effect on exit options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 1905, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mls_php_tr_t1905, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1905, 0.12).
narrative_ontology:measurement(mls_php_tr_t1922, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1922, 0.11).
narrative_ontology:measurement(mls_php_tr_t1939, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1939, 0.1).
narrative_ontology:measurement(mls_php_tr_t1956, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1956, 0.09).
narrative_ontology:measurement(mls_php_tr_t1968, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(mls_php_tr_t1980, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(mls_php_tr_t1992, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(mls_php_tr_t2004, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2004, 0.19).
narrative_ontology:measurement(mls_php_tr_t2015, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(mls_php_tr_t2026, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(mls_php_be_t1905, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1905, 0.6).
narrative_ontology:measurement(mls_php_be_t1922, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1922, 0.5).
narrative_ontology:measurement(mls_php_be_t1939, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1939, 0.42).
narrative_ontology:measurement(mls_php_be_t1956, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1956, 0.26).
narrative_ontology:measurement(mls_php_be_t1968, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1968, 0.22).
narrative_ontology:measurement(mls_php_be_t1980, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(mls_php_be_t1992, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1992, 0.23).
narrative_ontology:measurement(mls_php_be_t2004, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2004, 0.29).
narrative_ontology:measurement(mls_php_be_t2015, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(mls_php_be_t2026, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(mls_php_su_t1905, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1905, 0.7).
narrative_ontology:measurement(mls_php_su_t1922, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1922, 0.67).
narrative_ontology:measurement(mls_php_su_t1939, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1939, 0.63).
narrative_ontology:measurement(mls_php_su_t1956, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1956, 0.58).
narrative_ontology:measurement(mls_php_su_t1968, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1968, 0.54).
narrative_ontology:measurement(mls_php_su_t1980, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(mls_php_su_t1992, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement(mls_php_su_t2004, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement(mls_php_su_t2015, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(mls_php_su_t2026, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2026, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccination mandate legitimacy' covers three structurally distinct constraints (per the eps-invariance principle) and was decomposed into a three-story kernel family. This story (public_health_primary) authors eps over the mandate-absent arrangement, with the immunocompromised and pre-vaccination infants as victims and refusers as gainers. The sibling bodily_autonomy_primary authors eps over the mandate-imposed arrangement, with refusers as victims; the sibling proportionality_reading authors eps conditionally on severity, safety, and alternatives findings. The upstream story (this one) carries the higher-confidence empirical base (threshold epidemiology, outbreak surveillance) and structurally influences the proportionality sibling; the bodily-autonomy sibling is foreclosed rather than influenced, since the two core premises cannot coexist in one framework. Each file links the other two via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, moderate, 0.15).
constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
