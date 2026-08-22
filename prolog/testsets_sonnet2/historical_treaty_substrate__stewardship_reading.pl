% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate — Stewardship Reading (Shared Territorial Stewardship, No Cession)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the stewardship reading of the historical treaty
 *   substrate kernel: treaties as relational pacts establishing ongoing,
 *   mutual obligations for shared territorial stewardship, with no cession of
 *   underlying indigenous sovereignty. Under this reading, the treaty is a
 *   living covenant rather than a completed transaction — jurisdiction over
 *   land and resources is meant to be exercised jointly across generations.
 *   The gap this story measures is between that founding logic and the
 *   settler state's actual administrative practice, which largely proceeds as
 *   though the extinguishment reading (a separate constraint, not this one)
 *   governs: permits issued, resources allocated, and consultation treated as
 *   a courtesy rather than a co-governance requirement. Extractiveness and
 *   theater rise over the interval as the administrative apparatus of
 *   'consultation' expands in volume without expanding in binding force — a
 *   widening gap between the stewardship reading's coordination promise and
 *   its enforcement reality.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: beneficiary under this reading's own terms, but also payer in practice (organized/trapped) — bears the cost of the state's de facto adherence to a different reading
 *   - settler_state_resource_sectors: institutional beneficiary of the ambiguity between readings (institutional/arbitrage)
 *   - settler_state_executive_and_legislature: agenda_setter who administers implementation and could shift toward genuine co-management (institutional/arbitrage)
 *   - future_generations_of_treaty_territory: powerless, trapped, civilizational time horizon — the population the stewardship logic is structurally meant to protect
 *   - domestic_courts: analytical observer adjudicating between readings case by case
 *   - third_party_land_users: excluded from negotiations despite direct stake in any co-management transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.68).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.62).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate — Stewardship Reading (Shared Territorial Stewardship, No Cession)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'b497f74b-0701-4dd2-a38c-818b7675f274').
narrative_ontology:cs_kernel_codification('b497f74b-0701-4dd2-a38c-818b7675f274', distributed).
narrative_ontology:cs_authority_grounding('b497f74b-0701-4dd2-a38c-818b7675f274', distributed).
narrative_ontology:cs_reading_relation('b497f74b-0701-4dd2-a38c-818b7675f274', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('b497f74b-0701-4dd2-a38c-818b7675f274', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('b497f74b-0701-4dd2-a38c-818b7675f274', foundational, no_sovereignty_was_ceded).
narrative_ontology:cs_axiom_status(no_sovereignty_was_ceded, holdable).
narrative_ontology:cs_axiom_grounding('b497f74b-0701-4dd2-a38c-818b7675f274', no_sovereignty_was_ceded, deontological).
narrative_ontology:cs_axiom('b497f74b-0701-4dd2-a38c-818b7675f274', foundational, treaty_obligations_are_perpetually_renewing).
narrative_ontology:cs_axiom_status(treaty_obligations_are_perpetually_renewing, holdable).
narrative_ontology:cs_axiom_grounding('b497f74b-0701-4dd2-a38c-818b7675f274', treaty_obligations_are_perpetually_renewing, conventional).
narrative_ontology:cs_reference_frame('b497f74b-0701-4dd2-a38c-818b7675f274', pre_contact_relational_governance_norms).
narrative_ontology:cs_drift_state('b497f74b-0701-4dd2-a38c-818b7675f274', contemporary_land_and_resource_permitting_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b497f74b-0701-4dd2-a38c-818b7675f274', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_resource_sectors).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, future_generations_of_treaty_territory).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, treaty_relationship_as_living_covenant).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, coexistence_over_conquest_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the original treaty relationship as an ongoing covenant of shared stewardship over their territories — not a transfer of title. Under this reading they never ceded sovereignty and retain jurisdiction over land and resource decisions alongside the settler state. In practice they bear the cost of the settler state's persistent unilateral resource extraction and permitting decisions that treat the treaty as if it were the extinguishment version, while having no exit from the territory the relationship concerns or from the legal system that adjudicates the dispute.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations, beneficiary,
    organized, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations, payer).

% Government departments and licensed industry (mining, forestry, energy) that issue and hold permits over treaty territory on the assumption of settler state underlying title. They benefit from the ambiguity between readings — proceeding as though the extinguishment reading governs while the stewardship reading remains legally live and contested. Can relocate capital or shift jurisdictions if courts or negotiations move against them; the land cannot follow them.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_resource_sectors, beneficiary,
    institutional, generational, arbitrage, national).

% Administers treaty implementation, drafts consultation policy, and decides whether to negotiate co-management frameworks or default to unilateral permitting. Could adopt shared-governance structures consistent with the stewardship reading but bears the political and administrative cost of doing so, and has historically defaulted to the cheaper extinguishment-consistent posture.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_executive_and_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Inherit whichever resource, ecological, and jurisdictional state current extraction and negotiation leave behind. Under the stewardship reading their interests are structurally protected by the mutual-obligation logic (resources jointly managed, not depleted unilaterally); their actual exposure depends on whether that logic is honored or overridden in practice.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, future_generations_of_treaty_territory, payer,
    powerless, civilizational, trapped, regional).

% Adjudicate disputes between the readings, drawing on oral history, treaty text, and the honor-of-the-crown doctrine. Their rulings determine which reading the state's obligations are measured against in a given case, without settling the kernel contest for the corpus as a whole.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, domestic_courts, observer,
    institutional, generational, analytical, national).

% Settlers, municipalities, and smallholders occupying or using treaty territory under settler-state land grants issued without reference to the stewardship reading. Would have strong objections to any co-management transition that unsettled their existing tenure, but are not parties to treaty negotiations and are not consulted when governments decide how to implement treaty obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, third_party_land_users, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state_resource_sectors).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables ongoing coexistence and shared use of a territory by two political orders without requiring either to fully subordinate the other — treaty as living framework for joint decision-making over land, resources, and mutual obligations across generations.
% TRANSFER_FUNCTION: In principle moves nothing but obligations: consultation, consent, and shared benefit from resource use. In the settler state's actual administrative practice, it moves resource revenue and jurisdictional control from indigenous treaty nations to state and industry actors who proceed as though sovereignty had been ceded.
% ABSENT_VOICES: Third-party land users holding settler-granted tenure inside treaty territory are never at the negotiating table when governments decide how to implement (or evade) stewardship obligations, yet any genuine co-management shift would directly affect their land use.
% DISAPPEARANCE_RATIONALE: If the treaty relationship were treated as void rather than as an ongoing covenant, the legal basis for indigenous jurisdictional claims over the territory would collapse entirely, permitting regimes would lose their principal check, and courts would lose the doctrinal anchor (honor of the crown, fiduciary duty) currently used to constrain unilateral state action — the entire consultation and co-management apparatus that exists, however imperfectly, would have no foundation to stand on.
% FOUNDING_PROBLEM: Two political orders needed a durable basis for sharing a territory neither could fully control militarily or administratively without the other's cooperation — treaties were negotiated as the mechanism for ongoing coexistence rather than conquest or full assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous treaty nations and their legal historians attest the founding problem — coexistence without subordination — remains live and unresolved, citing oral history and treaty text consistent with covenant language. Independent judicial commissions and some domestic court rulings (outside both the indigenous nations and the settler state's resource sectors) have corroborated the coexistence reading in specific cases, while government legal departments continue to assert the problem was resolved at signing under the extinguishment reading — the corroboration itself splits along the kernel's contested lines, which is why the founding-problem status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) not because the stewardship reading itself is extractive in its own terms — a genuinely honored covenant of joint stewardship would show low extraction — but because ε for a kernel reading is assessed against the standing arrangement under contest, by this reading's own lights. From the stewardship reading's perspective, the standing arrangement (state-administered permitting proceeding largely without binding indigenous consent) IS the arrangement under contest, and it reads as substantially extractive relative to what the covenant obligates. Theater ratio rises across the interval (0.20 to 0.40+) as consultation processes multiply procedurally without correspondingly increasing binding indigenous authority over resource decisions — a Goodhart-style substitution of consultation volume for consultation weight. Suppression falls over the interval (0.80 to 0.62) reflecting genuine legal and political gains (recognition doctrines, court wins, treaty rights affirmations) that have reduced — without eliminating — the coercive default toward unilateral state action.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations are declared as both beneficiary (this reading vindicates their retained jurisdiction) and payer (the gap between the reading's obligations and administrative practice falls on them) — this dual role is deliberate and drives a directionality near the target end despite the reading's own normative content favoring them, because directionality tracks what actually flows, not what a framework entitles. Settler state resource sectors sit near full beneficiary: they extract resource value under an operative practice that does not fully honor the mutual-obligation structure this reading asserts. The settler state executive/legislature holds agenda-setting power and could close the gap by adopting binding co-management, but bears the political cost of doing so and has generally not.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coexistence without either party subordinating the other — is authored as contested rather than resolved precisely because the stewardship reading holds it live while state administrative practice treats it as resolved (in the extinguishment reading's favor). Classifying this as tangled_rope rather than snare preserves the genuine coordination function the treaty relationship was built to serve (shared, durable territorial coexistence is a real problem two political orders needed to solve) while recognizing the asymmetric extraction that persists through the same structure — the treaty apparatus that was supposed to bind the state to consent-based governance is the same apparatus currently used to legitimate permits issued without it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_vs_construction_ambiguity,
    'Is the stewardship reading a recovery of the treaty''s original negotiated meaning (as attested by oral history and indigenous legal traditions at the time of signing), or a modern reconstruction projected backward onto documents that settler negotiators understood as land cession?',
    'Comparative analysis of contemporaneous oral historical records, indigenous-language treaty minutes, and settler negotiator correspondence from the treaty-making period, weighted against the interpretive principle that ambiguities in treaties should be resolved in favor of the indigenous signatories (contra proferentem as applied in treaty jurisprudence).',
    'If the stewardship reading substantially matches the original negotiated understanding, the gap between it and administrative practice is a governance failure to honor an existing obligation. If it is better read as an evolving reconstruction, the gap is better understood as a contest over what the treaty ought to mean going forward rather than a violation of settled terms — this does not change ε for this story but affects how the founding_problem_status contest should be weighted going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_vs_construction_ambiguity, conceptual, 'Whether the stewardship reading recovers original treaty meaning or reconstructs it.').

omega_variable(
    reading_indexed_epsilon_divergence,
    'Given that the stewardship, extinguishment, and nation-to-nation readings of the same treaty substrate produce substantially different ε values and beneficiary/victim structures, which reading (if any) should govern binding legal interpretation in a given jurisdiction?',
    'Track which reading domestic and international courts actually apply across cases over time, and whether convergence toward one reading occurs through litigation, legislative reform, or negotiated modern treaties.',
    'Convergence toward the stewardship or nation-to-nation reading would substantially lower this story''s authored extractiveness by closing the gap between the covenant''s obligations and administrative practice; continued application of the extinguishment reading in practice (regardless of stewardship''s doctrinal gains) would keep extraction elevated even as courts formally credit stewardship-consistent principles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_indexed_epsilon_divergence, conceptual, 'Which sibling reading actually governs binding legal practice over time, and whether that could change.').

omega_variable(
    third_party_tenure_disruption_risk,
    'Would a genuine transition to binding co-management under the stewardship reading require disrupting third-party land tenure granted under the extinguishment assumption, and if so, at what scale?',
    'Land title audits within treaty territories comparing indigenous jurisdictional claims under the stewardship reading against existing third-party grants and leases.',
    'A high disruption risk would explain much of the settler state''s structural incentive to avoid full stewardship-reading implementation and would identify third-party land users as a latent additional victim/beneficiary class needing their own stakeholder analysis in a future version of this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_tenure_disruption_risk, empirical, 'Scale of tenure disruption a full stewardship transition would require.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__stewardship_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(hist_tr_t80, historical_treaty_substrate__stewardship_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(hist_tr_t120, historical_treaty_substrate__stewardship_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement(hist_tr_t160, historical_treaty_substrate__stewardship_reading, theater_ratio, 160, 0.42).
narrative_ontology:measurement(hist_tr_t200, historical_treaty_substrate__stewardship_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__stewardship_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(hist_be_t80, historical_treaty_substrate__stewardship_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(hist_be_t120, historical_treaty_substrate__stewardship_reading, base_extractiveness, 120, 0.72).
narrative_ontology:measurement(hist_be_t160, historical_treaty_substrate__stewardship_reading, base_extractiveness, 160, 0.66).
narrative_ontology:measurement(hist_be_t200, historical_treaty_substrate__stewardship_reading, base_extractiveness, 200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__stewardship_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(hist_su_t80, historical_treaty_substrate__stewardship_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(hist_su_t120, historical_treaty_substrate__stewardship_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(hist_su_t160, historical_treaty_substrate__stewardship_reading, suppression_requirement, 160, 0.63).
narrative_ontology:measurement(hist_su_t200, historical_treaty_substrate__stewardship_reading, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family decomposing the natural-language concept 'the historical treaty' into structurally distinct claims per the ε-invariance principle: extinguishment_reading (property transaction, sovereignty ceded), nation_to_nation_reading (international agreement between sovereign equals, ongoing consent required), and this story, stewardship_reading (relational covenant, no cession, mutual stewardship obligations). Each carries its own ε, beneficiary/victim structure, and claimed type; none is derived from or averaged with the others. All three link to each other via affects_constraints because litigation, legislative reform, or negotiation outcomes under any one reading structurally affect the legitimacy conditions and resource availability under the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
