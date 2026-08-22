% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Self-Determination Reading of Territorial Sovereignty Legitimacy
 *   domain: political theory/international relations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   territorial_sovereignty_legitimacy kernel: the
 *   self_determination_reading, under which sovereignty legitimacy derives
 *   from the modern principle of self-determination applied to the Arab
 *   population holding demographic majority and continuous residence in the
 *   territory during the modern period (19th-20th centuries). The reading
 *   fixes a temporal window (antiquity does not count), requires continuous
 *   presence, treats partition and the Balfour-era impositions as void acts
 *   of external powers, frames the Israeli state as the successor of a
 *   colonial project, and reads the right of return as restoration of the
 *   status quo ante. The epsilon referent is the standing arrangement under
 *   contest — the actual sovereignty regime — assessed strictly by this
 *   reading's own lights; the endorsed alternative (restoration) is NOT the
 *   referent. Claim and metrics are independent authored facts: claimed_type
 *   describes the criterion-as-lived structure; the metrics describe the
 *   standing arrangement as this reading experiences it. Sibling readings are
 *   separate constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - arab_majority_residents: Primary beneficiary (organized/trapped) — the demographic majority whose continuous modern-period residence grounds the legitimacy claim
 *   - - palestinian_refugee_diaspora: Restitution claimant (organized/identity_locked) — right of return as status quo ante restoration
 *   - - palestinian_representative_institutions: Agenda setter (organized/constrained) — administers the claim, converts the criterion into diplomacy and institutional resourcing
 *   - - israeli_state_institutions: Primary target (institutional/constrained) — de facto sovereignty whose legitimacy the criterion denies by construction
 *   - - jewish_israeli_citizens: Target (powerful/identity_locked) — collective self-determination claim disqualified under the criterion's temporal and majority tests
 *   - - pre_zionist_jewish_communities: Excluded voice (moderate/identity_locked) — continuous presence without majority; the criterion leaves their weight undefined
 *   - - neighboring_arab_states: Secondary beneficiary (institutional/mobile) — diplomatic leverage from invoking the criterion, hosting burdens as its cost
 *   - - international_legal_community: Analytical observer (institutional/analytical) — adjudicates the criterion's doctrinal standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.78).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Self-Determination Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political theory/international relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '181ce8b1-2d4a-41cd-b92f-aa8632261ec4').
narrative_ontology:cs_kernel_codification('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', formalized).
narrative_ontology:cs_authority_grounding('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', lineage).
narrative_ontology:cs_interpretation_layer_present('181ce8b1-2d4a-41cd-b92f-aa8632261ec4').
narrative_ontology:cs_reading_relation('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', territorial_sovereignty_legitimacy__covenant_continuity_reading, influences).
narrative_ontology:cs_reading_relation('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', foundational, legitimacy_tracks_modern_demographic_presence).
narrative_ontology:cs_axiom_status(legitimacy_tracks_modern_demographic_presence, holdable).
narrative_ontology:cs_axiom_grounding('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', legitimacy_tracks_modern_demographic_presence, deontological).
narrative_ontology:cs_axiom('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', secondary, external_power_impositions_void).
narrative_ontology:cs_axiom_status(external_power_impositions_void, holdable).
narrative_ontology:cs_axiom_grounding('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', external_power_impositions_void, conventional).
narrative_ontology:cs_reference_frame('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', wilsonian_self_determination_order).
narrative_ontology:cs_drift_state('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', contemporary_post_2024_advisory_opinion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('181ce8b1-2d4a-41cd-b92f-aa8632261ec4', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_majority_residents).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_representative_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, neighboring_arab_states).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, neighboring_arab_states).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, modern_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, decolonization_norm).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, restitution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitute the demographic majority with continuous residence through the modern period whose collective will the criterion takes as the source of sovereignty. They collect doctrinal standing and claim-coherence from the criterion while bearing the displacement, occupation, and non-remedy that the standing arrangement maintains. Leaving the territory would abandon the claim's demographic anchor; staying means living the grievance the criterion articulates.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_majority_residents, beneficiary,
    organized, generational, trapped, regional).

% Hold restitution claims keyed to the pre-displacement order; the criterion converts their displacement into a legal-restorative demand (return as restoration of the status quo ante). Refugee registration passes across generations, fusing family identity with the claim; relinquishing it would dissolve the registered-refugee identity and forfeit the demand the criterion sustains.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugee_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Administer the claim: diplomacy, negotiation posture, custodianship of refugee registries, and representation in international fora. They convert the criterion into negotiating positions and institutional resourcing. Their continuity depends on the claim remaining live enough to steward; settling it on terms short of the criterion's demands would strip the institutions of their mandate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_representative_institutions, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_representative_institutions, beneficiary).

% Exercise de facto sovereignty whose legitimacy the criterion denies: the state is framed as successor to an externally-imposed project, its founding acts void, its continued existence a continuing wrong. They bear delegitimation campaigns, legal exposure in international courts, and the demand to absorb return. Exit would mean dissolving the state's juridical identity — unavailable short of state death.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_institutions, payer,
    institutional, generational, constrained, national).

% Their collective self-determination claim finds no purchase under the criterion: arrival in the immigration era fails the continuous-presence test as the criterion weights it, and ancient presence falls outside the modern window entirely. They bear the security consequences of the unresolved contest and a legitimacy deficit that no conduct of theirs can cure, because the deficiency is assigned at the level of title rather than behavior. National identity is fused with the contested sovereignty, making exit unthinkable without identity rupture.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_israeli_citizens, payer,
    powerful, generational, identity_locked, national).

% Long-established communities in the principal cities lived continuously through the modern period but always as a small minority. The criterion's majority requirement leaves their presence-weight undefined: continuous residence counts, but not enough to ground anything. Their descendants would object that presence without majority still carries moral and historical weight; the criterion's architecture has no slot for that objection.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, pre_zionist_jewish_communities, excluded,
    moderate, civilizational, identity_locked, local).

% Invoke the criterion diplomatically and host large refugee populations descended from the displaced. They collect regional-political leverage from wielding the claim and pay hosting and integration costs for withholding it; several have at times traded the claim's advancement for bilateral settlements, showing the relationship to the criterion is instrumental and revisable.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, neighboring_arab_states, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, neighboring_arab_states, payer).

% Adjudicates the criterion's doctrinal standing: courts issuing advisory opinions, treaty bodies interpreting the self-determination guarantees, and scholars applying the decolonization jurisprudence comparatively to this territory. They take no side in the territorial dispute but determine how much positive-law force the criterion carries.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_representative_institutions).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a stateless population's sovereignty claim with the positive-law doctrine of self-determination: a single restorative demand (return), a shared temporal frame (the modern period), and a common evidentiary basis (demographic majority and continuous residence) replace fragmented grievance with doctrinally legible claim-making.
% TRANSFER_FUNCTION: Moves legitimacy-recognition from the de facto sovereign to the demographic-majority population; moves moral and legal standing away from ancient-title claims toward modern-demographic claims; in implementation, would move territorial control and residency rights from current holders to returnees.
% ABSENT_VOICES: Pre-Zionist Jewish communities with continuous but minority presence have no slot in the criterion's architecture — presence without majority weighs nothing, and they would object. Jewish communities displaced from Arab lands hold restitution narratives that find no seat in this criterion's victim economy. The security experience of Israeli civilians is heard only as colonial anxiety, not as a claim. Each sits outside the conversation the criterion structures.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, the Palestinian national movement loses its primary doctrinal frame, the refugee claim loses its restorative anchor and reverts to humanitarian management, the representative institutions lose their mandate's foundation, international legal engagement reorganizes around whatever frame rushes the vacated ground, and the delegitimation pressure on the standing arrangement dissipates into rival framings — the entire contest structure rearranges.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of legitimating an anti-colonial national claim against externally-authored impositions — Balfour, the Mandate, partition — without recourse to divine promise or great-power fiat, by grounding title in the modern principle that the people actually living on the land determines its sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The underlying principle is corroborated from outside the benefiting parties: ICJ advisory jurisprudence (Namibia, Western Sahara, and the 2024 occupation opinion) applies self-determination doctrine from a bench not composed of the claimant population, and UN General Assembly resolutions pass with broad non-party majorities. Corroboration for the specific application to this territory is thinner: it comes principally from the claimant side, aligned states, and human-rights organizations, with the contesting parties denying it outright — the principle is independently attested, the application is not.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82 at interval end) because, per this reading, the standing arrangement withholds land, residency, and return from the population the criterion identifies as the legitimate sovereign — the 1948 displacement and its non-remedy are the load-bearing extraction events. Suppression (0.78) is structural and physical: non-admission of returnees, military administration, checkpoint and blockade infrastructure enforcing the exclusion the criterion indicts. Theater (0.35) rises through the interim-self-rule era — arrangements performing limited sovereignty without the substance the criterion demands — then dips as that framework collapses. Accessibility_collapse (0.55) is moderate: alternatives (one-state, binational, compensated return) remain articulable and advocated but are practically blocked. Resistance (0.8) is sustained, multi-generational, and multi-form. The three measurement series share one time grid (points approximating 1920/1940/1960/1980/2000/2020) with every metric authored at every point. Boltzmann note: identity_coordination is declared because the criterion coordinates a people-boundary (who constitutes the self-determining unit); the coordination is genuine — a stateless population gaining positive-law footing — but the temporal window performs boundary maintenance that excludes rival presence, and the coupling deserves scrutiny rather than the offset's benefit of the doubt. Receipt surface: gain_flow names palestinian_representative_institutions because the constraint's operational gains (diplomatic access, resourcing, representation standing) demonstrably accrue to the seat administering the claim; fixing_cost is prohibitive because the criterion regenerates from the live grievance — removing it without changing the standing arrangement re-seeds it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute opposite types from identical structural data. From the israeli_state_institutions and jewish_israeli_citizens positions, the criterion operates as pure extraction: it denies their legitimacy by construction, assigns their presence zero or negative weight, and demands dissolution-level remedies. From the arab_majority_residents and palestinian_refugee_diaspora positions, the same criterion operates as pure coordination and restoration: it aligns their claim with positive law and gives displacement a legal vocabulary. The palestinian_representative_institutions seat experiences a third face — the criterion as institutional resource whose stewardship justifies the institution. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the claimant seats toward the beneficiary end of d: arab_majority_residents (trapped exit amplifies their stake in the criterion's vindication) and palestinian_refugee_diaspora (identity_locked — refugee identity is constituted through the return claim, pushing d firmly to the subsidized end). Victim declarations drive the Israeli seats toward the full-target end: israeli_state_institutions (constrained exit — the state cannot relocate its juridical identity) and jewish_israeli_citizens (identity_locked — national identity fused with the contested sovereignty). palestinian_representative_institutions derives a mid-low d as administrator-beneficiary. neighboring_arab_states carry secondary_role payer (hosting costs) tempering their derived beneficiary d. pre_zionist_jewish_communities sit ambiguously — continuous presence failing the majority test — which is why they are authored as excluded rather than forced into a victim or beneficiary slot. international_legal_community is analytical and neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving a displaced, stateless majority a principled answer to externally-authored impositions that did not depend on divine promise or great-power fiat — is live: the displacement and statelessness persist, so mandatrophy is not resolved and no sunset applies. The tangled_rope classification guards against two mislabels. Calling this a snare would ignore the genuine coordination function: the criterion aligns a stateless people's claim with the strongest doctrine in the modern international order and coordinates refugee advocacy, legal strategy, and diplomatic alignment around a single restorative demand. Calling it a rope would ignore that the asymmetry is built into the criterion's structure, not incidental to it: the temporal window and majority test render a rival population's presence claims weightless by construction, and active enforcement (doctrinal, institutional, diplomatic) is required to hold that exclusion against persistent contest. Both truths are structural; the hybrid category is the honest one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the territorial_sovereignty_legitimacy kernel — the self_determination_reading. Do the sibling readings (covenant_continuity_reading, existential_matrix_reading) instantiate structurally different constraints over the same territory?',
    'Compare the sibling stories'' victim sets, temporal windows, and epsilon values: covenant_continuity extends the temporal window to antiquity and shifts the beneficiary population; existential_matrix abandons the juridical criterion altogether.',
    'If a sibling reading were adopted as governing, the victim/beneficiary structure inverts or dissolves; this story''s epsilon (authored for the standing arrangement under this reading''s lights) does not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame contingency: one of three rival readings of one kernel.').

omega_variable(
    modern_period_demographic_record,
    'Does the historical record support continuous Arab demographic majority residence through the modern period, and how are 19th-century Ottoman demography, immigration waves, and 1948 displacement counts to be weighed?',
    'Archival demographic reconstruction: Ottoman censuses, British Mandate surveys, village registries, and refugee-count audits assessed by demographers outside the contesting parties.',
    'The criterion''s application strength varies with the record: qualified continuity or contested majority weakens the legitimacy claim''s foundation; robust confirmation strengthens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_period_demographic_record, empirical, 'The factual predicate of the criterion is itself contested.').

omega_variable(
    exclusivity_of_ground,
    'Is modern demographic self-determination the sole ground of sovereignty legitimacy (rendering ancient-presence claims weightless) or the preponderant ground among several?',
    'Doctrinal analysis of the reading''s own argumentation: does it treat rival grounds as void or merely outweighed; systematic survey of adherent texts and advocacy.',
    'Under exclusivity, the rival population''s claim occupies a structural victim seat; under preponderance, the constraint approaches low-friction coordination and the victim structure softens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_of_ground, conceptual, 'Whether the criterion is exclusive or weighted.').

omega_variable(
    implementation_symmetry_risk,
    'Would implementing the criterion (return and restoration of the status quo ante) reproduce extraction against the current resident population, mirroring the extraction the criterion indicts?',
    'Scenario modeling of return implementation: property registries, absorption capacity, and displacement risk under restorative versus compensatory remedies.',
    'If implementation is symmetrically extractive, the criterion as governing rule would compute as tangled_rope-or-worse from the reverse seats; if restorable without new victims, its extractive surface is confined to the contest function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_symmetry_risk, preference, 'Whether the restorative remedy risks mirrored extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(terr_tr_t20, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t60, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(terr_tr_t60, observed).
narrative_ontology:measurement(terr_tr_t80, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(terr_tr_t80, observed).
narrative_ontology:measurement(terr_tr_t100, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement_basis(terr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(terr_be_t20, observed).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t60, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(terr_be_t60, observed).
narrative_ontology:measurement(terr_be_t80, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement_basis(terr_be_t80, observed).
narrative_ontology:measurement(terr_be_t100, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement_basis(terr_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(terr_su_t20, observed).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t60, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(terr_su_t60, observed).
narrative_ontology:measurement(terr_su_t80, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement_basis(terr_su_t80, observed).
narrative_ontology:measurement(terr_su_t100, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement_basis(terr_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'whose sovereignty is legitimate in this territory' decomposes into three structurally distinct constraints — one per reading of the territorial_sovereignty_legitimacy kernel — with different epsilon values, temporal windows, and victim sets. This file is the self-determination member. All three readings cite the same historical raw material but weigh different strata: antiquity (covenant), modern demography (this file), or neither (existential). Network edges enable contamination analysis: erosion of self-determination doctrine in international law would degrade this constraint while leaving the existential reading untouched, and conversely a shift toward existential framing would hollow the juridical criterion both siblings share.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
