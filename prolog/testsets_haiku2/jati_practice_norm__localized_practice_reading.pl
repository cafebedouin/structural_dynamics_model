% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Practice Norms: Localized Renegotiation Reading
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'localized practice reading' of
 *   the jati-boundary kernel. The reading asserts that jati categories are
 *   coordination norms for occupational identity, apprenticeship, mutual aid,
 *   and ritual participation, continuously renegotiated and proliferated at
 *   the local/regional level without centralized authority. Empirical
 *   evidence: 3000+ recorded jati categories exist, many local to specific
 *   villages or occupational subgroups; boundary incorporation of migrants,
 *   occupational fission following economic change, and ritual adjudication
 *   of ambiguous cases are documented across pre-colonial records. The
 *   constraint's extractiveness (0.28) is low because enforcement depends on
 *   local acceptance and community utility, not coercion; proliferation to
 *   thousands of categories indicates weak centralized enforcement. The
 *   reading differs radically from the textual reading (which asserts fixed
 *   varna hierarchy and textual purity) and the colonial reading (which
 *   stabilized categories via administrative apparatus). This story claims
 *   jati practice is ropework: genuine coordination solving real
 *   village-level problems, with low extraction and low suppression. The
 *   schema requires explicit committer frame handling: the claim and metrics
 *   are authored independently; the reading relations and axioms populate
 *   cs_structure to document sibling relationships.
 *
 * KEY AGENTS:
 *   - Local artisan guilds: occupational identity maintainers; benefit from norm recognition without requiring centralized enforcement
 *   - Occupational communities: real-world problem-solvers; coordinate labor, knowledge, and mutual obligation through jati membership; continuously renegotiate boundaries
 *   - Village coordination structures: use jati to organize ritual roles, taxation, dispute resolution; boundaries remain locally adjustable
 *   - Brahminical orthodoxy seats: textual authority excluded from local practice; claim fixed varna hierarchy but lack enforcement mechanism
 *   - Colonial administrative apparatus: later intervene and reify categories for enumeration and control; this reading predates their stabilization project
 *   - Ritual specialists: local gatekeepers; perform boundary adjudication through ritual incorporation and marriage legitimation
 *   - Lower occupational strata: identity-locked into asymmetric obligation; pay labor and deference; do not collect primary benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.28).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.15).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Practice Norms: Localized Renegotiation Reading").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/religious/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '03c3e77b-0434-49c2-af2a-b57efe3e67ed').
narrative_ontology:cs_kernel_codification('03c3e77b-0434-49c2-af2a-b57efe3e67ed', distributed).
narrative_ontology:cs_authority_grounding('03c3e77b-0434-49c2-af2a-b57efe3e67ed', practice).
narrative_ontology:cs_interpretation_layer_present('03c3e77b-0434-49c2-af2a-b57efe3e67ed').
narrative_ontology:cs_reading_relation('03c3e77b-0434-49c2-af2a-b57efe3e67ed', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('03c3e77b-0434-49c2-af2a-b57efe3e67ed', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('03c3e77b-0434-49c2-af2a-b57efe3e67ed', foundational, jati_boundaries_locally_renegotiable).
narrative_ontology:cs_axiom_status(jati_boundaries_locally_renegotiable, holdable).
narrative_ontology:cs_axiom_grounding('03c3e77b-0434-49c2-af2a-b57efe3e67ed', jati_boundaries_locally_renegotiable, empirically_contingent).
narrative_ontology:cs_axiom('03c3e77b-0434-49c2-af2a-b57efe3e67ed', foundational, occupational_coordination_solves_village_problems).
narrative_ontology:cs_axiom_status(occupational_coordination_solves_village_problems, holdable).
narrative_ontology:cs_axiom_grounding('03c3e77b-0434-49c2-af2a-b57efe3e67ed', occupational_coordination_solves_village_problems, instrumental).
narrative_ontology:cs_reference_frame('03c3e77b-0434-49c2-af2a-b57efe3e67ed', pre_administrative_localized_practice).
narrative_ontology:cs_drift_state('03c3e77b-0434-49c2-af2a-b57efe3e67ed', pre_colonial_practice_equilibrium, gap(stable, minor, false)).
narrative_ontology:cs_created_at('03c3e77b-0434-49c2-af2a-b57efe3e67ed', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_artisan_guilds).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, occupational_communities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, village_coordination_structures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, merchant_networks).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, merchant_networks).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, lower_occupational_strata).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain occupational identity and status through jati membership; coordinate apprenticeship, trade secrets, and market access within the guild. Benefit from the norm's provision of recognized membership boundaries and mutual aid obligations. Entry is negotiated locally; craft knowledge and community standing matter more than scriptural verification.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_artisan_guilds, beneficiary,
    organized, generational, constrained, local).

% Use jati boundaries to coordinate labor standards, price-setting, social insurance within occupation-specific groups. The norm permits continuous renegotiation of who belongs — historically, occupations fissioned or merged when economic conditions changed or populations migrated, with jati boundaries following pragmatic reorganization rather than fixed categories.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, occupational_communities, beneficiary,
    moderate, biographical, mobile, regional).

% Use jati categories to organize ritual roles, taxation, dispute resolution, and mutual obligation within the village. The structure solves a real coordination problem: defining who owes what to whom in a multi-occupational settlement. Boundaries remain locally adjustable; newcomers integrate through negotiation; categories proliferate as communities differentiate.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, village_coordination_structures, beneficiary,
    moderate, generational, constrained, local).

% Textual authorities claim fixed varna hierarchy and scriptural purity boundaries. They would adjudicate jati legitimacy through textual canon and ritual law; their exclusion is implicit — local practice proceeds without waiting for doctrinal approval. Their objections to proliferation and local boundary-drift are heard but do not halt village-level renegotiation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, brahminical_orthodoxy_seats, excluded,
    powerful, civilizational, trapped, continental).

% Seeks fixed, enumerable categories for census, taxation, and law. This reading's reading predates administrative stabilization; colonial census-taking later froze categories the practice reading held as fluid. The apparatus is excluded from pre-colonial localized negotiation; its later intervention transforms the constraint's structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_administrative_apparatus, excluded,
    institutional, generational, analytical, national).

% Depend on jati identities for credit networks, trading partnerships, and contract enforcement across regions. They benefit from the norm's stability and recognition; they also pay through the obligation to maintain occupational reputation and assist community members. Their mobility allows some arbitrage — they negotiate jati identity differently in different markets while maintaining core occupational claims.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, merchant_networks, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, merchant_networks, payer).

% Occupy jati categories that carry obligations to higher strata (labor, deference, ritual restriction). The norm binds them into relationships of asymmetric obligation. Exit by occupational switching is theoretically possible but practically difficult — occupational identity carries kinship, inheritance, and social belonging. They do not collect primary benefits from the norm; they bear its obligations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, lower_occupational_strata, payer,
    powerless, biographical, identity_locked, local).

% Perform marriages, funerals, and jati-status rituals; their role as arbiters of legitimacy gives them practical gatekeeping power within local boundaries. They set and enforce norms through ritual; they adjudicate borderline cases and integrate newcomers through ritual incorporation. Their power is real but highly localized and subject to community push-back.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, ritual_specialists, agenda_setter,
    organized, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of defining membership, obligation, and role in multi-occupational communities. Provides recognized identity categories for apprenticeship, inheritance, mutual aid, marriage, ritual participation, and taxation. Coordinates behavior within occupational groups through shared norms of craft practice and occupational honor.
% TRANSFER_FUNCTION: Moves labor (lower strata work for upper strata), deference (status and ritual recognition), and material goods (bride price, ritual payment) upward through jati hierarchy. The transfer is legitimated by the norm's claim that obligation follows occupational identity and ritual status.
% ABSENT_VOICES: Colonial administrators (systematically excluded pre-colonial; later intervene and reify categories). Brahminical textual authorities (their scriptural objections do not halt local practice). Inter-regional merchants (their interests in standardization are real but negotiated locally, not authority-derived). Individuals seeking to exit their birth-jati (the norm constrains their exit; their objections are heard but historically did not overturn the system).
% DISAPPEARANCE_RATIONALE: This reading's position: if local jati-boundary practice vanished, occupational coordination would be disrupted but villages would likely re-invent similar categories (occupational guilds, caste-like associations, or functional groupings); the coordination problem does not disappear, only the inherited vocabulary. Dissenters (colonial reading, textual reading) would argue the constraint's disappearance would be salutary — boundaries would become fluid, merit-based, individually chosen. The empirical claim of the localized practice reading is that proliferation to 3000+ recorded categories demonstrates weak enforcement and continuous renegotiation; the constraint persists not by coercion but by local utility.
% FOUNDING_PROBLEM: Pre-industrial multi-occupational villages require coordination mechanisms for defining roles, obligations, inheritance, ritual participation, and mutual aid. Occupational knowledge requires transmission (apprenticeship); status requires recognition; mutual obligation requires clarity about who owes what. Jati categories solve these problems locally.
% FOUNDING_PROBLEM_CORROBORATION: Village ethnographers and occupational historians document continuous jati boundary negotiation and proliferation through the 18th–19th centuries (before census stabilization). Local records of guild incorporation, marriage negotiation, and ritual adjudication show communities regularly redefining boundaries. Independent anthropological testimony from non-benefiting observers (colonial administrators' own records document boundary fluidity they later tried to fix; Sanskrit scholars note textual varna is not empirically isomorphic to jati practice) attests the founding coordination problem remains live and is solved through local renegotiation, not textual fiat.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, contested).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) is low because the constraint solves genuine coordination problems at local scale without requiring coercive authority to maintain boundaries — communities renegotiate and proliferate categories as occupational and demographic conditions change. Suppression (0.15) is minimal: the norm persists through utility and social recognition, not through enforcement machinery or legal prohibition. Theater (0.12) is negligible: ritual performance is integral to the coordination function, not a thin cover for extraction. Accessibility_collapse (0.35) is moderate because alternatives (occupational guilds without jati framing, merit-based individual identity) remain theoretically available; local acceptance of jati membership is genuine, not coerced by unavoidable structural necessity. Resistance (0.42) is real because individuals and communities continuously push boundary limits, occupations fission, migrants negotiate entry, and inter-occupational movement occurs despite jati claims. The measurement series are flat to slightly rising because this reading assumes the constraint's dynamics are stable across pre-colonial period — the extractiveness plateau and suppression stability reflect the reading's claim that local renegotiation maintains approximate equilibrium. Colonial intervention (post-interval-end) transforms this reading's context by imposing administrative reification; the series terminates before that disruption.
 *
 * PERSPECTIVAL GAP:
 *   From the village-structure and occupational-community seats, jati is genuine coordination enabling mutual aid and occupational transmission. From the lower-strata seats, it is extractive obligation binding them to asymmetric labor and deference. From the ritual-specialist seat, it is authority they exercise to maintain boundaries. From the textual-authority seat (excluded but audible), it is deviation from scriptural varna. From the colonial seat (not yet present), it is a chaotic proliferation requiring administrative standardization. Each seat perceives a different constraint structure from the same practice — the localized reading asserts the coordination reading is empirically dominant (proliferation, renegotiation, acceptance) while acknowledging extraction exists at the lower end.
 *
 * DIRECTIONALITY LOGIC:
 *   Local artisan guilds (beneficiary, organized, constrained): d near 0.2 — they benefit from recognized membership and coordination without bearing asymmetric extraction costs. Occupational communities (beneficiary, moderate, mobile): d near 0.15 — genuine coordination benefit; some extraction from lower strata but communities renegotiate when extraction rises. Village structures (beneficiary, moderate, constrained): d near 0.25 — they collect coordination benefit but bear cost of maintaining ritual legitimacy and dispute arbitration. Ritual specialists (agenda_setter, organized, constrained): d near 0.35 — they set boundaries and collect status; they also bear burden of legitimation and adjudication. Lower occupational strata (payer, powerless, identity_locked): d near 0.75 — they are bound by asymmetric obligation (labor, deference); identity-locking prevents exit despite low mobility-based cost. Merchant networks (beneficiary/payer, powerful, mobile): d near 0.45 — genuine benefit from credit networks and contract enforcement, but they bear reputation costs and obligation to assist community; their mobility means they can negotiate differently in different markets. The engine derives d from beneficiary/victim + exit options; no overrides needed because structural data is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is live: occupational coordination in multi-occupational villages remains necessary; jati categories continue to solve apprenticeship, inheritance, and mutual-obligation problems. The localized reading predicts continuous renegotiation and community adjustment without centralized mandate decay — if the founding problem weakened, we would expect fewer jati categories and less frequent boundary negotiation. Empirically, proliferation to 3000+ categories suggests the opposite: the founding problem remains active and communities continuously spawn new categories as needed. Mandatrophy (mandate outliving function) is not the predicted trajectory for this reading; if the constraint were mandatrophy-afflicted, we would see ossified categories, rigid boundaries, and failed renegotiations — the data show the opposite. Mandatrophy becomes relevant when colonial census stabilization freezes categories and later post-colonial state reification treats jati as fixed legal categories (tangled_rope outcome), but that is downstream of this reading's interval and represents a reading shift, not mandatrophy within the localized reading itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is jati a localized, renegotiable coordination norm, or a fixed textual category, or a colonial administrative artifact?',
    'Ethnographic and archival evidence: pre-colonial boundary records, guild incorporation documents, marriage negotiations, ritual adjudication evidence. The localized reading predicts continuous proliferation and boundary renegotiation; the textual reading predicts conformity to varna structure; the colonial reading predicts stabilization post-census.',
    'If localized reading is supported, the constraint is low-extraction rope; if textual reading dominates, it is a snare with high extractive obligation and suppression; if colonial reading applies, the constraint transforms from rope to tangled_rope under administrative enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which kernel reading accurately describes jati boundary dynamics.').

omega_variable(
    suppression_source_ambiguity,
    'Is measured suppression (0.15) structural enforcement or internalized identity fusion?',
    'Post-exit tracking: individuals who leave their birth-jati occupational community show whether suppression persists (internalized) or vanishes (structural). Also: comparative analysis of communities with stronger vs. weaker ritual enforcement showing differential compliance.',
    'If suppression is primarily internalized (occupational identity fused with personhood), the constraint persists even when structural enforcement weakens, making renegotiation and exit harder than the low-suppression metric suggests. If structural, localized renegotiation remains genuinely possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Whether suppression of boundary-crossing is structural or internalized.').

omega_variable(
    proliferation_as_evidence_of_coordination,
    'Does empirical proliferation to 3000+ recorded jati categories demonstrate weak enforcement (rope reading), or does it represent institutionalized fragmentation masking high extractive specificity (tangled_rope reading)?',
    'Historical-economic analysis: if proliferation correlates with occupational differentiation and market changes (new trades, migration, economic restructuring), it supports the rope reading; if it correlates with increasing status-obsession and boundary-policing by upper strata, it supports tangled_rope.',
    'Rope reading: proliferation = active renegotiation, weak enforcement, genuine coordination. Tangled_rope reading: proliferation = fractalization designed to increase granular control and extractive targeting. The classification hinges on whether proliferation eases or increases the constraints on occupational mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_as_evidence_of_coordination, empirical, 'Whether proliferation indicates weak enforcement or granular control.').

omega_variable(
    textile_sibling_reading_frame,
    'How does this reading''s localized-practice frame relate to the orthodox_textual_reading''s scriptural-fixation frame and the colonial_census_reading''s administrative-stabilization frame?',
    'Textual analysis of pre-colonial Sanskrit sources vs. ethnographic practice; comparison of pre-census boundary records with post-census stabilization. The reading relations declare logical/structural relationships; this omega documents the empirical historical relationship between the three readings'' institutional contexts.',
    'If this reading''s localized practice predates and becomes reified by textual orthodoxy, the textual reading ''influences'' this one (downstream pressure). If colonial census-taking encounters already-textually-normative categories, the readings are less independent. The reading_relations in cs_structure declare structural relationships; this omega grounds them in documented historical sequence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textile_sibling_reading_frame, empirical, 'Historical sequence and institutional relationship between the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jati_tr_t4, jati_practice_norm__localized_practice_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement(jati_tr_t8, jati_practice_norm__localized_practice_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(jati_tr_t12, jati_practice_norm__localized_practice_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(jati_tr_t16, jati_practice_norm__localized_practice_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jati_be_t4, jati_practice_norm__localized_practice_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(jati_be_t8, jati_practice_norm__localized_practice_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(jati_be_t12, jati_practice_norm__localized_practice_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(jati_be_t16, jati_practice_norm__localized_practice_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(jati_su_t4, jati_practice_norm__localized_practice_reading, suppression_requirement, 4, 0.13).
narrative_ontology:measurement(jati_su_t8, jati_practice_norm__localized_practice_reading, suppression_requirement, 8, 0.14).
narrative_ontology:measurement(jati_su_t12, jati_practice_norm__localized_practice_reading, suppression_requirement, 12, 0.15).
narrative_ontology:measurement(jati_su_t16, jati_practice_norm__localized_practice_reading, suppression_requirement, 16, 0.15).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__localized_practice_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.1).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel has three structurally distinct constraint-story readings. The localized_practice_reading (this story) is upstream: local practice generates the categories that textual authorities later try to normalize (influences orthodox_textual_reading) and that colonial administrators later try to enumerate (influences colonial_census_reading). The three readings coexist across different epistemic communities (practitioners, scholars, administrators) and do not logically foreclose each other within their respective frameworks, but the localized reading's empirical claim (continuous proliferation and renegotiation) is incompatible with the textual reading's claim (fixed scriptural categories) and the colonial reading's eventual outcome (stabilized enumeration). See commentary.kernel_context for full reading-relationship analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
