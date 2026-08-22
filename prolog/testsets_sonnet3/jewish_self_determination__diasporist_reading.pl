% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading: Jewish Survival Through Pluralism, Not Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   Before 1948, and with roots stretching back through emancipation-era
 *   debates, a substantial current of Jewish political thought (Bundist
 *   socialists, religious anti-Zionists, liberal integrationists, cultural
 *   autonomists) held that Jewish continuity did not require, and might be
 *   endangered by, territorial statehood. This current largely lost the
 *   argument institutionally — not primarily through philosophical refutation
 *   but through the catastrophe of the Holocaust, the practical success of
 *   Israeli statehood, and the subsequent consolidation of Zionist-aligned
 *   organizations as the dominant infrastructure of organized Jewish communal
 *   life. What remains of the diasporist alternative persists mostly as
 *   minority intellectual position, historical memory, and scattered
 *   institutional remnants, while carrying real diagnostic force about
 *   specific harms: coercive communal pressure on dissenters and diffuse
 *   collective endangerment of diaspora Jews tied to a state's conduct they
 *   did not choose.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities_maintaining_distinct_identity: primary beneficiary (moderate/constrained) — sustains pluralist Jewish life independent of territorial framing
 *   - bundist_and_autonomist_institutional_remnants: atrophied beneficiary (powerless/trapped) — historical alternative nearly extinguished, now largely symbolic
 *   - jews_coerced_into_zionist_framing: primary payer (powerless/constrained) — bears communal pressure to affirm Zionist alignment as condition of belonging
 *   - diaspora_jews_endangered_by_association_with_israeli_state_actions: primary payer (powerless/trapped) — bears reputational and physical risk from collective-fate framing
 *   - mainstream_zionist_communal_organizations: agenda-setter (institutional/arbitrage) — administers the institutional infrastructure that marginalized diasporist alternatives
 *   - host_states: structural precondition and secondary agenda-setter (institutional/analytical) — whose tolerance diaspora pluralism actually depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.48).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.58).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading: Jewish Survival Through Pluralism, Not Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '40846364-1d4c-431b-9755-5bf693c7f69a').
narrative_ontology:cs_kernel_codification('40846364-1d4c-431b-9755-5bf693c7f69a', distributed).
narrative_ontology:cs_authority_grounding('40846364-1d4c-431b-9755-5bf693c7f69a', distributed).
narrative_ontology:cs_reading_relation('40846364-1d4c-431b-9755-5bf693c7f69a', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('40846364-1d4c-431b-9755-5bf693c7f69a', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('40846364-1d4c-431b-9755-5bf693c7f69a', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('40846364-1d4c-431b-9755-5bf693c7f69a', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('40846364-1d4c-431b-9755-5bf693c7f69a', foundational, collective_survival_severable_from_sovereignty).
narrative_ontology:cs_axiom_status(collective_survival_severable_from_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('40846364-1d4c-431b-9755-5bf693c7f69a', collective_survival_severable_from_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('40846364-1d4c-431b-9755-5bf693c7f69a', secondary, state_militarization_endangers_diffuse_diaspora_safety).
narrative_ontology:cs_axiom_status(state_militarization_endangers_diffuse_diaspora_safety, holdable).
narrative_ontology:cs_axiom_grounding('40846364-1d4c-431b-9755-5bf693c7f69a', state_militarization_endangers_diffuse_diaspora_safety, empirically_contingent).
narrative_ontology:cs_reference_frame('40846364-1d4c-431b-9755-5bf693c7f69a', pre_1948_communal_autonomism).
narrative_ontology:cs_drift_state('40846364-1d4c-431b-9755-5bf693c7f69a', contemporary_post_1948_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('40846364-1d4c-431b-9755-5bf693c7f69a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities_maintaining_distinct_identity).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, bundist_and_autonomist_institutional_remnants).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framing).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_association_with_israeli_state_actions).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, non_zionist_jewish_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, bundist_and_autonomist_institutional_remnants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sustain synagogue networks, Yiddish and Ladino cultural institutions, mutual-aid societies, and minority-rights advocacy in host countries. They benefit when diaspora pluralism is treated as a legitimate, sufficient form of Jewish continuity rather than a waystation to eventual aliyah. Their institutions have thinned as funding, prestige, and communal attention shifted toward Israel-centered organizing over the twentieth century.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities_maintaining_distinct_identity, beneficiary,
    moderate, generational, constrained, global).

% Political and cultural descendants of pre-war Jewish socialist and autonomist movements that argued for national-cultural rights within multi-ethnic states rather than territorial statehood. Nearly annihilated by the Holocaust and marginalized by the subsequent triumph of the Zionist movement as the dominant post-war Jewish political vocabulary; what remains functions mostly as historical memory and minor institutional forms rather than an active political alternative.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, bundist_and_autonomist_institutional_remnants, beneficiary,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, bundist_and_autonomist_institutional_remnants, payer).

% Individual diaspora Jews who do not personally hold Zionist commitments but experience communal, familial, or institutional pressure to affirm support for Israel as a condition of full acceptance within organized Jewish life. Dissent from this framing risks accusations of self-hatred or exclusion from communal institutions (synagogues, federations, day schools) that have become entangled with pro-Israel advocacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framing, payer,
    powerless, biographical, constrained, global).

% Diaspora Jews who face antisemitic backlash, harassment, or violence triggered by conflating diaspora Jewish identity with the policies of the Israeli state, particularly during periods of conflict. They bear costs generated by a geopolitical entity whose actions they did not choose and often cannot influence, yet are held collectively responsible for by hostile actors and, they argue, by a Zionist framing that insists all Jews share a stake in Israel's conduct.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_association_with_israeli_state_actions, payer,
    powerless, immediate, trapped, global).

% Jewish intellectuals, rabbis, and organizers (in the lineage of figures like Hannah Arendt's early writings, the American Council for Judaism, or contemporary anti-occupation Jewish groups) who publicly argue for diaspora-centered Jewish life and face organized communal ostracism, loss of institutional platforms, and characterization as illegitimate or traitorous within mainstream Jewish organizational life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, non_zionist_jewish_dissidents, payer,
    moderate, biographical, constrained, national).

% Federations, advocacy bodies, and major denominational institutions that have, since the mid-twentieth century, made support for Israel a near-consensus marker of communal belonging. They administer funding flows, honors, and institutional legitimacy in ways that reward Zionist alignment and marginalize diasporist alternatives, framing the shift as a natural response to the Holocaust and to antisemitism rather than as a contestable political choice.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, mainstream_zionist_communal_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Governments of countries where diaspora Jewish communities reside; their willingness to guarantee minority rights, protect against antisemitism, and permit communal autonomy is the actual precondition for diaspora pluralism functioning at all. Historically inconsistent (expulsion, quota systems, pogroms, contemporary rises in antisemitic violence), which is precisely the vulnerability Zionism was founded to answer and which diasporism must argue is now manageable through liberal minority-rights frameworks.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_states, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, host_states, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Diaspora pluralism, where it functions, coordinates Jewish communal survival with the political order of host states: it lets Jewish religious and cultural continuity persist without requiring territorial concentration, by relying on minority-rights guarantees, civic equality, and communal self-governance within existing states.
% TRANSFER_FUNCTION: The arrangement, at its atrophied present state, transfers institutional resources, communal legitimacy, and public attention away from diaspora-centered structures and toward Israel-centered organizing; it also transfers reputational and physical risk onto diaspora Jews who are held accountable for a state's actions under a collective-fate framing they may not endorse.
% ABSENT_VOICES: Bundist, autonomist, and non-Zionist religious voices (Reform anti-Zionists of the early twentieth century, Haredi anti-Zionist sects, contemporary anti-occupation Jewish organizers) that argued diaspora life was sufficient and legitimate are largely absent from mainstream Jewish institutional discourse, having been organizationally out-competed rather than argumentatively defeated; Palestinian voices are also absent from this reading's own frame, since this reading centers intra-Jewish political debate rather than the land conflict itself.
% DISAPPEARANCE_RATIONALE: If diasporist institutions and arguments vanished entirely, some analysts hold the world would barely change, since Zionist-aligned institutions already dominate organized Jewish political life and would simply absorb remaining space; others hold that the loss of a live diasporist alternative would foreclose meaningful internal Jewish dissent, further entrenching the coercive collapse this reading identifies and materially endangering diaspora Jews who wish to disaffiliate from state-linked identity.
% FOUNDING_PROBLEM: The diasporist tradition was built to answer: how can Jewish collective life persist, flourish, and secure protection from persecution without requiring a sovereign territorial state, at a time (especially pre-1948, and among Bundists explicitly) when territorial nationalism was seen as both impractical and philosophically at odds with universalist or autonomist commitments.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish political movements (documenting the Bund, the American Council for Judaism, and interwar autonomist thought) attest the founding problem was real and organizationally serious before 1948. Contemporary diasporist advocates attest it remains live, citing rising antisemitism entangled with anti-Zionism as evidence diaspora protection is fragile. Mainstream Zionist historians and communal leaders — themselves benefiting from the current arrangement — attest the problem was resolved by Israel's founding and that diasporism is a superseded position; this attestation comes from an interested party and is weighed accordingly rather than treated as neutral corroboration.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.48) because this reading holds that no single party captures concentrated rents from diasporist institutional atrophy — the costs (communal coercion, collective endangerment) are diffuse and the 'benefit' captured by Zionist organizations is legitimacy and resources, not a clean extraction from an identifiable pool. Theater ratio is authored as substantial and rising (0.20 to 0.62) because, per this reading, much of what diaspora communal life now performs in the name of 'Jewish continuity' (advocacy programming, communal unity rhetoric) increasingly substitutes for the atrophied function of genuine diaspora-centered institution-building. Accessibility collapse is high (0.68) because once Zionist-aligned institutions became the dominant vocabulary of organized Jewish life, the practical alternative of building rival diasporist infrastructure became very difficult to reconstruct. Resistance is moderate (0.55): diasporist dissent persists and is vocal but institutionally weak relative to what it contests.
 *
 * PERSPECTIVAL GAP:
 *   From the diasporist reading's own stakeholder seats, the payer seats experience real coercion and endangerment while the agenda-setting seat experiences its own dominance as simply 'how organized Jewish life responded rationally to catastrophe' — the engine's per-seat computation should show this asymmetry: powerless/constrained payer seats compute nearer extraction, while the institutional/arbitrage agenda-setter seat, absent an identified profit motive, computes closer to inertial administration than to predation, which is consistent with this reading's own piton claim (atrophied alternative, no concentrated capturer) rather than a snare claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities maintaining distinct pluralist identity and the surviving autonomist institutions are coded as beneficiaries of this reading's preferred arrangement (low d), even though their institutional position has weakened. Individual Jews who feel coerced into Zionist affiliation, and diaspora Jews who bear collective endangerment from state-association, are coded as targets (high d) because the costs land on them personally and their exit options are constrained by social and familial embeddedness. Mainstream Zionist communal organizations sit as agenda-setters with institutional power and arbitrage-like exit (they can reallocate resources and messaging at will) — under THIS reading, they are not named as concentrated beneficiaries capturing rents, which is why gain_flow is authored diffuse rather than naming them; the loss to diasporism is institutional atrophy, not clean transfer to an identifiable capturer.
 *
 * MANDATROPHY ANALYSIS:
 *   The claimed piton type prevents mislabeling this situation as pure extraction (snare) or as functioning coordination (rope): the diasporist institutions are not being actively preyed upon by an identifiable extractor, but their coordination function (secure, sufficient Jewish continuity without statehood) has genuinely atrophied through historical circumstance and institutional consolidation rather than through anyone's profit-seeking. The founding problem (Jewish survival absent sovereignty) is authored as contested rather than flatly dead or live, because whether it remains solvable through diaspora pluralism alone, given contemporary antisemitism, is exactly the question this reading and its Zionist-leaning critics dispute — mandatrophy language would be too strong (it would assert to resolution what remains genuinely contested).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_vs_deliberate_marginalization,
    'Did diasporist institutions atrophy through neutral historical circumstance (Holocaust destruction, organic post-war consolidation) or through deliberate marginalization by Zionist-aligned organizations seeking to eliminate a rival framing of Jewish political identity?',
    'Archival and institutional-history research into funding allocation decisions, communal leadership statements, and exclusion episodes (e.g. treatment of the American Council for Judaism, Bundist remnants in postwar Europe and the US) to determine whether marginalization was incidental or strategic.',
    'If deliberate and strategic, the constraint shifts from piton (inertial atrophy, no concentrated beneficiary) toward tangled_rope or snare (active suppression benefiting an identifiable set of institutions). If incidental, piton remains the structurally accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_deliberate_marginalization, empirical, 'Whether diasporist institutional decline was passive atrophy or active suppression.').

omega_variable(
    gain_flow_diffuse_vs_concentrated,
    'Is the diffuse gain_flow authored here correct, or do specific mainstream Zionist communal organizations in fact capture concentrated institutional and financial benefit from the marginalization of diasporist alternatives (donor consolidation, prestige, employment)?',
    'Financial and organizational analysis of major Jewish federations and advocacy bodies: track whether resources historically allocated to autonomist/diasporist institutions were redirected specifically to identifiable Zionist-aligned organizations versus dispersing into general communal decline.',
    'Concentrated capture would move gain_flow from diffuse to naming mainstream_zionist_communal_organizations, and would push the classification from piton toward snare or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gain_flow_diffuse_vs_concentrated, empirical, 'Whether the constraint''s benefit concentrates on named institutions or remains genuinely diffuse.').

omega_variable(
    coercion_mechanism_ambiguity,
    'Is the pressure on non-Zionist and dissenting Jews to affirm Zionist framing structural (institutional gatekeeping: employment, communal membership, funding) or internalized (post-Holocaust trauma producing a felt necessity of state-backed security that dissenters themselves partly hold)?',
    'Compare accounts of dissenters who have fully exited organized Jewish communal life (structural pressure test) against those who remain nominally affiliated but privately dissent (internalization test); track whether felt coercion persists after institutional exit.',
    'If largely structural, suppression is accurately captured by the authored scalar and could ease if institutional gatekeeping changed. If substantially internalized, effective suppression is understated by the structural measure and would not resolve even with institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of diasporist dissent within Jewish communal institutions.').

omega_variable(
    framing_choice_kernel_vs_reading,
    'Is the diasporist position best modeled as one reading among five of a single contested kernel (as done here), or does it more accurately constitute the historically prior baseline from which the other four readings all deviate — making it structurally the ''reference frame'' rather than a coordinate reading?',
    'Historical sequencing analysis: diasporism and autonomism substantially predate 1897 political Zionism as the default mode of Jewish communal survival theory; a case could be made that diasporism is the kernel''s t0 reference_frame rather than a sibling reading.',
    'If treated as reference frame rather than coordinate reading, the drift_state authored below (measuring diasporism''s own decline) would instead need to be authored on the OTHER readings as departures from diasporism, inverting which story carries the drift narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_reading, conceptual, 'Whether diasporism should be modeled as a sibling reading or as the kernel''s historical reference frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__diasporist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t12, jewish_self_determination__diasporist_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t12, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__diasporist_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__diasporist_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).
narrative_ontology:measurement(jewi_tr_t58, jewish_self_determination__diasporist_reading, theater_ratio, 58, 0.6).
narrative_ontology:measurement_basis(jewi_tr_t58, observed).
narrative_ontology:measurement(jewi_tr_t76, jewish_self_determination__diasporist_reading, theater_ratio, 76, 0.62).
narrative_ontology:measurement_basis(jewi_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__diasporist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t12, jewish_self_determination__diasporist_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement_basis(jewi_be_t12, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__diasporist_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement_basis(jewi_be_t25, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__diasporist_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(jewi_be_t40, observed).
narrative_ontology:measurement(jewi_be_t58, jewish_self_determination__diasporist_reading, base_extractiveness, 58, 0.47).
narrative_ontology:measurement_basis(jewi_be_t58, observed).
narrative_ontology:measurement(jewi_be_t76, jewish_self_determination__diasporist_reading, base_extractiveness, 76, 0.48).
narrative_ontology:measurement_basis(jewi_be_t76, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__diasporist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.1).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint stories decomposing the natural-language concept 'the Jewish self-determination question' per the ε-invariance principle. Each reading (diasporist, liberal_nationalist, indigenous_return, religious_covenant, settler_colonial) asserts a structurally distinct claim with its own beneficiary/victim structure and its own ε — they are not observer-relative measurements of one constraint but five separate constraints sharing a contested kernel. This diasporist reading's ε (0.48, moderate, piton-classified) should NOT be averaged with or reconciled against the other readings' ε values; each file stands alone and is linked here only for contamination-propagation and family-tracing purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
