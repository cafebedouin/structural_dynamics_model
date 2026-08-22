% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Diaspora Pluralism as Primary Vehicle for Jewish Survival (Anti-Zionist Reading)
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the diasporist reading of the
 *   jewish_self_determination kernel: the claim that Jewish collective
 *   survival is best secured through diaspora pluralism, minority rights, and
 *   cultural autonomy rather than territorial sovereignty, and that Zionism
 *   represents a dangerous deviation binding Jewish fate to a militarized
 *   state. This is one of five readings of the same kernel authored as
 *   separate constraints (liberal_nationalist, indigenous_return,
 *   settler_colonial, religious_covenant, and this one). The structural delta
 *   expected for this reading is piton: a once-live alternative institutional
 *   tradition (Bundism, diasporist socialism, cultural autonomism) that has
 *   atrophied under the institutional dominance of Zionist-aligned communal
 *   organizations since 1945, particularly since 1967. Unlike the sibling
 *   readings, which largely concern the Israeli-Palestinian territorial
 *   claim, this reading's central drama is intra-Jewish: a contest over what
 *   counts as authentic, sufficient provision for Jewish survival, fought
 *   inside diaspora Jewish institutional life itself.
 *
 * KEY AGENTS:
 *   - diaspora_communal_institutions: atrophied beneficiary institution — administers what remains of the pluralist model
 *   - diaspora_jews_maintaining_distinct_identity: primary beneficiary — benefits from non-territorial continuity where host-state tolerance holds
 *   - jews_coerced_into_zionist_identification: primary target — bears communal and reputational costs of the framework's marginalization
 *   - diaspora_jews_endangered_by_state_association: secondary target — bears externalized security costs from a state project they did not choose
 *   - zionist_institutional_establishment: excluded rival framework — holds the institutional power this reading contests
 *   - postcolonial_and_nationalism_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.42).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.58).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diaspora Pluralism as Primary Vehicle for Jewish Survival (Anti-Zionist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political philosophy / nationalism studies / postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'a5c04a59-29ef-4f57-956f-1fa76afd99ce').
narrative_ontology:cs_kernel_codification('a5c04a59-29ef-4f57-956f-1fa76afd99ce', distributed).
narrative_ontology:cs_authority_grounding('a5c04a59-29ef-4f57-956f-1fa76afd99ce', distributed).
narrative_ontology:cs_reading_relation('a5c04a59-29ef-4f57-956f-1fa76afd99ce', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5c04a59-29ef-4f57-956f-1fa76afd99ce', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5c04a59-29ef-4f57-956f-1fa76afd99ce', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('a5c04a59-29ef-4f57-956f-1fa76afd99ce', jewish_self_determination__religious_covenant_reading, forecloses).
narrative_ontology:cs_axiom('a5c04a59-29ef-4f57-956f-1fa76afd99ce', foundational, sovereignty_is_not_necessary_for_survival).
narrative_ontology:cs_axiom_status(sovereignty_is_not_necessary_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('a5c04a59-29ef-4f57-956f-1fa76afd99ce', sovereignty_is_not_necessary_for_survival, empirically_contingent).
narrative_ontology:cs_axiom('a5c04a59-29ef-4f57-956f-1fa76afd99ce', secondary, territorial_statehood_increases_diaspora_risk).
narrative_ontology:cs_axiom_status(territorial_statehood_increases_diaspora_risk, holdable).
narrative_ontology:cs_axiom_grounding('a5c04a59-29ef-4f57-956f-1fa76afd99ce', territorial_statehood_increases_diaspora_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('a5c04a59-29ef-4f57-956f-1fa76afd99ce', bundist_cultural_autonomism).
narrative_ontology:cs_drift_state('a5c04a59-29ef-4f57-956f-1fa76afd99ce', post_1967_zionist_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a5c04a59-29ef-4f57-956f-1fa76afd99ce', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_communal_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jews_maintaining_distinct_identity).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_identification).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_state_association).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissidents).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, minority_rights_framework_sufficiency).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, diaspora_nationalism_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Synagogues, kehillot, cultural federations, and Bundist-descended organizations that historically provided Jewish communal self-governance without a state. Their institutional relevance and funding base have eroded as diaspora Jewish identity has become organized substantially around support for or opposition to Israel, and as younger diaspora Jews increasingly route identity and philanthropy through Israel-linked organizations instead. What remains functions partly as living infrastructure and partly as commemorative theater of a model whose primary structural role has atrophied.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_communal_institutions, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, diaspora_communal_institutions, agenda_setter).

% Jews who build religious, cultural, and communal life within host societies (the US, France, Argentina, the UK) without organizing their Jewishness around a state project. They benefit from pluralist minority-rights protections when host states honor them, and from a diasporist tradition that offers a non-territorial account of Jewish continuity. Their situation depends heavily on host-state tolerance, which is not guaranteed and has historically failed catastrophically.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jews_maintaining_distinct_identity, beneficiary,
    moderate, biographical, constrained, global).

% Diaspora Jews who experience communal, familial, or institutional pressure to treat support for Israel as constitutive of Jewish identity or Jewish safety, such that expressing the diasporist or anti-Zionist position risks communal ostracism, loss of institutional employment (in Jewish schools, synagogues, federations), or being labeled a self-hating Jew. From this reading's vantage, the near-total institutional monopoly Zionist organizations hold over the phrase 'Jewish interest' forecloses the diasporist alternative as a live communal option, not merely as a policy debate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_identification, payer,
    powerless, biographical, trapped, global).

% Diaspora Jews who face antisemitic violence, harassment, or scapegoating that is triggered or amplified by Israeli state actions with which they had no involvement and often disagree. They bear costs generated by a state project that, in this reading, was supposed to make Jews safer but instead exports geopolitical liability onto dispersed communities who cannot control or disclaim it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_state_association, payer,
    powerless, immediate, trapped, global).

% Jewish intellectuals, organizers, and religious figures (in the tradition of the Bund, Reform anti-Zionism, and post-war diasporist thought) who actively argue for this position within Jewish communal life and are frequently excluded from mainstream Jewish institutional platforms, denied pulpits or communal leadership roles, or treated as a fringe requiring containment rather than as a legitimate pole of Jewish political thought.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissidents, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissidents, excluded).

% Nation-states in which diaspora Jewish communities live. Their willingness to extend genuine minority rights and pluralist protection is the load-bearing condition for this entire reading's viability; when host states withdraw tolerance (as in 1930s Europe, or various points in the Arab world after 1948), the diasporist model has no independent enforcement mechanism of its own and depends entirely on external goodwill.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_states, agenda_setter,
    institutional, generational, analytical, national).

% Major Jewish federations, AIPAC-aligned organizations, and the state of Israel itself, which this reading treats as the rival framework that has captured mainstream Jewish institutional life and defines communal legitimacy substantially through relationship to Israel. From the diasporist vantage they are the reason the alternative has atrophied into a piton rather than persisting as a live, resourced tradition; this reading does not grant them a voice in defining what counts as authentic Jewish self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutional_establishment, excluded,
    institutional, generational, arbitrage, global).

% Academic observers assessing whether territorial sovereignty is necessary or sufficient for minority group survival, drawing comparative evidence from other diasporic and stateless national groups. They document the historical decline of diasporist Jewish political organizations (the Bund, Jewish socialist federations) alongside the rise of Zionist hegemony but do not adjudicate the underlying normative dispute.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, postcolonial_and_nationalism_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Diaspora pluralism, where it functions, coordinates minority group survival through negotiated legal and cultural accommodation within host states, communal self-governance over religious and cultural life, and a portable, non-territorial account of continuity that does not require a state apparatus or standing military to sustain Jewish identity across generations.
% TRANSFER_FUNCTION: The reading claims that territorial-sovereignty framing transfers communal legitimacy, institutional funding, and the authority to define 'Jewish interest' away from diaspora pluralist institutions and toward a state-centered establishment, while distributing the geopolitical liabilities of that state's actions back onto dispersed diaspora communities who have no control over the state's conduct.
% ABSENT_VOICES: Palestinian voices are not seated in this constraint at all — this story is scoped to the intra-Jewish debate over sovereignty-versus-diaspora as a means of Jewish survival, and the Palestinian national claim is the subject of the separate settler_colonial_reading and indigenous_return_reading constraints in this kernel family. Within the intra-Jewish debate itself, anti-Zionist and diasporist Jewish voices are present but structurally marginalized in mainstream communal institutions, which is itself part of what this reading is protesting.
% DISAPPEARANCE_RATIONALE: If the diasporist framework's institutional remnants vanished entirely, mainstream Zionist-aligned Jewish organizations would likely experience little practical disruption, since they already hold the dominant share of communal authority, funding, and the definitional power over 'Jewish interest' — supporting a piton reading (the world is largely already rearranged around its absence). But diaspora Jews who rely on non-Israel-centered communal, religious, and cultural infrastructure would lose one of the few institutional homes that does not require assent to a state project, which is a real and contested loss from within the reading's own frame.
% FOUNDING_PROBLEM: The founding problem, on this reading, was how a stateless, historically persecuted minority could survive and flourish without reproducing the nationalist logic (ethnic sovereignty backed by force) that had been used to persecute it — the Bundist and diasporist answer was minority rights, cultural autonomy, and internationalist solidarity rather than a state of one's own.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish socialism and the Bund (writing from outside both the Zionist establishment and the current diasporist advocacy groups) corroborate that a substantial, organized diasporist political tradition existed and was largely destroyed by the Holocaust and subsequently marginalized by Zionist institutional dominance — this is attested in mainstream historiography, not only by diasporist partisans. Whether the founding problem remains live (i.e., whether minority-rights-based diaspora survival is still a viable independent path in the 21st century, given rising diaspora antisemitism tied to Israel/Palestine) is disputed even among scholars sympathetic to the diasporist tradition.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).
:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.42) because the diasporist model's own operation extracts relatively little directly — its central failure mode, on this reading, is not that it extracts rents but that it has been institutionally outcompeted and marginalized, leaving its remaining adherents to bear disproportionate communal costs (exclusion, labeling, loss of institutional standing) for holding a minority position within Jewish political life. Suppression is authored higher (0.58) than extraction because the mechanism of harm here is largely definitional and reputational — control over what counts as legitimate 'Jewish interest' — rather than coercive apparatus. Theater ratio rises substantially over the interval (0.15 to 0.55) reflecting the reading's core historical claim: diasporist institutions increasingly perform continuity with a once-vital autonomist tradition while their actual coordinating function (communal self-governance independent of a state project) has been hollowed out by resource and legitimacy capture elsewhere. Accessibility collapse (0.62) reflects that the diasporist option, while never legally foreclosed, has become practically difficult to sustain as a fully resourced communal alternative. Resistance (0.5) reflects real, ongoing anti-Zionist Jewish organizing that persists despite marginalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communal institutions and diaspora Jews maintaining distinct, non-Israel-centered identity are coded as beneficiaries: where the model still functions, it provides genuine communal infrastructure without requiring state alignment. Jews coerced into Zionist identification, diaspora Jews endangered by association with Israeli state conduct, and anti-Zionist Jewish dissidents are coded as victims/payers: they bear the costs of the diasporist alternative's institutional weakness — either by feeling compelled to adopt a framework they reject, or by absorbing externalized risk from a state project, or by being excluded from communal legitimacy for holding this view. Host states sit as agenda_setter because the entire model's viability is contingent on their tolerance, which is exercised, not owned, by diaspora communities themselves — a structural dependency this reading treats as a central weakness of its own preferred arrangement, not a strength.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification prevents this reading from being mistaken for either a thriving Rope (a fully functioning alternative coordination mechanism, which it is not, given documented institutional decline) or a pure Snare (there is no concentrated beneficiary extracting rents from its persistence — no single actor profits from diaspora institutions' atrophy; the decline is diffuse and largely a byproduct of resource and attention capture by a rival institutional complex). The founding problem (stateless minority survival without reproducing exclusionary nationalism) may still be live in principle, but the institutional vehicle built to solve it has weakened faute de mieux rather than through any single actor's plan, which is precisely the piton signature: administered inertia, no concentrated capturer, real diffuse cost to those who still need the alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diasporist_reading_as_kernel_committer,
    'Is the decline of diasporist Jewish institutions genuinely attributable to Zionist institutional capture of Jewish communal ''interest,'' or to independent historical forces (the Holocaust''s destruction of the Bundist heartland in Eastern Europe, postwar assimilation pressures in host states, the general 20th-century decline of internationalist socialist movements) that would have weakened diasporism regardless of Zionism''s rise?',
    'Comparative institutional history: track diasporist organizational funding, membership, and communal authority in cases with varying exposure to Zionist institutional competition (e.g., diaspora communities with weaker Zionist federations) against those with strong exposure, controlling for Holocaust destruction and assimilation independent of Zionism.',
    'If decline is substantially independent of Zionist institutional capture, this reading''s causal claim weakens and the classification may shift toward a Mountain-adjacent ''historically overtaken alternative'' rather than a Piton sustained by an identifiable rival''s institutional dominance. If capture is the dominant driver, the Piton/rival-framework reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diasporist_reading_as_kernel_committer, empirical, 'Whether diasporism''s institutional decline is caused by Zionist capture or independent historical attrition.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the five kernel readings disagree — is it about facts (what has historically happened to diaspora Jewish communities, what Israeli state conduct has caused), or about the normative weight assigned to sovereignty versus minority-rights protection as the correct vehicle for group survival?',
    'This is not resolvable by further data alone; it is the committer structure of the kernel itself. Documenting it here rather than inside the constraint''s classification logic per Rule 2.',
    'The five readings largely share overlapping factual premises (diaspora vulnerability is real, Zionist institutions are dominant, some Palestinians were displaced in 1948) but weight them into incompatible normative architectures for what counts as legitimate self-determination. This reading and liberal_nationalist_reading disagree less on facts than on whether statehood-via-sovereignty or statelessness-via-rights is the safer bet for a persecuted minority; this reading and settler_colonial_reading share skepticism of the state project but for different primary reasons (intra-Jewish harm here, Palestinian dispossession there).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'The kernel''s readings diverge primarily on normative architecture for group survival, not on a single disputed fact.').

omega_variable(
    host_state_tolerance_fragility,
    'Does the diasporist model''s dependence on host-state tolerance constitute a fatal structural weakness (given the historical record of that tolerance failing catastrophically, most notably 1933-1945) or a manageable risk comparable to any minority group''s dependence on rule-of-law protections?',
    'Comparative analysis of minority group survival outcomes under diaspora-pluralist versus sovereignty-based models across multiple stateless and state-holding minorities, weighted by base rates of host-state tolerance collapse.',
    'If host-state tolerance is judged structurally fragile and historically unreliable, this reading''s viability claim weakens considerably and its ε (currently authored moderate) might be revised upward, since the model''s operation would be shown to rest on an unreliable foundation rather than a durable one. If judged comparably reliable to other minority-rights arrangements, the moderate ε and piton classification stand as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(host_state_tolerance_fragility, preference, 'Whether diaspora pluralism''s dependence on host-state goodwill is a disqualifying structural fragility or an ordinary minority-rights risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1945, jewish_self_determination__diasporist_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(jewi_tr_t1960, jewish_self_determination__diasporist_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(jewi_tr_t1975, jewish_self_determination__diasporist_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(jewi_tr_t1990, jewish_self_determination__diasporist_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__diasporist_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(jewi_tr_t2015, jewish_self_determination__diasporist_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1945, jewish_self_determination__diasporist_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(jewi_be_t1960, jewish_self_determination__diasporist_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(jewi_be_t1975, jewish_self_determination__diasporist_reading, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(jewi_be_t1990, jewish_self_determination__diasporist_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__diasporist_reading, base_extractiveness, 2005, 0.37).
narrative_ontology:measurement(jewi_be_t2015, jewish_self_determination__diasporist_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__diasporist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling stories decomposing the natural-language label 'the debate over Jewish self-determination and Zionism' per the ε-invariance principle. Each reading (diasporist, liberal_nationalist, indigenous_return, settler_colonial, religious_covenant) instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and claimed type, because measuring the underlying kernel through each reading's own lights yields incompatible extraction profiles that cannot be averaged or reconciled into one story. All five are linked via affects_constraints to preserve the family relationship; none forecloses all others (see cs_structure.reading_relations for the specific typed edges from this reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
